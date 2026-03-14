{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server
  ( ServerEnv (..),
    withServer,
    setReloading,
    clearReloading,
    updateMessages,
    updateSession,
    rewriteRequestForGhci,
    rewriteResponseFromGhci,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Extra
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as BS
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Aeson.Micro as JSON
import Data.Aeson.Micro ((.=), (.:))
import qualified Data.Text as T
import Language.Haskell.Ghcid.Escape (unescape)
import Language.Haskell.Ghcid.Types
import Network.Socket
import qualified Network.Socket.ByteString as NBS
import Session (PathMode (..), Session, sessionCurrentDir, sessionExec, sessionPathMode)
import System.Directory
import System.FilePath
import System.Info (os)
import Crypto.Hash.SHA256
import Language.Haskell.Ghcid.Util
#if MIN_VERSION_base(4,14,0)
import System.IO.Error (isResourceVanishedError)
#endif
import System.IO.Unsafe (unsafePerformIO)

data ServerEnv = ServerEnv
  { seSession :: IORef Session,
    seLock :: Lock,
    seReloading :: Var Bool,
    seMessages :: MVar [Load],
    seClients :: Var [Socket]
  }

newServerEnv :: IO ServerEnv
newServerEnv = do
  sessionRef <- newIORef (error "Session not initialized")
  lock <- newLock
  reloading <- newVar False
  messages <- newEmptyMVar
  clients <- newVar []
  pure
    ServerEnv
      { seSession = sessionRef,
        seLock = lock,
        seReloading = reloading,
        seMessages = messages,
        seClients = clients
      }

updateSession :: ServerEnv -> Session -> IO ()
updateSession ServerEnv {..} = writeIORef seSession

setReloading :: ServerEnv -> IO ()
setReloading ServerEnv {..} = writeVar seReloading True

clearReloading :: ServerEnv -> IO ()
clearReloading ServerEnv {..} = writeVar seReloading False

updateMessages :: ServerEnv -> [Load] -> IO ()
updateMessages env@ServerEnv {..} msgs = do
  inserted <- tryPutMVar seMessages msgs
  unless inserted $ void $ swapMVar seMessages msgs
  broadcastDiagnostics env (renderDiagnostics msgs)

withServer :: (ServerEnv -> IO a) -> IO a
withServer act = bracket_
  (logDebug "withServer: start")
  (logDebug "withServer: stop")
  $ do
    env <- newServerEnv
    createDirectoryIfMissing True socketDir
    other <- canConnect serverSocketPath
    if other
      then do
        logInfo $ "withServer: Another ghcid server is already running at " ++ serverSocketPath
        act env
      else do
        removeIfExists serverSocketPath
        withListenServerSocket env $ \sock -> do
          logDebug $ "withServer: Socket server listening on " ++ serverSocketPath

          let serverLoop = forever $ do
                (conn, _) <- accept sock
                logDebug "withServer: Accepted socket connection"
                void $ async $ handle
                  (\(e :: SomeException) ->
                    if isExpectedDisconnect e
                      then logDebug $ "withServer: Client disconnected: " ++ show e
                      else logErr $ "withServer: Socket session crashed: " ++ show e)
                  (serveClient env conn `finally` (do
                    close conn
                    logDebug "withServer: Closed socket connection"
                    ))

          withAsync serverLoop $ \serverAsync -> do
            link serverAsync
            act env

{-# NOINLINE socketDir #-}
socketDir :: FilePath
socketDir = unsafePerformIO $ do
  -- Prefer /tmp over long $TMPDIR on mac to avoid socket length limit
  tmp <- (if os == "mingw32" then getTemporaryDirectory else pure "/tmp/ghcid")
  cwd <- getCurrentDirectory
  let digest = hash (BS.pack cwd)
  let shortHash = BS.unpack $ BS.take 8 $ B64URL.encodeUnpadded digest
  pure $ tmp </> shortHash

{-# NOINLINE serverSocketPath #-}
serverSocketPath :: FilePath
serverSocketPath = socketDir </> "server.sock"

withListenServerSocket :: ServerEnv -> (Socket -> IO a) -> IO a
withListenServerSocket env =
  bracket acquire release
  where
    acquire = do
      logDebug $ "Binding server socket at " ++ serverSocketPath
      s <- socket AF_UNIX Stream defaultProtocol
      bind s (SockAddrUnix serverSocketPath)
      listen s 8
      pure s

    release sock = do
      logDebug $ "Cleaning up socket at " ++ serverSocketPath
      clients <- readVar $ seClients env
      mapM_ close clients
      close sock
      removeIfExists serverSocketPath

removeIfExists :: FilePath -> IO ()
removeIfExists p = removeFile p `catch` (\(_ :: IOException) -> pure ())

canConnect :: FilePath -> IO Bool
canConnect path = do
  result <- try $ bracket (socketConnect path) close (const $ pure ())
  pure $ case result of
    Right () -> True
    Left (_ :: IOException) -> False

socketConnect :: FilePath -> IO Socket
socketConnect path = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix path)
  pure s

isExpectedDisconnect :: SomeException -> Bool
isExpectedDisconnect e =
  case fromException e :: Maybe IOException of
    Just ioe
      | isResourceVanishedErrorCompat ioe -> True
      | otherwise -> False
    Nothing -> False

isResourceVanishedErrorCompat :: IOError -> Bool
#if MIN_VERSION_base(4,14,0)
isResourceVanishedErrorCompat = isResourceVanishedError
#else
isResourceVanishedErrorCompat _ = False
#endif

serveClient :: ServerEnv -> Socket -> IO ()
serveClient env@ServerEnv {..} sock = do
  bracket_
    (modifyVar_ seClients $ pure . (sock :))
    (modifyVar_ seClients $ pure . filter (/= sock))
    $ do
        initial <- renderDiagnostics <$> readMVar seMessages
        sendLine sock "diag" initial
        loop BS.empty
  where
    loop pending = do
      (line, rest) <- recvLine sock pending
      case line of
         Nothing -> logDebug "Client disconnected"
         Just raw -> handleLine raw >> loop rest

    handleLine raw = case parseRequestLine raw of
      Left err -> sendLine sock "err" err
      Right cmd -> do
        logDebug $ "client->server " ++ BS.unpack raw
        result <- execGhci env cmd
        case result of
          Left err -> sendLine sock "err" err
          Right out -> sendLine sock "stdout" out

recvLine :: Socket -> BS.ByteString -> IO (Maybe BS.ByteString, BS.ByteString)
recvLine sock pending =
  case BS.break (== '\n') pending of
    (line, rest)
      | not (BS.null rest) -> pure (Just line, BS.drop 1 rest)
      | otherwise -> do
          chunk <- NBS.recv sock 4096
          if BS.null chunk
            then
              if BS.null pending
                then pure (Nothing, BS.empty)
                else pure (Just pending, BS.empty)
            else recvLine sock (pending <> chunk)

parseRequestLine :: BS.ByteString -> Either String String
parseRequestLine line = do
  let Nothing <?> a = Left a
      Just b <?> _ = Right b

  let parseRequest = JSON.withObject "Request" $ \obj -> do
        typ <- obj .: "type"
        payload <- obj .: "payload"
        if typ == "stdin"
          then pure (T.unpack payload)
          else fail $ "Unsupported message type: " ++ T.unpack typ

  value <- JSON.decodeStrict line <?> "Invalid JSON"

  JSON.parseMaybe parseRequest value <?> "Invalid request payload"

sendLine :: Socket -> T.Text -> String -> IO ()
sendLine sock tag payload = do
  let o = JSON.encodeStrict (JSON.object ["type" .= tag, "payload" .= T.pack payload]) <> "\n"
  logDebug $ "server->client " ++ BS.unpack o
  NBS.sendAll sock o


execGhci :: ServerEnv -> String -> IO (Either String String)
execGhci ServerEnv {..} cmd = do
  reloading <- readVar seReloading
  if reloading
    then do
      logDebug "Rejecting request because reload is in progress"
      pure $ Left "Reload in progress"
    else do
      result <- withLock seLock $ try $ do
        session <- readIORef seSession
        projectDir <- sessionCurrentDir session
        mode <- sessionPathMode session
        let rewrittenCmd = rewriteRequestForGhci mode projectDir cmd
        ls <- sessionExec session rewrittenCmd
        pure $ rewriteResponseFromGhci mode projectDir ls
      case result of
        Left (e :: SomeException) -> do
          logErr $ "sessionExec threw: " ++ show e
          pure $ Left (show e)
        Right ls -> pure $ Right (unlines ls)

rewriteRequestForGhci :: PathMode -> FilePath -> String -> String
rewriteRequestForGhci mode projectDir cmd =
  if mode == PathRelative
    then foldr rewrite cmd [":type-at", ":loc-at", ":uses"]
    else cmd
  where
    rewrite prefix acc =
      fromMaybe acc $ do
        rest <- stripPrefix (prefix ++ " " ++ addTrailingPathSeparator projectDir) acc
        pure $ prefix ++ " " ++ rest

rewriteResponseFromGhci :: PathMode -> FilePath -> [String] -> [String]
rewriteResponseFromGhci mode projectDir
  | mode == PathRelative = map (rewriteLocationLine projectDir)
  | otherwise = id

rewriteLocationLine :: FilePath -> String -> String
rewriteLocationLine projectDir line =
  if looksRelativeLocation line
    then projectDir </> line
    else line
  where
    looksRelativeLocation x =
      not (null x)
        && not (isAbsolute x)
        && not ("<" `isPrefixOf` x)
        && any (`isInfixOf` x) [".hs:", ".lhs:", ".hs-boot:"]

renderDiagnostics :: [Load] -> String
renderDiagnostics msgs =
  case [unescape line | Message {loadMessage = ls} <- msgs, line <- ls] of
    [] -> "All good (0 modules)\n"
    ls -> unlines ls

broadcastDiagnostics :: ServerEnv -> String -> IO ()
broadcastDiagnostics ServerEnv {..} payload = do
  clients <- readVar seClients
  forM_ clients $ \sock -> do
    result <- try $ sendLine sock "diag" payload
    case result of
      Left (e :: IOException) -> do
        logDebug $ "Failed to push diag to client: " ++ show e
      Right () -> pure ()

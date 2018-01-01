
-- | Test behavior of the executable, polling files for changes
module Test.Ghcid(ghcidTest) where

import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import System.Directory.Extra
import System.IO.Extra
import System.Time.Extra
import Data.Version.Extra
import System.Environment
import System.Process.Extra
import System.FilePath
import System.Exit

import Test.Tasty
import Test.Tasty.HUnit

import Ghcid
import Language.Haskell.Ghcid.Util
import Data.Functor
import Prelude


ghcidTest :: TestTree
ghcidTest = testGroup "Ghcid test"
    [basicTest
    ,dotGhciTest
    ,cabalTest
    ,stackTest
    ]


freshDir :: IO a -> IO a
freshDir act = withTempDir $ \tdir -> withCurrentDirectory tdir act

copyDir :: FilePath -> IO a -> IO ()
copyDir dir act = do
    b <- doesDirectoryExist dir
    if not b then putStrLn $ "Couldn't run test because test source is missing, " ++ dir else void $
        withTempDir $ \tdir -> do
            xs <- withCurrentDirectory dir $ listFilesRecursive "."
            forM_ xs $ \x -> do
                createDirectoryIfMissing True $ takeDirectory $ tdir </> x
                copyFile (dir </> x) (tdir </> x)
            withCurrentDirectory tdir act


whenStack :: IO a -> IO ()
whenStack act = do
    v <- findExecutable "stack"
    case v of
        Nothing -> putStrLn "Couldn't run test because stack is missing"
        Just _ -> void act


withGhcid :: [String] -> (([String] -> IO ()) -> IO a) -> IO a
withGhcid args script = do
    chan <- newChan
    let require want = do
            t <- timeout 30 $ readChan chan
            case t of
                Nothing -> fail $ "Require failed to produce results in time, expected: " ++ show want
                Just got -> assertApproxInfix want got
            sleep =<< getModTimeResolution

    let output = writeChan chan . filter (/= "") . map snd
    done <- newBarrier
    res <- bracket
        (flip forkFinally (const $ signalBarrier done ()) $
            withArgs (["--no-title","--no-status"]++args) $
                mainWithTerminal (return (100, 50)) output)
        killThread $ \_ -> script require
    waitBarrier done
    return res


-- | Since different versions of GHCi give different messages, we only try to find what
--   we require anywhere in the obtained messages, ignoring weird characters.
assertApproxInfix :: [String] -> [String] -> IO ()
assertApproxInfix want got = do
    -- Spacing and quotes tend to be different on different GHCi versions
    let simple = lower . filter (\x -> isLetter x || isDigit x || x == ':')
        got2 = simple $ unwords got
    all (`isInfixOf` got2) (map simple want) @?
        "Expected " ++ show want ++ ", got " ++ show got


write :: FilePath -> String -> IO ()
write file x = do
    print ("writeFile",file,x)
    writeFile file x

append :: FilePath -> String -> IO ()
append file x = do
    print ("appendFile",file,x)
    appendFile file x

rename :: FilePath -> FilePath -> IO ()
rename from to = do
    print ("renameFile",from,to)
    renameFile from to



---------------------------------------------------------------------
-- ACTUAL TEST SUITE

basicTest :: TestTree
basicTest = testCase "Ghcid basic" $ freshDir $ do
    write "Main.hs" "main = print 1"
    withGhcid ["-cghci -fwarn-unused-binds Main.hs"] $ \require -> do
        require [allGoodMessage]
        write "Main.hs" "x"
        require ["Main.hs:1:1"," Parse error:"]
        write "Util.hs" "module Util where"
        write "Main.hs" "import Util\nmain = print 1"
        require [allGoodMessage]
        write "Util.hs" "module Util where\nx"
        require ["Util.hs:2:1","Parse error:"]
        write "Util.hs" "module Util() where\nx = 1"
        require ["Util.hs:2:1","Warning:","Defined but not used: `x'"]

        -- check warnings persist properly
        write "Main.hs" "import Util\nx"
        require ["Main.hs:2:1","Parse error:"
                ,"Util.hs:2:1","Warning:","Defined but not used: `x'"]
        write "Main.hs" "import Util\nmain = print 2"
        require ["Util.hs:2:1","Warning:","Defined but not used: `x'"]
        write "Main.hs" "main = print 3"
        require [allGoodMessage]
        write "Main.hs" "import Util\nmain = print 4"
        require ["Util.hs:2:1","Warning:","Defined but not used: `x'"]
        write "Util.hs" "module Util where"
        require [allGoodMessage]

        -- check recursive modules work
        write "Util.hs" "module Util where\nimport Main"
        require ["imports form a cycle","Main.hs","Util.hs"]
        write "Util.hs" "module Util where"
        require [allGoodMessage]

        ghcVer <- readVersion <$> systemOutput_ "ghc --numeric-version"

        -- check renaming files works
        when (ghcVer < makeVersion [8]) $ do
            -- note that due to GHC bug #9648 and #11596 this doesn't work with newer GHC
            -- see https://ghc.haskell.org/trac/ghc/ticket/11596
            rename "Util.hs" "Util2.hs"
            require ["Main.hs:1:8:","Could not find module `Util'"]
            rename "Util2.hs" "Util.hs"
            require [allGoodMessage]

        -- after this point GHC bugs mean nothing really works too much


dotGhciTest :: TestTree
dotGhciTest = testCase "Ghcid .ghci" $ copyDir "test/foo" $ do
    write "test.txt" ""
    ignore $ void $ system "chmod go-w .ghci"
    withGhcid ["--test=:test"] $ \require -> do
        require [allGoodMessage]
        sleep 1 -- time to write out the test
        readFile "test.txt" >>= (@?= "X") -- the test writes out X
        append "Test.hs" "\n"
        require [allGoodMessage]
        sleep 1 -- time to write out the test
        readFile "test.txt" >>= (@?= "XX")
        print =<< readFile ".ghci"
        write ".ghci" ":set -fwarn-unused-imports\n:load Root Paths.hs Test"
        require ["The import of Paths_foo is redundant"]
        sleep 1 -- time to write out the test
        readFile "test.txt" >>= (@?= "XX") -- but shouldn't run on warning


cabalTest :: TestTree
cabalTest = testCase "Ghcid Cabal" $ copyDir "test/bar" $ do
    env <- getEnvironment
    let db = ["--package-db=" ++ x | x <- maybe [] splitSearchPath $ lookup "GHC_PACKAGE_PATH" env]
    (_, _, _, pid) <- createProcess $
         (proc "cabal" $ "configure":db){env = Just $ filter ((/=) "GHC_PACKAGE_PATH" . fst) env}
    ExitSuccess <- waitForProcess pid

    withGhcid [] $ \require -> do
        require [allGoodMessage]
        orig <- readFile' "src/Literate.lhs"
        append "src/Literate.lhs" "> x"
        require ["src/Literate.lhs:5:3","Parse error:"]
        write "src/Literate.lhs" orig
        require [allGoodMessage]

stackTest :: TestTree
stackTest = testCase "Ghcid Stack" $ copyDir "test/bar" $ whenStack $ do
    system_ "stack init"
    createDirectoryIfMissing True ".stack-work"

    withGhcid [] $ \require -> do
        require [allGoodMessage]
        append "src/Literate.lhs" "> x"
        require ["src/Literate.lhs:5:3","Parse error:"]
{-
    -- Stack seems to have changed, and continues to do so - lets just test the basics
    withGhcid ["src/Boot.hs"] $ \require -> do
        require [allGoodMessage]
        writeFile "src/Boot.hs" "X"
        require ["src/Boot.hs:1:1","Parse error:"]
-}

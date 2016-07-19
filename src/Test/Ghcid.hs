
-- | Test behavior of the executable, polling files for changes
module Test.Ghcid(ghcidTest) where

import Control.Concurrent
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

import Test.Tasty
import Test.Tasty.HUnit

import Ghcid
import Language.Haskell.Ghcid.Util
import Data.Functor
import Prelude


ghcidTest :: TestTree
ghcidTest = basicTest


freshDir :: IO a -> IO a
freshDir act = withTempDir $ \tdir -> withCurrentDirectory tdir act


withGhcid :: [String] -> (([String] -> IO ()) -> IO a) -> IO a
withGhcid args script = do
    chan <- newChan
    let require want = do
            t <- timeout 5 $ readChan chan
            case t of
                Nothing -> fail "require failed"
                Just got -> assertApproxInfix want got
            sleep =<< getModTimeResolution

    let output = writeChan chan . filter (/= "") . map snd
    done <- newBarrier
    res <- bracket
        (flip forkFinally (const $ signalBarrier done ()) $
            withArgs (["--notitle","--no-status"]++args) $
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
    all (`isInfixOf` got2) (map simple got) @?
        "Expected " ++ show want ++ ", got " ++ show got


write :: FilePath -> String -> IO ()
write file x = do
    print ("writeFile",file,x)
    writeFile file x

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
        require ["Main.hs:1:1"," Parse error: naked expression at top level"]
        write "Util.hs" "module Util where"
        write "Main.hs" "import Util\nmain = print 1"
        require [allGoodMessage]
        write "Util.hs" "module Util where\nx"
        require ["Util.hs:2:1","Parse error: naked expression at top level"]
        write "Util.hs" "module Util() where\nx = 1"
        require ["Util.hs:2:1","Warning: Defined but not used: `x'"]

        -- check warnings persist properly
        write "Main.hs" "import Util\nx"
        require ["Main.hs:2:1","Parse error: naked expression at top level"
                ,"Util.hs:2:1","Warning: Defined but not used: `x'"]
        write "Main.hs" "import Util\nmain = print 2"
        require ["Util.hs:2:1","Warning: Defined but not used: `x'"]
        write "Main.hs" "main = print 3"
        require [allGoodMessage]
        write "Main.hs" "import Util\nmain = print 4"
        require ["Util.hs:2:1","Warning: Defined but not used: `x'"]
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

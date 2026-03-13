-- | Test the high level library API
module Test.API(apiTests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath
import System.IO.Extra
import System.Time.Extra
import Language.Haskell.Ghcid
import Language.Haskell.Ghcid.Util
import Server
import Session (PathMode(..))
import Test.Common


apiTests :: TestTree
apiTests = testGroup "API test"
    [testCase "No files" $ withTempDir $ \dir -> do
        (ghci,load) <- startGhci "ghci -ignore-dot-ghci" (Just dir) $ const putStrLn
        load @?= []
        showModules ghci >>= (@?= [])
        exec ghci "import Data.List"
        exec ghci "nub \"test\"" >>= (@?= ["\"tes\""])
        stopGhci ghci

    ,disable19650 $ testCase "Load file" $ withTempDir $ \dir -> do
        writeFile (dir </> "File.hs") "module A where\na = 123"
        (ghci, load) <- startGhci "ghci -ignore-dot-ghci File.hs" (Just dir) $ const putStrLn
        load @?= [Loading "A" "File.hs"]
        exec ghci "a + 1" >>= (@?= ["124"])
        reload ghci >>= (@?= [])

        sleep =<< getModTimeResolution
        writeFile (dir </> "File.hs") "module A where\na = 456"
        exec ghci "a + 1" >>= (@?= ["124"])
        reload ghci >>= (@?= [Loading "A" "File.hs"])
        exec ghci "a + 1" >>= (@?= ["457"])
        stopGhci ghci

    ,testCase "Rewrite IDE paths" $ do
        let projectDir = "/project"
        rewriteRequestForGhci PathRelative projectDir ":type-at /project/app/Main.hs 5 34 5 38"
            @?= ":type-at app/Main.hs 5 34 5 38"
        rewriteRequestForGhci PathRelative projectDir ":type-at /other/Main.hs 5 34 5 38"
            @?= ":type-at /other/Main.hs 5 34 5 38"
        rewriteRequestForGhci PathRelative projectDir ":uses /project/dir with spaces/Main.hs 1 1 1 4"
            @?= ":uses dir with spaces/Main.hs 1 1 1 4"
        rewriteRequestForGhci PathAbsolute projectDir ":type-at /project/app/Main.hs 5 34 5 38"
            @?= ":type-at /project/app/Main.hs 5 34 5 38"
        rewriteResponseFromGhci PathRelative projectDir ["app/Main.hs:(5,34)-(5,38)", "app/Main.hs:5:34-38", "plain text"]
            @?= ["/project/app/Main.hs:(5,34)-(5,38)", "/project/app/Main.hs:5:34-38", "plain text"]
        rewriteResponseFromGhci PathAbsolute projectDir ["app/Main.hs:(5,34)-(5,38)"]
            @?= ["app/Main.hs:(5,34)-(5,38)"]
    ]

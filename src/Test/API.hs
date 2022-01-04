-- | Test the high level library API
module Test.API(apiTests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath
import System.IO.Extra
import System.Time.Extra
import Language.Haskell.Ghcid
import Language.Haskell.Ghcid.Util


apiTests :: TestTree
apiTests = testGroup "API test" $ take 1
    [testCase "No files" $ withTempDir $ \dir -> do
        (ghci,load) <- startGhci "ghci -ignore-dot-ghci" (Just dir) $ const putStrLn
        load @?= []
        showModules ghci >>= (@?= [])
        exec ghci "import Data.List"
        exec ghci "nub \"test\"" >>= (@?= ["\"tes\""])
        stopGhci ghci

    ,testCase "Load file" $ withTempDir $ \dir -> do
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
    ]

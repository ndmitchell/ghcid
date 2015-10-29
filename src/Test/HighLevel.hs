-- | Test the high level library API
module Test.HighLevel(highLevelTests) where

import System.Directory
import System.IO.Extra
import System.FilePath
import System.Environment
import Control.Exception
import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Ghcid


highLevelTests :: TestTree
highLevelTests = testGroup "High Level tests"
    [testStartRepl
    ,testShowModules
    ]

testStartRepl :: TestTree
testStartRepl = testCase "Start cabal repl" $
    withTestProject $ \root -> withCabal $ \flags -> do
        (ghci,load) <- startGhci (unwords $ "cabal repl":flags) (Just root) True
        stopGhci ghci
        load @?=  [ Loading "B.C" (normalise "src/B/C.hs")
                  , Loading "A" (normalise "src/A.hs")
                  ]

testShowModules :: TestTree
testShowModules = testCase "Show Modules" $
    withTestProject $ \root -> withCabal $ \flags -> do
        (ghci,_) <- startGhci (unwords $ "cabal repl":flags) (Just root) True
        mods <- showModules ghci
        stopGhci ghci
        mods @?= [("A",normalise "src/A.hs"),("B.C",normalise "src/B/C.hs")]


-- | Cabal chokes on GHC_PACKAGE_PATH from Stack, so I have to pass it on as --package-db
withCabal :: ([String] -> IO a) -> IO a
withCabal act = do
    path <- lookupEnv "GHC_PACKAGE_PATH"
    case path of
        Nothing -> act []
        Just path -> act ["--package-db=" ++ x | x <- splitSearchPath path]
                     `finally` setEnv "GHC_PACKAGE_PATH" path


testFiles :: [(FilePath, [String])]
testFiles =
    [(projectName <.> "cabal",
        ["name: " ++ projectName
        ,"version:0.1"
        ,"cabal-version:  >= 1.8"
        ,"build-type:     Simple"
        ,""
        ,"library"
        ,"  hs-source-dirs:  src"
        ,"  exposed-modules: A"
        ,"  other-modules:  B.C"
        ,"  build-depends:  base"
        ,""
        ,"executable BWTest"
        ,"  hs-source-dirs:  src"
        ,"  main-is:         Main.hs"
        ,"  other-modules:  B.D"
        ,"  build-depends:  base"
        ,"  ghc-options: -dynamic"
        ,""
        ,"test-suite BWTest-test"
        ,"  type:            exitcode-stdio-1.0"
        ,"  hs-source-dirs:  test"
        ,"  main-is:         Main.hs"
        ,"  other-modules:  TestA"
        ,"  build-depends:  base"])
    ,("Setup.hs",
        ["#!/usr/bin/env runhaskell"
        ,"import Distribution.Simple"
        ,"main :: IO ()"
        ,"main = defaultMain"])
    ,("src/A.hs",
        ["module A where"
        ,"fA = undefined"])
    ,("src/B/C.hs",
        ["module B.C where"
        ,"fC = undefined"])
    ,("src/B/D.hs",
        ["module B.D where"
        ,"fD = undefined"])
    ,("src/Main.hs",
        ["module Main where"
        ,"main = undefined"])
    ,("test/Main.hs",
        ["module Main where"
        ,"main = undefined"])
    ,("test/TestA.hs",
        ["module TestA where"
        ,"fTA = undefined"])
    ]

projectName = "BWTest"

withTestProject :: (FilePath -> IO a) -> IO a
withTestProject act = withTempDir $ \dir -> do
    forM_ testFiles $ \(name,contents) -> do
        createDirectoryIfMissing True $ takeDirectory $ dir </> name
        writeFile (dir </> name) $ unlines contents
    act dir

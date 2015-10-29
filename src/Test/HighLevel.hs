-- | Test the high level library API
module Test.HighLevel(highLevelTests) where

import System.Directory
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
testStartRepl = testCase "Start cabal repl" $ do
    root <- createTestProject
    print =<< getEnv "PATH"
    withCabal $ \flags -> do
        (ghci,load) <- startGhci (unwords $ "cabal repl":flags) (Just root) True
        stopGhci ghci
        load @?=  [ Loading "B.C" (normalise "src/B/C.hs")
                  , Loading "A" (normalise "src/A.hs")
                  ]

testShowModules :: TestTree
testShowModules = testCase "Show Modules" $ do
    root <- createTestProject
    withCabal $ \flags -> do
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


testProjectName = "BWTest"

testCabalContents = unlines
    ["name: "++testProjectName
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
    ,"  build-depends:  base"
    ]

testCabalFile :: FilePath -> FilePath
testCabalFile root = root </> takeDirectory root <.> ".cabal"

testAContents = unlines ["module A where","fA=undefined"]
testCContents = unlines ["module B.C where","fC=undefined"]
testDContents = unlines ["module B.D where","fD=undefined"]
testMainContents = unlines ["module Main where","main=undefined"]
testMainTestContents = unlines ["module Main where","main=undefined"]
testTestAContents = unlines ["module TestA where","fTA=undefined"]

testSetupContents = unlines
    ["#!/usr/bin/env runhaskell"
    ,"import Distribution.Simple"
    ,"main :: IO ()"
    ,"main = defaultMain"
    ]

createTestProject :: IO FilePath
createTestProject = do
    temp<-getTemporaryDirectory
    let root=temp </> testProjectName
    ex<-doesDirectoryExist root
    when ex (removeDirectoryRecursive root)
    createDirectory root
    writeFile (testCabalFile root) testCabalContents
    writeFile (root </> "Setup.hs") testSetupContents
    let srcF=root </> "src"
    createDirectory srcF
    writeFile (srcF </> "A.hs") testAContents
    let b=srcF </> "B"
    createDirectory b
    writeFile (b </> "C.hs") testCContents
    writeFile (b </> "D.hs") testDContents
    writeFile (srcF </> "Main.hs") testMainContents
    let testF=root </> "test"
    createDirectory testF
    writeFile (testF </> "Main.hs") testMainTestContents
    writeFile (testF </> "TestA.hs") testTestAContents
    return root

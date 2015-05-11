-- | Test the high level library API
module Test.HighLevel 
  (
    highLevelTests
  ) where

import System.Directory
import System.FilePath
import Control.Monad (when)

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Ghcid


highLevelTests :: TestTree
highLevelTests=testGroup "High Level tests"
  [ testStartRepl
  , testShowModules 
  ]

testStartRepl :: TestTree
testStartRepl = testCase "Start cabal repl" $
  do
    root <- createTestProject
    (ghci,load) <- startGhci "cabal repl" (Just root)
    stopGhci ghci
    load @?=  [ Loading "B.C" (normalise "src/B/C.hs")
              , Loading "A" (normalise "src/A.hs")
              ]

testShowModules :: TestTree
testShowModules = testCase "Show Modules" $
  do
    root <- createTestProject
    (ghci,_) <- startGhci "cabal repl" (Just root)
    mods <- showModules ghci
    stopGhci ghci
    mods @?= [("A",normalise "src/A.hs"),("B.C",normalise "src/B/C.hs")] 

testProjectName :: String
testProjectName="BWTest"   

testCabalContents :: String
testCabalContents = unlines ["name: "++testProjectName,
        "version:0.1",
        "cabal-version:  >= 1.8",
        "build-type:     Simple",
        "",
        "library",
        "  hs-source-dirs:  src",
        "  exposed-modules: A",
        "  other-modules:  B.C",
        "  build-depends:  base",
        "",
        "executable BWTest",
        "  hs-source-dirs:  src",
        "  main-is:         Main.hs",
        "  other-modules:  B.D",
        "  build-depends:  base",
        "  ghc-options: -dynamic",
        "",
        "test-suite BWTest-test",
        "  type:            exitcode-stdio-1.0",
        "  hs-source-dirs:  test",
        "  main-is:         Main.hs",
        "  other-modules:  TestA",
        "  build-depends:  base",
        ""
        ]        
     
testCabalFile :: FilePath -> FilePath
testCabalFile root =root </> (last (splitDirectories root) <.> ".cabal") 
     
testAContents :: String     
testAContents=unlines ["module A where","fA=undefined"]
testCContents :: String
testCContents=unlines ["module B.C where","fC=undefined"]     
testDContents :: String
testDContents=unlines ["module B.D where","fD=undefined"]     
testMainContents :: String
testMainContents=unlines ["module Main where","main=undefined"]   
testMainTestContents :: String
testMainTestContents=unlines ["module Main where","main=undefined"]   
testTestAContents :: String
testTestAContents=unlines ["module TestA where","fTA=undefined"]           
        
testSetupContents ::String
testSetupContents = unlines ["#!/usr/bin/env runhaskell",
        "import Distribution.Simple",
        "main :: IO ()",
        "main = defaultMain"]        
        
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

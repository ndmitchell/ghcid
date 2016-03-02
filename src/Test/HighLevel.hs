-- | Test the high level library API
module Test.HighLevel(highLevelTests) where

import System.Directory
import System.IO.Extra
import System.Exit
import System.FilePath
import System.Environment
import Control.Monad
import System.Process

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
    withTestProject $ \root -> do
        (ghci,_,load) <- startGhci "cabal repl" (Just root) True
        stopGhci ghci
        load @?=  [ Loading "B.C" (normalise "src/B/C.hs")
                  , Loading "A" (normalise "src/A.hs")
                  ]

testShowModules :: TestTree
testShowModules = testCase "Show Modules" $
    withTestProject $ \root -> do
        (ghci,_,_) <- startGhci "cabal repl" (Just root) True
        mods <- showModules ghci
        stopGhci ghci
        mods @?= [("A",normalise "src/A.hs"),("B.C",normalise "src/B/C.hs")]


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
    env <- getEnvironment
    let db = ["--package-db=" ++ x | x <- maybe [] splitSearchPath $ lookup "GHC_PACKAGE_PATH" env]
    (_, _, _, pid) <- createProcess $
        (proc "cabal" $ "configure":db){env = Just $ filter ((/=) "GHC_PACKAGE_PATH" . fst) env, cwd = Just dir}
    ExitSuccess <- waitForProcess pid
    act dir

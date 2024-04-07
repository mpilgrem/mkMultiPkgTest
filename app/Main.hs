{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad ( forM_ )
import Data.Maybe ( fromJust )
import Path ( Abs, Dir, Path, (</>), fromAbsFile,  parseRelDir, parseRelFile )
import Path.IO ( createDir, getCurrentDir )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn
      "No arguments. Two arguments expected: the project name and the number \
      \of packages."
    [p, n] -> do
      let projectRelDir = fromJust $ parseRelDir p
          n' = read n :: Int
      wd <- getCurrentDir
      let projectAbsDir = wd </> projectRelDir
      createDir projectAbsDir
      forM_ [1..n'] $ \i -> do
        let deps = case i of
              1 -> []
              _ -> [1..i - 1]
        mkPackage projectAbsDir i deps
    _ -> putStrLn
      "More than two arguments. Two arguments expected: the project name and \
      \the number of packages."

mkPackage :: Path Abs Dir -> Int -> [Int] -> IO ()
mkPackage projectDir n deps = do
  let n' = show n
      deps' = map show deps
      dependencies = concatMap mkDep deps'
      imports = concatMap mkImport deps'
      depFuncs = concatMap mkDepFunc deps'
      packageName = mkPackageName n'
      libName = "Lib" <> n'
      funcName = "someFunc" <> n'
      packageRelDir = fromJust $ parseRelDir packageName
      packageAbsDir = projectDir </> packageRelDir
      packageRelFile = fromJust $ parseRelFile "package.yaml"
      packageAbsFile = packageAbsDir </> packageRelFile
      srcRelDir = fromJust $ parseRelDir "src"
      srcAbsDir = packageAbsDir </> srcRelDir
      libRelFile = fromJust $ parseRelFile $ libName <> ".hs"
      libAbsFile = srcAbsDir </> libRelFile
  createDir packageAbsDir
  createDir srcAbsDir
  writeFile (fromAbsFile libAbsFile) $
       "module " <> libName <> "\n"
    <> "  ( " <> funcName <> "\n"
    <> "  ) where\n"
    <> "\n"
    <> imports
    <> "\n"
    <> funcName <> " :: IO ()\n"
    <> funcName <> " = do\n"
    <> "  putStrLn \"" <> funcName <> "\"\n"
    <> depFuncs
  writeFile (fromAbsFile packageAbsFile) $
       "name: " <> packageName <> "\n"
    <> "version: 0.1.0.0\n"
    <> "dependencies:\n"
    <> "- base >= 4.7 && < 5\n"
    <> dependencies
    <> "library:\n"
    <> "  source-dirs: src\n"

mkPackageName :: String -> String
mkPackageName n = "package" <> n

mkDep :: String -> String
mkDep n = "- " <> mkPackageName n <> "\n"

mkImport :: String -> String
mkImport n = "import Lib" <> n <> "\n"

mkDepFunc :: String -> String
mkDepFunc n = "  someFunc" <> n <> "\n"

module Main where

import Build_doctests (deps)
import Control.Applicative
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import Test.DocTest

opts :: [FilePath]
opts = 
    [   "-isrc"
    ,   "-idist/build/autogen"
    ,   "-optP-include"
    ,   "-optPdist/build/autogen/cabal_macros.h"
    ,   "-hide-all-packages"
    ] ++ (map ("-package=" ++) deps)

-- the list of all file paths to search for source files
sourceDirs :: [FilePath]
sourceDirs = ["src"]

main :: IO ()
main = getSources >>= \sources -> doctest $ opts ++ sources

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
    c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
    (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c

isSourceFile :: FilePath -> Bool
isSourceFile p = and [takeFileName p /= "Setup.hs", isSuffixOf ".hs" p]

getSources :: IO [FilePath]
getSources = liftM (filter isSourceFile . concat) (mapM go sourceDirs)
    where
        go dir = do
            (dirs, files) <- getFilesAndDirectories dir
            (files ++) . concat <$> mapM go dirs

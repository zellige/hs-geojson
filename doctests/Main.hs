module Main where

import Build_doctests (deps, opts)
import Control.Applicative
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import Test.DocTest

docopts :: [FilePath]
docopts =
    [   "-isrc"
    ,   "-idist/build/autogen"
    ,   "-optP-include"
    ,   "-optPdist/build/autogen/cabal_macros.h"
    ,   "-hide-all-packages"
    ] ++ map ("-package=" ++) deps ++ opts

-- the list of all file paths to search for source files
sourceDirs :: [FilePath]
sourceDirs = ["src"]

main :: IO ()
main = getSources >>= \sources -> doctest $ docopts ++ sources

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
    c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
    (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c

isSourceFile :: FilePath -> Bool
isSourceFile p = (takeFileName p /= "Setup.hs") && (".hs" `isSuffixOf` p)

getSources :: IO [FilePath]
getSources = liftM (filter isSourceFile . concat) (mapM go sourceDirs)
    where
        go dir = do
            (dirs, files) <- getFilesAndDirectories dir
            (files ++) . concat <$> mapM go dirs

{-# LANGUAGE CPP #-}
module Main where

import Build_doctests (deps, opts)
import Control.Monad

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.List
import System.Directory
import System.FilePath
import Test.DocTest

extensions :: [String]
extensions =
    [   "OverloadedStrings"
    ]

docopts :: [FilePath]
docopts =
    [   "-isrc"
    ,   "-idist/build/autogen"
    ,   "-optP-include"
    ,   "-optPdist/build/autogen/cabal_macros.h"
    ,   "-hide-all-packages"
    ] ++ map ("-package=" ++) deps ++ opts ++ map ("-X" ++) extensions

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

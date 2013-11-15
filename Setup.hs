#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List ( nub )
import Data.Version ( showVersion )
import Control.Applicative
import Distribution.Package ( PackageName(PackageName), PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose, findProgramVersion, currentDir )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Verbosity ( Verbosity )
import Distribution.Version ( Version )
import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    {   buildHook = \pkg lbi hooks flags -> do
            generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
            buildHook simpleUserHooks pkg lbi hooks flags
    }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
    let dir = autogenModulesDir lbi
    createDirectoryIfMissingVerbose verbosity True dir
    withLibLBI pkg lbi $ \_ libcfg ->
        withTestLBI pkg lbi $ \suite suitecfg -> do
            ghcOpts <-
                    generateCabalDevOpts
                <$> isCabalDevPresent
                <*> getGhcVersion verbosity
            rewriteFile (dir </> "Build_" ++ testName suite ++ ".hs") $ unlines
                [   "module Build_" ++ testName suite ++ " where"
                ,   "deps :: [String]"
                ,   "deps = " ++ show (formatdeps (testDeps libcfg suitecfg))
                ,   "opts :: [String]"
                ,   "opts = " ++ show ghcOpts
                ]
    where
        formatdeps = map (formatone . snd)
        formatone p = case packageName p of
            PackageName n -> n ++ "-" ++ showVersion (packageVersion p)

isCabalDevPresent :: IO Bool
isCabalDevPresent = do
    contents <- getDirectoryContents currentDir
    return $ "cabal-dev" `elem` contents

getGhcVersion :: Verbosity -> IO (Maybe Version)
getGhcVersion verb = findProgramVersion
    "--version"
    (last . words)
    verb
    "ghc"

generateCabalDevOpts :: Bool -> Maybe Version -> [String]
generateCabalDevOpts isCabalDev version =
    case version of
        Nothing -> []
        Just version' ->
            let
                baseOpts =  [   "-Lcabal-dev/lib"
                            ,   "-package-conf=cabal-dev/packages-" ++ showVersion version' ++ ".conf"
                            ]
            in
                if isCabalDev then baseOpts else []

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

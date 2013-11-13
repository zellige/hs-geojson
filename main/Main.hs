module Main where

import System.Console.GetOpt
import System.Environment

data CommandLineOption = Help | Version deriving (Show, Eq)

coreOptions :: [OptDescr CommandLineOption]
coreOptions = 
    [   Option "h?" ["help"]    (NoArg Help)    "print this usage message"
    ,   Option "V"  ["version"] (NoArg Version) "output the version"
    ]

argOrder :: ArgOrder a
argOrder = Permute

-- Define the additional options for your app here...
options :: [OptDescr CommandLineOption]
options = []

usageString :: String
usageString = "Usage: app [OPTIONS] args"

versionString :: String
versionString = "app: 0.0.1"

usageMsg :: String
usageMsg = usageInfo usageString (coreOptions ++ options)

-- | prints the usage message
printUsageMsg :: IO ()
printUsageMsg = putStrLn $ usageInfo usageString (coreOptions ++ options)

-- | checks the core flags of the app and if help and version dont appear passes control onto appMain where
-- | the user can do their own opt checks
checkCoreFlagsAndRunMain :: [CommandLineOption] -> [String] -> IO ()
checkCoreFlagsAndRunMain opts args
    | Help `elem` opts      = printUsageMsg
    | Version `elem` opts   = putStrLn versionString
    | otherwise             = appMain opts args

main :: IO ()
main = do 
    (opts, args, errorMsgs) <- getOpt argOrder (coreOptions ++ options) `fmap` getArgs
    if null errorMsgs then
        checkCoreFlagsAndRunMain opts args
    else
        ioError $ userError $ concat errorMsgs ++ usageMsg

-- Put the actual App code in here
appMain :: [a] -> [String] -> IO ()
appMain opts args = do
    putStrLn "Command Line Args: "
    mapM_ putStrLn args

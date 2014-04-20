module Main where

import Control.Monad ( unless )
import Language.Haskell.HLint ( hlint )
import System.Environment ( getArgs )
import System.Exit ( exitFailure )

main :: IO ()
main = do
    args <- getArgs
    hlints <- hlint $ ["src", "--cpp-define=HLINT", "--cpp-ansi"] ++ args
    unless (null hlints) exitFailure

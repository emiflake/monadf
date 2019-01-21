module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import System.Environment

import Parser
import Optimize
import Evaluation
import Generator

helpMessage = putStrLn . unlines $ [ "Please select one of the following options: "
                                   , "repl"
                                   , "          to go into a repl instance."
                                   , "eval <path>"
                                   , "          to evaluate a file."
                                   , "optimize <path>"
                                   , "          to optimize a file (and put in '.new.b' suffixed file)" ]

main :: IO ()
main = do 
    args <- getArgs
    case args of
        ("repl":_) -> replLoop startMachine
        ("eval":path) -> runFromFile $ unwords path
        ("optimize":path) -> passiveOptimize $ unwords path
        _ -> helpMessage
     {-runFromFile "test.bf"-}

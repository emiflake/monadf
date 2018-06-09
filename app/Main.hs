module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import System.Environment

import Parser
import Optimize
import Evaluation

helpMessage = putStrLn . unlines $ [ "Please select one of the following options: "
                                   , "repl"
                                   , "          to go into a repl instance."
                                   , "eval <path>"
                                   , "          to evaluate a file." ]

main :: IO ()
main = do 
    args <- getArgs
    case args of
        ("repl":_) -> replLoop startMachine
        ("eval":path) -> runFromFile $ unwords path
        _ -> helpMessage
     {-runFromFile "test.bf"-}

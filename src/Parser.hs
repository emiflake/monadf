module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Control.Monad

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

data Instruction = Loop [Instruction]
                 | Increment
                 | Decrement
                 | MoveLeft
                 | MoveRight
                 | Input
                 | Output
                 | Comment
                 deriving (Show, Eq)

charMatchParser :: Char -> a -> Parser a
charMatchParser ch val = char ch >> pure val

parseManyInstructions :: Parser [Instruction]
parseManyInstructions = many1 parseInstruction

parseInstruction :: Parser Instruction
parseInstruction = 
    choice [ parseLoop
           , parseIncrement
           , parseDecrement
           , parseRight
           , parseLeft
           , parseOutput
           , parseInput
           , parseComment ]

parseIncrement, parseDecrement, parseRight, parseLeft, parseOutput,
    parseInput, parseComment :: Parser Instruction
parseLoop = char '[' >> Loop <$> manyTill parseInstruction (char ']')
parseIncrement = charMatchParser '+' Increment
parseDecrement = charMatchParser '-' Decrement
parseRight     = charMatchParser '>' MoveRight
parseLeft      = charMatchParser '<' MoveLeft
parseOutput    = charMatchParser '.' Output
parseInput     = charMatchParser ',' Input
parseComment   = anyChar >> pure Comment

removeComments :: [Instruction] -> [Instruction]
removeComments (Loop insts:xs) = Loop (removeComments insts):removeComments xs
removeComments (Comment:xs) = removeComments xs
removeComments (x:xs) = x:removeComments xs
removeComments [] = []

parseBF :: String -> Either ParseError [Instruction]
parseBF s = removeComments <$> regularParse parseManyInstructions s



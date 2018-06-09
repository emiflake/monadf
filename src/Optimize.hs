module Optimize where

import Parser

data Optimization = Raw Instruction
                  | Many [Optimization]
                  | Set Integer
                  | Add Integer
                  | Sub Integer
                  | Move Integer
                  deriving Show

countFromStart t = length . takeWhile (==t)

optimize :: [Instruction] -> [Optimization]
optimize (Loop[Decrement]:xs) = Set 0 : optimize xs
optimize (Increment:xs) = Add (succ . fromIntegral . length . fst $ s) : optimize (snd s)
    where s = span (==Increment) xs
optimize (Decrement:xs) = Sub (succ . fromIntegral . length . fst $ s) : optimize (snd s)
    where s = span (==Decrement) xs
optimize (MoveLeft:xs) = Move (negate $ 1 + fromIntegral lefts) : optimize (drop lefts xs)
    where lefts = countFromStart MoveLeft xs
optimize (MoveRight:xs) = Move (1 + fromIntegral rights) : optimize (drop rights xs)
    where rights = countFromStart MoveRight xs
optimize (Loop inLoops:xs) = (Many $ optimize inLoops) : optimize xs
optimize (x:xs) = Raw x : optimize xs
optimize [] = []


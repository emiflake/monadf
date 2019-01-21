module Optimize where

import Parser

data Optimization = Raw Instruction
                  | Many [Optimization]
                  | Set Integer
                  | Add Integer
                  | Sub Integer
                  | Move Integer
                  deriving (Show, Eq)

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

optimize2' :: [Optimization] -> [Optimization]
optimize2' (Add a:Add b:xs) = Add (a + b) : optimize2' xs
optimize2' (Sub a:Add b:xs) = Add (-a + b) : optimize2' xs
optimize2' (Sub a:Sub b:xs) = Add (-a - b) : optimize2' xs
optimize2' (Add a:Sub b:xs) = Add (a - b) : optimize2' xs
optimize2' (Set a:Add b:xs) = Set (a + b) : optimize2' xs
optimize2' (Set a:Sub b:xs) = Set (a - b) : optimize2' xs
optimize2' (Move a:Move b:xs) = Move (a + b) : optimize2' xs
optimize2' (x:xs) = x : optimize2' xs
optimize2' [] = []

-- multipass non-destructive optimization
optimize2 :: [Optimization] -> [Optimization]
optimize2 xs | o2 == xs = xs
             | otherwise = optimize2 o2
    where o2 = optimize2' xs
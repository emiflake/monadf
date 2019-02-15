{-# LANGUAGE DeriveFunctor #-}
module DSL where

import Parser
import Optimize
import Generator
import Evaluation

import Control.Monad
import Data.Either
import Data.Char

newtype BFWriter a = BFWriter { runWriter :: (a, [Optimization]) } deriving (Functor, Show)

instance Applicative BFWriter where
    pure x = BFWriter (x, [])
    (<*>) (BFWriter (f, v)) (BFWriter (xs, w)) = BFWriter (f xs, v ++ w)

instance Monad BFWriter where
    return x = BFWriter (x, [])
    (BFWriter (a, v)) >>= f = let (BFWriter (b, w)) = f a in BFWriter (b, v ++ w)

write :: Optimization -> BFWriter ()
write x = BFWriter ((), [x])

move_left :: Int -> BFWriter ()
move_left n = write $ Move (fromIntegral (-n))

move_right :: Int -> BFWriter ()
move_right n = write $ Move (fromIntegral n)

set :: Int -> BFWriter ()
set n = write $ Set (fromIntegral n) 

loop :: BFWriter a -> BFWriter ()
loop (BFWriter (_, insts)) = write $ Many insts

increment :: Int -> BFWriter ()
increment n = write $ Add (fromIntegral n)

decrement :: Int -> BFWriter ()
decrement n = write $ Sub (fromIntegral n)

-- Nonoptimized char generation
saveChar :: Char -> BFWriter ()
saveChar char = write $ Set (fromIntegral $ ord char)

-- WARNING: creates array of chars in the tape, use carefully! (puts you at the end)
saveString :: String -> BFWriter ()
saveString (x:xs) = do
    saveChar x
    move_right 1
    saveString xs
saveString [] = pure ()

simply :: String -> BFWriter ()
simply s = case parseBF s of
    Left err -> error (show err)
    Right insts -> mapM_  write $ optimize2 . optimize $ insts 

prt :: BFWriter ()
prt = write $ Raw Output

putstr :: String -> BFWriter ()
putstr = (>> set 0) . mapM_ putc

putc :: Char -> BFWriter ()
putc char = do
    saveChar char
    prt

shifted :: Int -> BFWriter () -> BFWriter ()
shifted n f = do
    move_right n
    f
    move_left n

load :: [Int] -> BFWriter ()
load xs = do 
    load' xs
    move_left 1
    where load' (x:xs) = do
            set x
            move_right 1
            load xs
          load' [] = pure ()

-- overwrites current cell with user input
input :: BFWriter ()
input = write $ Raw Input

carry :: Int -> BFWriter ()
carry n = do
    loop $ do
        move_right n
        increment 1
        move_left n
        decrement 1
    move_right n

copy :: Int -> Int -> BFWriter ()
copy target intermediate = do
    loop $ do
        move_right target
        increment 1
        move_left (target-intermediate)
        increment 1
        move_left intermediate
        decrement 1
    move_right intermediate
    carry (-intermediate)

walk_left :: BFWriter ()
walk_left = loop (move_left 1)

foreach :: BFWriter () -> BFWriter ()
foreach f = loop $ do
    f
    move_right 1

runBF :: BFWriter () -> IO ()
runBF (BFWriter (_, insts)) = do
    finalMachine <- manyStepMachineOptimized insts startMachine
    putStrLn $ printArea 20 finalMachine

-- generateBF :: BFWriter () -> String
generateBF (BFWriter (_, insts)) = 
    generate . 
    optimize2 . optimize . fromRight [] . parseBF . 
    generate $ insts

-- Leave space after B
multiply :: Int -- A relloc
         -> Int -- B relloc
         -> Int -- Target 
         -> BFWriter ()
multiply a b target = do
    shifted a $
        loop $ do
            shifted (b - a) $ copy target 2
            decrement 1
    shifted b (set 0)
    move_right (target + 1)

printnum :: BFWriter ()
printnum = do
    simply "[>>+>+<<<-]>>>[<<<+>>>-]<<+>[<->[>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]++++++++[<++++++>-]>[<<+>>-]>[<<+>>-]<<]>]<[->>++++++++[<++++++>-]]<[.[-]<]<"
    putc '\n'
    set 0


ifElse' :: Int -> Int -> BFWriter () -> BFWriter () -> BFWriter ()
ifElse' pred offset a b = do
    move_right (offset + pred) -- Go to predicate, then go to offset
    increment 1 -- Make the else condition run by default
    move_left offset -- Go back to predicate
    
    loop $ do -- Will only run if cell at pred is nonzero
        set 0 -- clear cell
        shifted offset (set 0) -- set acc to zero (so it doesn't run the else case)
        a -- run then case
    move_right offset -- go to acc
    loop $ do
        set 0 -- set acc to zero as cleanup
        shifted offset b -- run the else case from the predicate

ifElse :: BFWriter () -> BFWriter () -> BFWriter ()
ifElse =
    ifElse' 0 1

test1 :: BFWriter ()
test1 = do
    putstr $ replicate 1000 'o'
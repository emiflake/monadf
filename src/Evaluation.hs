module Evaluation where

import Parser
import Optimize
import Control.Monad

type Cell = Integer

data StateMachine = Machine { left :: [Cell]
                            , current :: Cell
                            , right :: [Cell] } 
                            deriving Show

startMachine = Machine (repeat 0) 0 (repeat 0)

printArea :: Int -> StateMachine -> String
printArea size machine = 
    unwords $ map ($ machine) 
        [ unwords . map show . reverse . take size . left
        , ("["++) . (++"]") . show                 . current
        , unwords . map show           . take size . right ]

applyToCurrent :: (Integer -> Integer) -> StateMachine -> StateMachine
applyToCurrent f machine = machine{current=f . current $ machine}

moveTimes :: Integer -> StateMachine -> StateMachine
moveTimes 0 = id
moveTimes x | x > 0     = moveTimes (pred x) . moveRight
            | otherwise = moveTimes (succ x) . moveLeft

moveLeft, moveRight :: StateMachine -> StateMachine
moveLeft Machine{left=l, current=c, right=r} = 
    Machine{left=tail l, current=head l, right=c:r}
moveRight Machine{left=l, current=c, right=r} = 
    Machine{right=tail r, current=head r, left=c:l}

stepMachine :: Instruction -> StateMachine -> IO StateMachine
stepMachine Increment machine = pure $ applyToCurrent succ machine 
stepMachine Decrement machine = pure $ applyToCurrent pred machine
stepMachine MoveLeft machine  = pure $ moveLeft machine
stepMachine MoveRight machine = pure $ moveRight machine
stepMachine a@(Loop xs) mach | current mach <= 0 = pure mach
                             | otherwise         = newMachine >>= stepMachine a
    where newMachine = manyStepMachine xs mach
stepMachine Input machine = do
    x <- getChar
    pure $ machine{current=fromIntegral $ fromEnum x}
stepMachine Output machine = (putStr . pure . toEnum . fromInteger . current $ machine) >> pure machine
stepMachine Comment mach = pure mach

manyStepMachine :: [Instruction] -> StateMachine -> IO StateMachine
manyStepMachine xs machine = foldM (flip stepMachine) machine xs

stepMachineOptimized :: Optimization -> StateMachine -> IO StateMachine
stepMachineOptimized (Raw inst) mach = stepMachine inst mach
stepMachineOptimized a@(Many insts) mach | current mach <= 0 = pure mach
                                         | otherwise         = newMachine >>= stepMachineOptimized a
   where newMachine = manyStepMachineOptimized insts mach
stepMachineOptimized (Add n) mach = pure $ applyToCurrent (+n) mach
stepMachineOptimized (Sub n) mach = pure $ applyToCurrent (+(-n)) mach
stepMachineOptimized (Move n) mach = pure $ moveTimes n mach 
stepMachineOptimized (Set x) mach = pure mach{current=x}

manyStepMachineOptimized :: [Optimization] -> StateMachine -> IO StateMachine
manyStepMachineOptimized xs machine = foldM (flip stepMachineOptimized) machine xs

-- Go into a repl-like environment
replLoop :: StateMachine -> IO ()
replLoop st = do
    input <- getLine
    case parseBF input of
        Right insts -> do
            let optimized = optimize insts
            ns <- manyStepMachineOptimized optimized st
            putStrLn . printArea 20 $ ns
            replLoop ns
        Left err -> print err

-- Load, parse, optimize and run a file.
runFromFile f = readFile f >>= \i -> case parseBF i of
    Right is -> do 
        let optimized = optimize is
        ns <- manyStepMachineOptimized optimized startMachine
        putStrLn $ printArea 20 ns
    Left err -> print err


module Generator where

import Optimize
import Parser

-- Generate movement
movement :: Int -> String
movement n | n >= 0    = replicate n '>'
           | otherwise = replicate (-n) '<' 

-- non-assuming generator from Optimizations
generate :: [Optimization] -> String
generate (Raw i:xs) = toChar i : generate xs
generate (Many insts:xs) = "[" ++ generate insts ++ "]" ++ generate xs
generate (Set n:xs) = "[-]" ++ replicate (fromIntegral n) '+' ++ generate xs
generate (Add n:xs) = replicate (fromIntegral n) '+' ++ generate xs
generate (Sub n:xs) = replicate (fromIntegral n) '-' ++ generate xs
generate (Move n:xs) = movement (fromIntegral n) ++ generate xs
generate [] = []

passiveOptimize :: FilePath -> IO ()
passiveOptimize fp = do
    contents <- readFile fp
    case parseBF contents of
        Left err -> putStrLn $ "Parser error: " ++ show err 
        Right tokens -> do
            let generatedCode = generate . optimize2 . optimize $ tokens

            writeFile (fp ++ ".new.b") generatedCode
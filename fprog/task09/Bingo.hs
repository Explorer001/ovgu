import Data.List
import System.Random

-- Pads a single digit to two digit space.
pad :: Int -> String
pad n =
    if length (show n) == 2 then
        show n
    else
        show n ++ " "

-- Generates bingo field in string representation.
generateBingo :: IO String
generateBingo = do
    g <- newStdGen
    uniqs <- return (take 25 . nub $ (randomRs (0, 99) g :: [Int]))
    return $ fmt 0 uniqs where
        fmt :: Int -> [Int] -> String
        fmt n lst
            | n >= length lst = ""
            | n == 0  = "| " ++ (pad $ lst !! n) ++ " |" ++ fmt (n+1) lst
            | n == 12 = "free|" ++ fmt (n+1) lst
            | mod n 5 == 0 = "\n" ++ replicate 26 '-' ++ "\n| " ++ (pad $ lst !! n) ++ " |" ++ fmt (n+1) lst
            | otherwise = " " ++ (pad $ lst !! n) ++ " |" ++ fmt (n+1) lst

nBingos :: Int -> IO String
nBingos n =
    if n == 1 then do
        generateBingo
    else do
        b <- generateBingo
        c <- nBingos (n-1)
        return $ b ++ "\n\n\n\n" ++ c

bingoMaker :: IO ()
bingoMaker = do
    putStrLn "How many bingo cards should be generated?"
    n <- readLn
    bingos <- nBingos n
    writeFile "Bingos.txt" bingos

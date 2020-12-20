import System.Random

--
-- Rolls a dN.
--
roll :: Int -> IO Int
roll n = randomRIO (1, n)

--
-- Rolls m dN.
--
mroll :: Int -> Int -> IO ()
mroll n m =
    if m == 0 then do
        putStrLn "Done rolling."
    else do 
        ln <- roll n
        if m == 1 then
            putStrLn (show ln)
        else
            putStr ((show ln) ++ ",")
        mroll n (m-1)

--
-- Interactive roll.
--
rollLoop :: IO ()
rollLoop = do
    putStrLn "What kind of die should be rolled?(q for quit)"
    arg <- getLine
    if arg == "q" then
        putStrLn "Ended rolling dice."
    else do
        let n = read arg :: Int
        rolled <- roll $ n
        putStrLn ("Rolled: " ++ (show rolled))
        rollLoop

--
-- Interactive mroll.
--
mrollLoop :: IO ()
mrollLoop = do
    putStrLn "What kind of die should be rolled?(q for quit)"
    arg1 <- getLine
    if arg1 == "q" then
        putStrLn "Ended rolling dice."
    else do
        putStrLn "How many times should the die be rolled?"
        arg2 <- getLine
        let n = read arg1 :: Int
        let m = read arg2 :: Int
        mroll n m
        mrollLoop

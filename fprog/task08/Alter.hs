import System.IO
import System.Directory
import Data.Char

--
-- Gets index of first non whitespace.
-- If the string does not contain a non whitespace 0 will be returned.
--
firstNonWhitespace :: String -> Int
firstNonWhitespace str = fnw str 0 where
    fnw :: String -> Int -> Int
    fnw str n
        | null str = 0
        | isSpace (head str) = fnw (tail str) (n + 1)
        | otherwise = n

--
-- Gets index of first whitespace.
-- If the string does not contain whitespaces 0 will be returned.
--
firstWhitespace :: String -> Int
firstWhitespace str = fw str 0 where
    fw :: String -> Int -> Int
    fw str n
        | null str = 0
        | isSpace (head str) = n
        | otherwise = fw (tail str) (n + 1)

--
-- Splits string in following manor:
--  * if the string starts with a whitespace the string is split
--    in whitspace sequence and rest -> e.g "   asd asd" => "   ","asd asd".
--  * if the string starts with a non whitespace char, the string is
--    "tokenized" -> e.g "asd  asd" => "asd","  asd".
--
weirdSplit :: String -> (String, String)
weirdSplit str
    | null str = ("", str)
    | isSpace (head str) = let n = firstNonWhitespace str in
        if n == 0 then (str, "") else (take n str, drop n str)
    | otherwise = let n = firstWhitespace str in
        if n == 0 then (str, "") else (take n str, drop n str) 

--
-- Splits string in whitespace and non whitespace sequences
--
weirdTokenize :: String -> [String]
weirdTokenize str
    | null str = []
    | otherwise = let t = weirdSplit str in
        fst t : weirdTokenize (snd t)

alter :: IO ()
alter = do
    file <- getLine
    exists <- doesFileExist file
    if not exists then putStrLn "File doesn't exist!"
    else do
        handle <- openFile file ReadMode
        content <- hGetContents handle
        writeFile ("U." ++ file) (concat (map (\w -> if isSpace (head w) then w else (reverse w)) (weirdTokenize (map toUpper content))))
        hClose handle

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Map
import Data.Maybe

newtype Sudoku = Sudoku (Map (Int, Int) Int)

toS :: Maybe Int -> String
toS i = case i of
    Just i -> if i == 0 then " " else show i
    Nothing -> " " 

instance Show Sudoku where
    show s = "/---v---v---\\\n" ++
             row s 1 ++ row s 2 ++ row s 3 ++
             ">---+---+---<\n" ++
             row s 4 ++ row s 5 ++ row s 6 ++
             ">---+---+---<\n" ++
             row s 7 ++ row s 8 ++ row s 9 ++
             "\\---^---^---/\n"
        where
            row :: Sudoku -> Int -> String
            row (Sudoku s) i =
                "|" ++
                concat [toS (Data.Map.lookup (i,j) s) | j <- [1..3]] ++
                "|" ++
                concat [toS (Data.Map.lookup (i,j) s) | j <- [4..6]] ++
                "|" ++
                concat [toS (Data.Map.lookup (i,j) s) | j <- [7..9]] ++
                "|\n"

lineP :: Parser String
lineP = do
    char '|'
    cnt <- many $ noneOf "|"
    char '|'
    return cnt

linesP :: Parser [String]
linesP = many (do l <- lineP
                  endOfLine
                  return l)

sudokuP :: Parser Sudoku
sudokuP = do
    c <- linesP
    let cnct = concat c in do
        return $ Sudoku (fromList [((i+1,j+1), (f (cnct !! (i*8+j)))) | i <- [0..8], j <- [0..8]])
        where
            f :: Char -> Int
            f c = if c == ' ' then 0 else read [c]

main = do
    f <- readFile "sudoku.txt"
    print $ parse sudokuP "ERROR" f

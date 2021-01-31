import Text.ParserCombinators.Parsec
import Data.List.Split
import qualified Data.Text.IO as TextIO
import Text.Parsec.Char

data Implication = Implication [String] [String] deriving (Show)

implication :: Parser String
implication = many (noneOf " \n")

implicationParser :: Parser Implication
implicationParser = do
    p <- implication
    string " -> "
    q <- implication
    return $ Implication (splitOn "," p) (splitOn "," q)

implicationsP :: Parser [Implication]
implicationsP = many (do l <- implicationParser
                         endOfLine
                         return l)

main :: IO ()
main = do log <- readFile "implications.txt"
          print $ parse implicationsP "ERROR" log

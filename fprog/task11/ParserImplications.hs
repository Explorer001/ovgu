import Text.ParserCombinators.Parsec
import Data.List.Split
import qualified Data.Text.IO as TextIO

data Implication = Implication [String] [String] deriving (Show)

implication :: Parser String
implication = many (noneOf " \n")

implicationParser :: Parser Implication
implicationParser = do
    p <- implication
    string " -> "
    q <- implication
    return $ Implication (splitOn "," p) (splitOn "," q)

main :: IO ()
main = do log <- readFile "implications.txt"
          print $ parse implicationParser "ERROR" log

import Data.Attoparsec.Text
import qualified Data.Text as Text

data Expr = Var String
          | Num Double
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          deriving (Eq, Show)

exprP :: Parser Expr
exprP = choice
    [ opP <* skipSpace <*> exprP <* skipSpace <*> exprP
    , Num <$> double
    , Var <$> many' letter]
    where
        opP = choice
            [ char '+' *> pure Plus
            , char '-' *> pure Minus
            , char '*' *> pure Times
            , char '/' *> pure Div
            ]

main = print $ parseOnly exprP (Text.pack "/ + 4 5 - 7 2")

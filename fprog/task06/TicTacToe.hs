module TicTacToe
( newGame
, doTurn
, doTurns
) where

import Data.Map
import Data.Maybe

data GameState = Running | P1Won | P2Won | Draw | Illegal deriving (Enum, Eq)

--
-- Tokens to be placed in map.
-- Field can either be occupied by player 1, player 2 or be empty.
--
data Token = P1 | P2 | Empty deriving (Enum, Eq)

--
-- The Game context.
--
-- A game consists of:
--  * The game grid given by Map
--  * The current Game State given by Enumeration.
--  * A symbol for Player 1.
--  * A symbol for Player 2.
--  * The turn counter.
--
data Game = Game (Map String Token) GameState String String Int

--
-- Builds a empty TicTacToe grid.
--
emptyMap :: Map String Token
emptyMap = fromList [("tl", Empty), ("tm", Empty), ("tr", Empty),
                     ("ml", Empty), ("mm", Empty), ("mr", Empty),
                     ("bl", Empty), ("bm", Empty), ("br", Empty)]

--
-- Initializes new Game context.
--
-- Takes the two player symbols as input.
--
newGame :: String -> String -> Game
newGame s1 s2
    | Prelude.null s1 && Prelude.null s2 = Game emptyMap Running "x" "o" 0
    | Prelude.null s1 = Game emptyMap Running "x" (Prelude.take 1 s2) 0
    | Prelude.null s2 = Game emptyMap Running (Prelude.take 1 s1) "o" 0
    | otherwise = Game emptyMap Running (Prelude.take 1 s1)
                       (Prelude.take 1 s2) 0

--
-- Retrieves token at specified position.
--
tokenAt :: Map String Token -> String -> Token
tokenAt g s = fromJust (Data.Map.lookup s g)

--
-- Sets player token to grid according to turn.
--
setToken :: Map String Token -> String -> Int -> Map String Token
setToken g p t
    | mod t 2 == 0 = Data.Map.insert p P1 g
    | otherwise = Data.Map.insert p P2 g

--
-- Checks if given token has won the game
--
hasWon :: Map String Token -> Token -> Bool
hasWon g t
    | tokenAt g "tl" == t && tokenAt g "tm" == t && tokenAt g "tr" == t = True
    | tokenAt g "ml" == t && tokenAt g "mm" == t && tokenAt g "mr" == t = True
    | tokenAt g "bl" == t && tokenAt g "bm" == t && tokenAt g "br" == t = True
    | tokenAt g "tl" == t && tokenAt g "ml" == t && tokenAt g "bl" == t = True
    | tokenAt g "tm" == t && tokenAt g "mm" == t && tokenAt g "bm" == t = True
    | tokenAt g "tr" == t && tokenAt g "mr" == t && tokenAt g "br" == t = True
    | tokenAt g "tl" == t && tokenAt g "mm" == t && tokenAt g "br" == t = True
    | tokenAt g "bl" == t && tokenAt g "mm" == t && tokenAt g "tr" == t = True
    | otherwise = False
 

--
-- Checks if player won and returns corresponding status.
--
-- Returns P1Won, P2Won, Draw or Running.
--
setState :: Map String Token -> GameState
setState m
    | hasWon m P1 = P1Won
    | hasWon m P2 = P2Won
    | elem Empty m == False = Draw
    | otherwise = Running

--
-- Performs a single Move in game.
--
doTurn :: Game -> String -> Game
doTurn (Game g s s1 s2 t) p
    | s == P1Won = Game g s s1 s2 t
    | s == P2Won = Game g s s1 s2 t
    | s == Draw = Game g s s1 s2 t
    | member p g == False = Game g Illegal s1 s2 t
    | tokenAt g p /= Empty = Game g Illegal s1 s2 t
    | otherwise = let ng = setToken g p t in Game ng (setState ng) s1 s2 (t + 1) 

--
-- Performs list of Moves in game.
--
doTurns :: Game -> [String] -> Game
doTurns g ps = doTurns' g (reverse ps) where
    doTurns' :: Game -> [String] -> Game
    doTurns' g ps
        | Prelude.null ps = g
        | otherwise = doTurn (doTurns' g (tail ps)) (head ps)

--
-- Get string reperesentation of game state.
--
stateString :: GameState -> String
stateString s = case s of
    Running -> "Game is running."
    P1Won   -> "Player 1 won!"
    P2Won   -> "Player 2 won!"
    Draw    -> "Draw!"
    Illegal -> "Illegal move!"

--
-- Get string representation of token.
--
-- Takes token and both player symbols.
--
tS :: Token -> String -> String -> String
tS t s1 s2 = case t of
    P1    -> s1
    P2    -> s2
    Empty -> " "

--
-- Gets string of current map.
--
-- Takes grid and both player symbols.
--
gridS :: Map String Token -> String -> String -> String
gridS g s1 s2 = gT g "tl" s1 s2 ++ "|" ++ gT g "tm" s1 s2 ++ "|" ++ gT g "tr" s1 s2  ++ "\n" ++
                "-----\n" ++
                gT g "ml" s1 s2 ++ "|" ++ gT g "mm" s1 s2 ++ "|" ++ gT g "mr" s1 s2  ++ "\n" ++
                "-----\n" ++
                gT g "bl" s1 s2 ++ "|" ++ gT g "bm" s1 s2 ++ "|" ++ gT g "br" s1 s2
    where
    gT :: Map String Token -> String -> String -> String -> String
    gT m s s1 s2 = tS (fromJust (Data.Map.lookup s m)) s1 s2
--
-- Show instance for game context.
--
instance Show Game where
    show (Game g st s1 s2 t) = stateString st ++ "\n" ++ 
                               gridS g s1 s2 ++ "\n" ++
                               "Turn: " ++ show t ++ "\n"

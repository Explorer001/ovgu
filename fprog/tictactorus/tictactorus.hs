import Data.Map
import Data.Maybe
import Data.List

-- Possible game states:
--  * Running: No Player as won and there are still possible moves.
--  * PWon: Player indicated by Int parameter has won the game.
--  * Draw: Game is remis.
--  * Illegal: Some pisdez.
data GameState = Running | PWon Int | Draw | Illegal deriving (Eq, Show)

-- State to string.
state :: GameState -> String
state s = case s of
    Running -> "The game is running."
    PWon x -> "Player " ++ show (x+1) ++ " won!"
    Draw -> "Draw!"
    Illegal -> "Pisdez!"

-- Check if state is PWon.
stateWon :: GameState -> Bool
stateWon s = case s of
    PWon _ -> True
    _ -> False

-- Supported game modes.
--  * Torus: Basic torus TicTacToe.
--  * Inverted: Set as many tokens as possible without winning.
--  * Gravity: Torus like connect 4.
--
data GameMode = Torus | Inverted | Gravity

-- Mode to string.
mode :: GameMode -> String
mode m = case m of
    Torus -> "Torus"
    Inverted -> "Inverted"
    Gravity -> "Gravity"

-- Player tokens.
--  * T: Token of player Int.
--  * Empty: Field does not contain a token yet.
data Token = T Int | Empty deriving (Eq, Show)

-- Get token number.
tkn :: Token -> Int
tkn (T x) = x

-- Options contain following values:
--
-- + The grid size.
-- + The number of players
-- + The win condition.
-- + The game mode.
data Opts = Opts Int Int Int GameMode

-- Get grid size.
getGridSize :: Opts -> Int
getGridSize (Opts g _ _ _) = g

-- Set grid size.
setGridSize :: Int -> Opts -> Opts
setGridSize ng (Opts g n w gm) = Opts ng n w gm

-- Get number of players.
getNumPlayers :: Opts -> Int
getNumPlayers (Opts _ n _ _) = n

-- Set number of players.
setNumPlayers :: Int -> Opts -> Opts
setNumPlayers nn (Opts g n w gm) = Opts g nn w gm

-- Get the win condition.
getWinCon :: Opts -> Int
getWinCon (Opts _ _ w _) = w

-- Set the win condition.
setWinCon :: Int -> Opts -> Opts
setWinCon nw (Opts g n w gm) = Opts g n nw gm

-- Get the game mode.
getGameMode :: Opts -> GameMode
getGameMode (Opts _ _ _ gm) = gm

-- Set the game mode.
setGameMode :: GameMode -> Opts -> Opts
setGameMode ngm (Opts g n w gm) = Opts g n w ngm

-- The game context.
-- 
-- Parameters:
--  * Game torus given by map.
--  * Player token symbols given by map.
--  * The current state of the game.
--  * The game options.
--  * The turn counter.
data Game = Game (Map (Int, Int) Token) (Map Int Char) GameState Opts Int

-- Get the game grid.
getGrid :: Game -> Map (Int, Int) Token
getGrid (Game grid _ _ _ _) = grid

-- Set the game grid.
setGrid :: Map (Int, Int) Token -> Game -> Game
setGrid ngd (Game gd tk st op tn) = Game ngd tk st op tn

-- Get the token map.
getTokens :: Game -> Map Int Char
getTokens (Game _ t _ _ _) = t

-- Set the token map.
setTokens :: Map  Int Char -> Game -> Game
setTokens ntk (Game gd tk st op tn) = Game gd ntk st op tn

-- Get the game state.
getGameState :: Game -> GameState
getGameState (Game _ _ s _ _) = s

-- Set the game state.
setGameState :: GameState -> Game -> Game
setGameState nst (Game gd tk st op tn) = Game gd tk nst op tn

-- Get options.
getOpts :: Game -> Opts
getOpts (Game _ _ _ o _) = o

-- Set options.
setOpts :: Opts -> Game -> Game
setOpts nop (Game gd tk st op tn) = Game gd tk st nop tn

-- Get current turn.
getTurn :: Game -> Int
getTurn (Game _ _ _ _ t) = t

-- Set turn.
setTurn :: Int -> Game -> Game
setTurn ntn (Game gd tk st op tn) = Game gd tk st op ntn

-- Returns corresponding char for a token.
lookupToken :: Map Int Char -> Token -> Char
lookupToken m t = case t of
    Empty -> '.'
    T x -> fromJust (Data.Map.lookup x m)

-- Builds map string.
--
-- game: The game context.
mapString :: Game -> String
mapString game =
    let tkm = getTokens game
        grd = getGrid game 
        n = getGridSize (getOpts game) in
            intercalate "\n" ["|" ++ (Prelude.map (lookupToken tkm)  (getRow x n grd)) ++ "|" | x <- [0 .. n-1]]

instance Show Game where
    show game = (state (getGameState game)) ++ "\n" ++ mapString game

-- Retrieve row i of a n sized grid.
--
--  i: Row to retrieve.
--  n: Size of the grid.
--  gmap: The game grid.
getRow :: Int -> Int -> Map (Int, Int) Token -> [Token]
getRow i n gmap =
    getRow' i 0 n gmap where
        getRow' :: Int -> Int -> Int -> Map (Int, Int) Token -> [Token]
        getRow' i j n gmap
            | j >= n = []
            | i >= n = []
            | otherwise = fromJust (Data.Map.lookup (i, j) gmap) : getRow' i (j+1) n gmap

-- Retrieve column i of a n sized grid.
--
-- i: Column to retrieve.
-- n: Size of the grid.
-- gmap: The game grid.
getCol :: Int -> Int -> Map (Int, Int) Token -> [Token]
getCol i n gmap =
    getCol' i 0 n gmap where
        getCol' :: Int -> Int -> Int -> Map (Int, Int) Token -> [Token]
        getCol' i j n gmap
            | j >= n = []
            | i >= n = []
            | otherwise = fromJust (Data.Map.lookup (j, i) gmap) : getCol' i (j+1) n gmap

-- Retrieve rising diagonal i of a n sized grid (torus).
--
-- i: Diagonal to retrieve.
-- n: Size of the grid.
-- gmap: The game grid.
getDiagR :: Int -> Int -> Map (Int, Int) Token -> [Token]
getDiagR i n gmap = 
    getDiagR' i 0 n gmap where
        getDiagR' :: Int -> Int -> Int -> Map (Int, Int) Token -> [Token]
        getDiagR' i j n gmap
            | j >= n = []
            | i >= n = []
            | otherwise = fromJust (Data.Map.lookup (j, i) gmap) : getDiagR' (mod (i-1) n) (j+1) n gmap 

-- Retrieve falling diagonal i of a n sized grid (torus).
--
-- i: Diagonal to retrieve.
-- n: Size of the grid.
-- gmap: The game grid.
getDiagF :: Int -> Int -> Map (Int, Int) Token -> [Token]
getDiagF i n gmap =
    getDiagF' i 0 n gmap where
        getDiagF' :: Int -> Int -> Int -> Map (Int, Int) Token -> [Token]
        getDiagF' i j n gmap
            | j >= n = []
            | i >= n = []
            | otherwise = fromJust (Data.Map.lookup (j, i) gmap) : getDiagF' (mod (i+1) n) (j+1) n gmap

-- Retrieve all possible candidates.
--
-- game: The game context.
getCandidates :: Game -> [[Token]]
getCandidates game = 
    let gmap = getGrid game
        n = getGridSize (getOpts game) in
                       [getDiagF x n gmap | x <- [0 .. n-1]] ++
                       [getDiagR x n gmap | x <- [0 .. n-1]] ++
                       [getCol x n gmap | x <- [0 .. n-1]] ++
                       [getRow x n gmap | x <- [0 .. n-1]]

-- Check if a candidate contains a winning condition.
--
-- wcon: The win condition number.
-- cand: The candidate to check.
-- returns: Player token that won or Empty
checkCandidate :: Int -> [Token] -> Token
checkCandidate wcon cand = 
    checkCand' Empty 0 wcon cand where
        checkCand' :: Token -> Int -> Int -> [Token] -> Token
        checkCand' tkn i wcon cand
            | i >= wcon = tkn
            | Prelude.null cand = Empty
            | head cand == Empty = checkCand' Empty 0 wcon (tail cand)
            | head cand == tkn = checkCand' tkn (i+1) wcon (tail cand)
            | otherwise = checkCand' (head cand) 1 wcon (tail cand)

-- Checks if a player won the game.
--
-- game: The game context.
checkWin :: Game -> Token
checkWin game =
    checkWin' (getWinCon (getOpts game)) (getCandidates game) where
        checkWin' :: Int -> [[Token]] -> Token
        checkWin' wcon tokens
            | Prelude.null tokens = Empty
            | otherwise = let tkn = checkCandidate wcon (head tokens) in
                if tkn /= Empty then
                    tkn
                else
                    checkWin' wcon (tail tokens)

-- Get token at specific position.
--
-- i,j: The token position.
-- game: The game context.
tokenAt :: Int -> Int -> Game -> Maybe Token
tokenAt i j game = Data.Map.lookup (i,j) (getGrid game)

-- Set token to grid.
--
-- i,j: The token position.
-- game: The game context.
setToken :: Int -> Int -> Game -> Game
setToken i j game =
    let tkn = T (mod (getTurn game) (getNumPlayers (getOpts game))) in
        setGrid (Data.Map.insert (i, j) tkn (getGrid game)) game

-- Sets the game state for a Torus game.
--
-- game: The game context.
setStateTorus :: Game -> Game
setStateTorus game
    | elem Empty (getGrid game) == False = setGameState Draw game
    | otherwise = let winner = checkWin game in
        if winner /= Empty then
            setGameState (PWon (tkn winner)) game
        else
            setGameState Running game

-- Checks if the game is currently running.
--
-- game: The game context.
isRunning :: Game -> Bool
isRunning game = case (getGameState game) of
    Draw -> False
    PWon _ -> False
    _ -> True

-- Performs a turn in torus.
--
-- i,j: The token position.
-- game: The game context.
doTurnTorus :: Int -> Int -> Game -> Game
doTurnTorus i j game
    | stateWon (getGameState game) = game
    | getGameState game == Draw = game
    | isNothing (tokenAt i j game) = setGameState Illegal game
    | fromJust (tokenAt i j game) /= Empty = setGameState Illegal game
    | otherwise = setStateTorus (setTurn ((getTurn game) + 1) (setToken i j game))
 
-- The main game loop.
--
-- At the beginning no token is set, thus the game cannot be won.
-- The gameloop performs the following steps until the game ends:
--  1. Show game.
--  2. Token placement
--  3. Execute according turn function.
gameLoop :: Game -> IO ()
gameLoop game = do
    putStrLn $ show game
    player <- return $ (mod (getTurn game) (getNumPlayers (getOpts game))) + 1
    putStrLn $ "Enter i (Player " ++ show player ++ "):"
    i <- readLn
    putStrLn $ "Enter j (Player " ++ show player ++ "):"
    j <- readLn
    ngame <- return $ doTurnTorus i j game
    if isRunning ngame then
        gameLoop ngame
    else
        putStrLn $ show ngame   
 

-- Start torus game.
startTorus :: Game -> IO ()
startTorus game = gameLoop (setOpts (setGameMode Torus (getOpts game)) game)

displayHelp :: IO ()
displayHelp = do
    putStrLn "(h)elp     - Show help."
    putStrLn "(g)ridsize - Alter the grid size."
    putStrLn "(p)layers  - Alter the number of players."
    putStrLn "(t)oken    - Alter a players token representation."
    putStrLn "(w)incond  - Alter the win condition."
    putStrLn "(t)orus    - Start torus with current settings."
    putStrLn "(i)nvert   - Start invert with current settings."
    putStrLn "(g)ravity  - Start gravity with current settings."
    putStrLn "(q)uit     - Exit the game."
    putStrLn "(s)how     - Print current settings."

-- Changes the grid size.
--
-- game: The game context.
changeGrid :: Game -> IO Game
changeGrid game = do
    putStrLn "Enter new grid size (3-20):"
    size <- readLn
    if size < 3 || size > 20 then
        return game
    else do
        wopts <- return $ setGridSize size (setWinCon size (getOpts game))
        return $ setOpts wopts (setGrid (emptyGrid size) game)

-- Displays current settings.
displaySettings :: Game -> IO ()
displaySettings game = do
    putStrLn ("Number of players: " ++ (show (getNumPlayers (getOpts game))))
    putStrLn ("Grid size        : " ++ (show (getGridSize (getOpts game))))
    putStrLn ("Win condition    : " ++ (show (getWinCon (getOpts game))))
    putStrLn "Player tokens    :"

-- Checks if list contains given string.
pMatch :: String -> [String] -> Bool
pMatch needle haystack
    | Prelude.null haystack = False
    | needle == head haystack = True
    | otherwise = pMatch needle (tail haystack)

-- Creates a empty grid of size n x n.
emptyGrid :: Int -> Map (Int, Int) Token
emptyGrid n = fromList [((x,y), Empty) | x <- [0 .. n-1], y <- [0 .. n-1]]

-- Creates a T3 TicTacTorus
createT3 :: Game
createT3 = Game (emptyGrid 3) (fromList [(0, 'x'), (1, 'o')]) Running (Opts 3 2 3 Torus) 0

-- The main game loop.
mainLoop :: IO ()
mainLoop = do
    putStrLn "Welcome to TicTacTorus!"
    putStrLn "Enter \'help\' to display commands."
    putStrLn "Moves are provided by coordinates, i.e \'0 0\' is the field top left."
    putStrLn "Coordinate \'0 2\' is the third field from left in the top row."
    mainLoop' createT3 where
        mainLoop' :: Game -> IO ()
        mainLoop' game = do
            line <- getLine
            if pMatch line ["q", "quit"] then do
                putStrLn "Exiting..."
            else if pMatch line ["h", "help"] then do
                displayHelp
                mainLoop' game
            else if pMatch line ["s", "show"] then do
                displaySettings game
                mainLoop' game
            else if pMatch line ["t", "torus"] then do
                startTorus game
                mainLoop
            else if pMatch line ["g", "grid"] then do
                ng <- changeGrid game
                mainLoop' ng
            else do
                putStrLn ("Unknown command \'" ++ line ++ "\'")
                mainLoop' game

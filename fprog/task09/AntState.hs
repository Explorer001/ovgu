import Control.Monad.State.Lazy
import Data.Map

data Direction = North | East | South | West deriving (Show)
data Ant       = Ant (Int, Int) Direction deriving (Show)
data Field     = Field (Map (Int, Int) Bool) Ant deriving (Show)

getDst :: (Int, Int) -> Direction -> (Int, Int)
getDst (x, y) d = case d of
    North -> (x, y+1)
    East  -> (x+1, y)
    South -> (x, y-1)
    West  -> (x-1, y)

cw :: Direction -> Direction
cw d = case d of
    North -> East
    East  -> South
    South -> West
    West  -> North

ccw :: Direction -> Direction
ccw d = case d of
    North -> West
    East  -> North
    South -> East
    West  -> South

dToS :: Direction -> String
dToS d = case d of
    North -> "U"
    East  -> "R"
    South -> "D"
    West  -> "L"

alterM :: (Int, Int) -> Map (Int, Int) Bool -> Map (Int, Int) Bool
alterM f m = let val = Data.Map.lookup f m in
    case val of
        Nothing -> m
        Just x -> Data.Map.insert f (not x) m

walk :: Field -> (String, Field)
walk (Field m (Ant (x,y) d)) = let s = Data.Map.lookup (getDst (x,y) d) m
                                   dst = getDst (x,y) d 
                                   nmap = alterM (x,y) m in
    case s of
        Nothing -> ("Error!", Field m (Ant (x,y) d))
        Just v -> case v of
            True -> ("ccw " ++ dToS d ++ "-" ++ dToS (ccw d), Field nmap (Ant dst (ccw d)))
            False -> ("cw " ++ dToS d ++ "-" ++ dToS (cw d), Field nmap (Ant dst (cw d)))

step :: State Field String
step = state walk

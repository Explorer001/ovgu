import Control.Lens.Prism
import Control.Lens

data V = V Int | V1 String | V2 Char deriving Show

x = [(V 2), (V1 "sa"), (V2 'c'), (V 42), (V1 "si")]

vString :: Prism' V String
vString = prism V1 $ \v ->
    case v of
        V1 a -> Right a
        _ -> Left v

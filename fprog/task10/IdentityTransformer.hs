import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Class

newtype RightIDT m a = RightIDT {runRightIDT :: m (Identity a)}

instance Monad m => Applicative (RightIDT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (RightIDT m) where
    fmap = liftM

instance Monad m => Monad (RightIDT m) where
    return = pure
    x >>= f = RightIDT $ do
        Identity v <- runRightIDT x
        runRightIDT (f v)

instance MonadTrans RightIDT where
    lift = RightIDT . (liftM Identity)

newtype LeftIDT m a = LeftIDT {runLeftIDT :: Identity (m a)}

instance Monad m => Applicative (LeftIDT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (LeftIDT m) where
    fmap = liftM

instance Monad m => Monad (LeftIDT m) where
    return = pure
    x >>= f = LeftIDT $ do
        a <- runLeftIDT x
        -- not possible? a is of type m a and it is not possible to extract a
        -- from a arbitrary monad. 

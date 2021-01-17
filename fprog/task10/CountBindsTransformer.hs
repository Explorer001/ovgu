import Control.Monad
import Control.Monad.Trans.Class

data CountBinds a = CountBinds (Integer, a)
binds (CountBinds (a,_)) = a

instance Functor CountBinds where
    fmap f (CountBinds(b,a)) = CountBinds (b, f a)

instance Applicative CountBinds where
    pure a = CountBinds (0,a)
    (<*>) (CountBinds (a,f)) (CountBinds (b,c)) = CountBinds (b,f c)

instance Monad CountBinds where
    return = pure
    (>>=) (CountBinds (a,b)) f =
        (\(CountBinds(a',b')) -> CountBinds (a'+1,b')) (f b)

newtype CountBindsT m a = CountBindsT {runCountBindsT :: m (CountBinds a)}

instance Monad m => Applicative (CountBindsT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (CountBindsT m) where
    fmap = liftM

instance Monad m => Monad (CountBindsT m) where
    return = pure
    x >>= f = CountBindsT $ do
        CountBinds (a,b) <- runCountBindsT x
        CountBinds (_,b') <- runCountBindsT (f b)
        return $ CountBinds (a,b')

instance MonadTrans CountBindsT where
    lift m = CountBindsT $ do
        a <- m
        return (CountBinds (0, a))

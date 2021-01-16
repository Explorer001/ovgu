import Data.Either
import Control.Monad.Trans.Class
import Control.Monad

newtype EitherT m a = EitherT {runEitherT :: m (Either String a)}

instance Monad m => Applicative (EitherT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (EitherT m) where
    fmap = liftM

instance Monad m => Monad (EitherT m) where
    return dat = EitherT (return $ Right dat)
    x >>= f = EitherT $ do
        v <- runEitherT x
        case v of
            Left err -> return $ Left err
            Right d  -> runEitherT (f d)

instance MonadTrans EitherT where
    lift = EitherT . (liftM Right)

> {-# LANGUAGE InstanceSigs #-}
> import Control.Monad (ap, liftM, MonadPlus(..), guard, msum)
> import Control.Applicative (Applicative(..))
> import Control.Monad.Trans

Monad Transformers worksheet

> getPassword :: IO (Maybe String)
> getPassword = do
>   pass <- getLine
>   return $ if isValidPass pass then Just pass else Nothing

> isValidPass pass = 3 < n && n <= 8
>   where
>     n = length pass

> askPassphrase :: IO ()
> askPassphrase = do
>   putStrLn "Insert your password: "
>   mpass <- getPassword
>   case mpass of
>     Just pass -> putStrLn "Storing password in database"
>     Nothing -> putStrLn "You cannot proceed! Invalid password..."

The above code can be simplified using Monad transformers. We define a
trasformer for Maybe time which will wrap a Maybe value inside another monad m.

Here m is any type of Monad and a is the type of the Maybe we are dealing with.
If we take the above example m is IO monad and a is the String type (for passwords)

> newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

This monad transformer does satisfy monadic laws, thus can be an instance of the
Monad class.

> instance Monad m => Monad (MaybeT m) where
>   return = MaybeT . return . Just
>
>   (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
>   x >>= f = MaybeT $ do val <- runMaybeT x
>                         case val of
>                           Nothing -> return Nothing
>                           Just v -> runMaybeT $ f v

The return definition can be written as follows
return = MaybeT . return . return.
Here the first return is to put the value (of a generic type) in the default
Maybe context which is just Just! So they are the same definitions...

Following are the trivial definitions of Functor and Applicative instances
using monadic primitives. It will be mandatory to define Applicative instances
for all Monads from GHC 7.10.

> instance Monad m => Functor (MaybeT m) where
>   fmap = liftM

> instance Monad m => Applicative (MaybeT m) where
>   pure = return
>   (<*>) = ap

> instance Monad m => MonadPlus (MaybeT m) where
>   mzero = MaybeT $ return Nothing
>   mplus x y = MaybeT $ do mval <- runMaybeT x
>                           case mval of
>                             Nothing -> runMaybeT y
>                             Just _ -> return mval

> instance MonadTrans MaybeT where
>   lift = MaybeT . (liftM Just)

Application to the passphrase example

> getValidPassword :: MaybeT IO String
> getValidPassword = do s <- lift getLine
>                       guard (isValidPass s)
>                       return s

> askPassphrase' :: MaybeT IO ()
> askPassphrase' = do lift $ putStrLn "Insert your new password:"
>                     value <- getValidPassword
>                     lift $ putStrLn "You are good to go..."

> askPassphraseNagging :: MaybeT IO ()
> askPassphraseNagging = do lift $ putStrLn "Insert your new password:"
>                           value <- msum $ repeat getValidPassword
>                           lift $ putStrLn "You are good to go..."

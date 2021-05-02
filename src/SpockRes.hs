{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SpockRes where

import Utils
import Web.Spock
import Control.Monad.IO.Class
import Control.Monad.Fail
import Control.Monad.Trans.Class
import System.Console.ANSI

-- Utils --
somethingWrong = "Something went wrong!"

type Res = SpockResT (WebStateM () () ()) [Char]

class PrettyPrintable a where
  prettyPrint :: a -> String

instance PrettyPrintable String where
   prettyPrint = id

-- Let's keep this commented away till I can
-- figure out overloaded instances properly
-- instance (Show a) => PrettyPrintable a where
--     prettyPrint = show

getEitherBody :: Show e => MonadIO m => Either e a -> SpockResT m a
getEitherBody ev = do
    case ev of
        Left err -> do
            liftIO $ setSGR [SetColor Foreground Vivid Red]
            liftIO $ putStrLn $ "ERR> " <> show err
            liftIO $ setSGR [SetColor Foreground Vivid Yellow]
            fail somethingWrong
        Right val -> pure val

getMaybeBody :: Show e => MonadIO m => Maybe e -> SpockResT m e
getMaybeBody ev = do
    case ev of
        Nothing -> do
            liftIO $ setSGR [SetColor Foreground Vivid Red]
            liftIO $ putStrLn "ERR> getMaybeBody got Nothing"
            liftIO $ setSGR [SetColor Foreground Vivid Yellow]
            fail somethingWrong
        Just val -> pure val

runBody :: MonadIO m => PrettyPrintable a => SpockResT m a -> ActionT m ()
runBody sr = do
    res <- lift $ runSpockResT sr
    case res of
        Failure msg -> do
            liftIO $ setSGR [SetColor Foreground Vivid Red]
            liftIO $ putStrLn $ "ERR> " <> msg
            liftIO $ setSGR [SetColor Foreground Vivid Yellow]
            json $ getResponse False msg
        Success msg -> json $ getResponse True $ prettyPrint msg

runBodyRes :: MonadIO m => PrettyPrintable a => SpockRes a -> ActionT m ()
runBodyRes res = do
    case res of
        Failure msg -> do
            liftIO $ setSGR [SetColor Foreground Vivid Red]
            liftIO $ putStrLn $ "ERR> " <> msg
            liftIO $ setSGR [SetColor Foreground Vivid Yellow]
            json $ getResponse False msg
        Success msg -> json $ getResponse True $ prettyPrint msg

runBodyRes' :: MonadIO m => SpockRes a -> ActionT m ()
runBodyRes' res = do
    case res of
        Failure msg -> do
            liftIO $ setSGR [SetColor Foreground Vivid Red]
            liftIO $ putStrLn $ "ERR> " <> msg
            liftIO $ setSGR [SetColor Foreground Vivid Yellow]
            json $ getResponse False msg
        Success msg -> json $ getResponse True "Operation succeeded"

-- Monad Transformer --
newtype SpockResT m a = SpockResT { runSpockResT :: m (SpockRes a) }
-- runSpockResT :: SpockResT m a -> m (SpockRes a)

instance (Monad m) => Functor (SpockResT m) where
    fmap = fmap

instance (Monad m) => Applicative (SpockResT m) where
    pure x = SpockResT $ pure $ pure x
    (<*>) = (<*>)

instance (Monad m) => Monad (SpockResT m) where
    return = SpockResT . pure . pure

    x >>= f = SpockResT $ do
        res <- runSpockResT x
        case res of
            Failure msg -> pure $ Failure msg
            Success msg -> runSpockResT $ f msg

instance MonadTrans SpockResT where
    lift = SpockResT . fmap Success

instance (Monad m) => MonadFail (SpockResT m) where
    fail = SpockResT . pure . fail

instance (MonadIO m) => MonadIO (SpockResT m) where
    liftIO = lift . liftIO

-- Monad --
data SpockRes a =
    -- route succeeded
    Success a
    --  route failed
    | Failure String
    deriving Show

instance Functor SpockRes where
    fmap f (Success s) = Success $ f s
    fmap _ (Failure e) = Failure e

instance Applicative SpockRes where
    pure = Success
    Success f <*> Success v = Success $ f v
    _ <*> Failure f = Failure f

instance Monad SpockRes where
    return = Success
    Success s >>= f = f s
    Failure fl >>= _ = Failure fl

instance MonadFail SpockRes where
    fail = Failure
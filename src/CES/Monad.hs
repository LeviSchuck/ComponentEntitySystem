{-# LANGUAGE LambdaCase #-}
module CES.Monad
    ( CEST(..)
    , newEntity
    , addComponent
    , remComponent
    , onEntities
    , evalCEST
    -- Reexport lift
    , TC.lift
    )
where

import Control.Applicative
import Control.Monad
import Data.Foldable
import qualified Control.Monad.Trans.Class as TC

import qualified Data.Map as M
import qualified Data.Set as S

import CES.Data

newtype CEST c m v = CEST
    { runCEST :: CESContext c -> m (v, CESContext c)
    }

instance (Functor m) => Functor (CEST c m) where
    fmap f m = CEST $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runCEST m s

instance (Functor m, Monad m) => Applicative (CEST c m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (CEST c m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (CEST c m) where
    return a = CEST $ \s -> return (a, s)
    m >>= k  = CEST $ \s -> do
        ~(a, s') <- runCEST m s
        runCEST (k a) s'
    fail str = CEST $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (CEST c m) where
    mzero       = CEST $ \_ -> mzero
    m `mplus` n = CEST $ \s -> runCEST m s `mplus` runCEST n s

instance TC.MonadTrans (CEST c) where
    lift m = CEST $ \s -> do
        a <- m
        return (a, s)

ces :: (Monad m)
      => (CESContext c -> (a, CESContext c)) 
      -> CEST c m a
ces f = CEST (return . f)


newEntity :: (Monad m) => CEST c m ComponentID
newEntity = ces (\c -> (cesNextID c, c {cesNextID = cesNextID c + 1}))
{-# INLINE newEntity #-}

addComponent :: (Monad m, Ord c) => c -> ComponentID -> CEST c m ()
addComponent comp entity = ces (\c -> ((), c
    { entities = M.alter (\case
        Nothing -> Just (S.singleton entity)
        Just s -> Just (S.insert entity s)
        ) comp (entities c)
    }))
{-# INLINE addComponent #-}

remComponent :: (Monad m, Ord c) => c -> ComponentID -> CEST c m ()
remComponent comp entity = ces (\c -> ((), c 
    { entities = M.alter (\case
        Nothing -> Nothing
        Just s -> Just (S.delete entity s)
        ) comp (entities c)
    }))
{-# INLINE remComponent #-}

onEntities :: (Monad m, Ord c) => c -> (ComponentID -> CEST c m ()) -> CEST c m ()
onEntities comp on = CEST (\c -> do
    case M.lookup comp (entities c) of
        Nothing -> return ((),c)
        Just s -> do
            (_, c') <- runCEST (forM_ s on) c
            return ((), c')
    )
{-# INLINE onEntities #-}

evalCEST :: (Monad m, Ord c) => CEST c m a -> CESContext c -> m a
evalCEST m c = do
    ~(a, _) <- runCEST m c
    return a
{-# INLINE evalCEST #-}

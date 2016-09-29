{-# LANGUAGE LambdaCase #-}
module CES.Monad
    ( CEST(..)
    , newEntity
    , addComponent
    , remComponent
    , onEntities
    , foldEntities
    , onEntity
    , onEntity_
    , runCEST
    , execCEST
    , evalCEST
    , ces
    , cesM
    -- Reexport lift
    , TC.lift
    )
where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Unsafe.Coerce

import qualified Control.Monad.Trans.Class as TC
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import CES.Data
import CES.Data.Internal

newtype CEST m v = CEST
    { runCEST' :: CESContext -> m (CESRet v, CESContext)
    }

instance (Functor m) => Functor (CEST m) where
    fmap f m = CEST $ \ s ->
        fmap (\ ~(a, s') -> (fmap f a, s')) $ runCEST' m s
    {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (CEST m) where
    pure x = CEST $ \s -> (return (NormalRet x, s))
    {-# INLINE pure #-}
    m <*> n = CEST $ \s -> runCEST' m s >>= \case
        (VoidRet, s)      -> return (VoidRet, s)
        (FailureRet x, s) -> return (FailureRet x, s)
        (NormalRet f, s)  -> runCEST' n s >>= \case
            (VoidRet, s')      -> return (VoidRet, s') 
            (FailureRet x, s') -> return (FailureRet x, s')
            (NormalRet v, s')  -> return (NormalRet (f v), s')
    {-# INLINE (<*>) #-}

instance (Monad m) => Monad (CEST m) where
    fail str = CEST $ \s -> return (FailureRet str, s)
    {-# INLINE fail #-}
    return a = CEST $ \s -> return (NormalRet a, s)
    {-# INLINE return #-}
    m >>= k  = CEST $ \s -> do
        ~(a, s') <- runCEST' m s
        case a of
            VoidRet      -> return (VoidRet, s)
            FailureRet r -> return (FailureRet r, s)
            NormalRet v  -> runCEST' (k v) s'
    {-# INLINE (>>=) #-}
    
instance (Functor m, Monad m) => Alternative (CEST m) where
    empty = CEST $ \s -> return (VoidRet, s)
    {-# INLINE empty #-}
    m <|> n = CEST $ \s -> runCEST' m s >>= \case
        (VoidRet, s')     -> runCEST' n s'
        (FailureRet _, _) -> runCEST' n s
        (NormalRet v, s') -> return (NormalRet v, s')
    {-# INLINE (<|>) #-}

instance (MonadPlus m) => MonadPlus (CEST m) where
    mzero       = CEST $ \s -> return (VoidRet, s)
    {-# INLINE mzero #-}
    m `mplus` n = CEST $ \s -> do
        ~(a, s') <- runCEST' n s
        case a of
            VoidRet -> runCEST' n s'
            FailureRet _ -> runCEST' n s'
            NormalRet r -> return (NormalRet r, s')
    {-# INLINE mplus #-}

instance TC.MonadTrans CEST where
    lift m = CEST $ \s -> do
        a <- m
        return (NormalRet a, s)
    {-# INLINE lift #-}

ces :: (Monad m)
      => (CESContext -> (CESReturn a, CESContext)) 
      -> CEST m a
ces f = CEST $ \s -> case f s of
    (Left Nothing, _)    -> return (VoidRet, s)
    (Left (Just str), _) -> return (FailureRet str, s)
    (Right a, s)         -> return (NormalRet a, s)
{-# INLINE ces #-}

cesM :: (Monad m)
      => (CESContext -> m (CESReturn a, CESContext)) 
      -> CEST m a
cesM f = CEST $ \s -> f s >>= \case
    (Left Nothing, _)    -> return (VoidRet, s)
    (Left (Just str), _) -> return (FailureRet str, s)
    (Right a, s)         -> return (NormalRet a, s)
{-# INLINE cesM #-}

runCEST :: (Monad m) => CEST m a -> CESContext -> m (CESReturn a, CESContext)
runCEST m c = do
    ~(a, c') <- runCEST' m c
    case a of
        VoidRet -> return (Left Nothing, c)
        FailureRet str -> return (Left (Just str), c)
        NormalRet a -> return (Right a, c')
{-# INLINE runCEST #-}

execCEST :: (Monad m) => CEST m a -> CESContext -> m CESContext
execCEST m c = runCEST m c >>= return . snd
{-# INLINE execCEST #-}

evalCEST :: (Monad m) => CEST m a -> CESContext -> m (CESReturn a)
evalCEST m c = runCEST m c >>= return . fst
{-# INLINE evalCEST #-}

newEntity :: (Monad m) => CEST m ComponentID
newEntity = ces (\c -> (Right (cesNextID c), c {cesNextID = (ComponentID (getCompID (cesNextID c) + 1))}))
{-# INLINE newEntity #-}

addComponent :: (Monad m, ComponentPhantom a) => a -> ComponentID -> ComponentType a -> CEST m ()
addComponent comp eid entity = ces (\c -> (Right (), c
    { entities = M.alter (\case
        Nothing -> Just (M.singleton eid (unsafeCoerce entity))
        Just m -> Just (M.insert eid (unsafeCoerce entity) m)
        ) (ComponentClass comp) (entities c)
    }))
{-# INLINEABLE addComponent #-}

remComponent :: (Monad m, ComponentPhantom a) => a -> ComponentID -> CEST m ()
remComponent comp entity = ces (\c -> (Right (), c 
    { entities = M.alter (\case
        Nothing -> Nothing
        Just s -> Just (M.delete entity s)
        ) (ComponentClass comp) (entities c)
    }))
{-# INLINE remComponent #-}

onEntities :: (Monad m, ComponentPhantom a) => a -> (ComponentID -> ComponentType a -> CEST m ()) -> CEST m ()
onEntities comp on = CEST (\c -> case M.lookup (ComponentClass comp) (entities c) of
        Nothing -> return (NormalRet (), c)
        Just m -> do
            (_, c') <- runCEST' (mapM_ (\(k,v) -> on k (unsafeCoerce v)) (M.toList m)) c
            return (NormalRet (), c')
    )
{-# INLINABLE onEntities #-}

foldEntities :: (Monad m, ComponentPhantom a) => a -> b -> (b -> ComponentID -> ComponentType a -> CEST m b) -> CEST m b
foldEntities comp d on = CEST (\c -> case M.lookup (ComponentClass comp) (entities c) of
        Nothing -> return (NormalRet d, c)
        Just m -> do
            (r, c') <- runCEST' (foldM (\d' (k,v) -> on d' k (unsafeCoerce v)) d (M.toList m)) c
            case r of
                VoidRet -> return (VoidRet, c)
                FailureRet str -> return (FailureRet str, c)
                NormalRet r -> return (NormalRet r, c')
    )
{-# INLINABLE foldEntities #-}

onEntity :: (Monad m, ComponentPhantom a) => a -> ComponentID -> (ComponentType a -> CEST m (Maybe (ComponentType a))) -> CEST m ()
onEntity comp entity on = CEST (\c -> case M.lookup (ComponentClass comp) (entities c) of
    Nothing -> return (VoidRet, c)
    Just m -> case M.lookup entity m of
        Nothing -> return (VoidRet, c)
        Just x -> runCEST' (on (unsafeCoerce x)) c >>= \case
            (VoidRet, _) -> return (VoidRet, c)
            (FailureRet str, _) -> return (FailureRet str, c)
            (NormalRet Nothing, c') -> return (NormalRet (), c' {entities = M.alter (\case
                Nothing -> Nothing
                Just cs -> Just (M.delete entity cs)
                ) (ComponentClass comp) (entities c')})
            (NormalRet (Just nv), c') -> return (NormalRet (), c' {entities = M.alter (\case
                Nothing -> Just (M.singleton entity (unsafeCoerce nv))
                Just cs -> Just (M.insert entity (unsafeCoerce nv) cs)
                ) (ComponentClass comp) (entities c')})
    )

onEntity_ :: (Monad m, ComponentPhantom a) => a -> ComponentID -> (ComponentType a -> CEST m ()) -> CEST m ()
onEntity_ comp entity on = CEST (\c -> case M.lookup (ComponentClass comp) (entities c) of
    Nothing -> return (VoidRet, c)
    Just m -> case M.lookup entity m of
        Nothing -> return (VoidRet, c)
        Just x -> runCEST' (on (unsafeCoerce x)) c >>= \case
            (VoidRet, _) -> return (VoidRet, c)
            (FailureRet str, _) -> return (FailureRet str, c)
            (NormalRet (), c') -> return (NormalRet (), c')
    )
{-# INLINABLE onEntity_ #-}

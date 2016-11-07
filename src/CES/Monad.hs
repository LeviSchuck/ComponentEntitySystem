{-# LANGUAGE LambdaCase #-}
module CES.Monad where
{- TODO: reexport TC.lift -}

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Typeable

-- import Debug.Trace

import qualified Control.Monad.Trans.Class as TC
import qualified Control.Monad.IO.Class as IOC
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified CES.Data.Internal as DI

newtype CESMonad t m a = CESMonad { runCES :: DI.CESContext t -> m (a, DI.CESContext t) }

instance TC.MonadTrans (CESMonad t) where
  lift m = CESMonad $ \ ctx -> do
    a <- m
    return (a, ctx)
  {-# INLINE lift #-}

instance (IOC.MonadIO m) => IOC.MonadIO (CESMonad t m) where
  liftIO = TC.lift . IOC.liftIO
  {-# INLINE liftIO #-}

instance (Functor m) => Functor (CESMonad t m) where
  fmap f m = CESMonad $ \ ctx ->
    fmap (\ (a, ctx') -> (f a, ctx')) $ runCES m ctx
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (CESMonad t m) where
  pure a = CESMonad $ \ ctx -> return (a, ctx)
  {-# INLINE pure #-}
  CESMonad mf <*> CESMonad mx = CESMonad $ \ ctx -> do
    (f, ctx') <- mf ctx
    (x, ctx'') <- mx ctx'
    return (f x, ctx'')
  {-# INLINE (<*>) #-}

instance (Monad m) => Monad (CESMonad t m) where
  return a = CESMonad $ \ ctx -> return (a, ctx)
  {-# INLINE return #-}
  m >>= k  = CESMonad $ \ ctx -> do
    (a, ctx') <- runCES m ctx
    runCES (k a) ctx'
  {-# INLINE (>>=) #-}
  fail str = CESMonad $ \ _ -> fail str
  {-# INLINE fail #-}


ces :: (Monad m) => (DI.CESContext t -> (a, DI.CESContext t)) -> CESMonad t m a
ces f = CESMonad (return . f)
{-# INLINE ces #-}

new :: (Monad m) => CESMonad t m DI.EntityID
new = ces DI.nextEntity

kill :: (Monad m) => DI.EntityID -> CESMonad t m ()
kill ent = ces (DI.killEntity ent) >>= \case
  DI.Success -> return ()
  f -> fail (DI.err f)

get :: (Monad m, DI.CESType t, Typeable d) => DI.EntityID -> t -> CESMonad t m (Maybe d)
get ent ty = ces (DI.getEntityType ent ty) >>= \case
  (DI.Success, x) -> return x
  (DI.NoData, _) -> return Nothing
  (f, _) -> fail (DI.err f)

has :: (Monad m, DI.CESType t) => DI.EntityID -> t -> CESMonad t m Bool
has ent ty = ces (DI.hasEntityType ent ty) >>= \case
  (DI.Success, x) -> return x
  (f, _) -> fail (DI.err f)

add :: (Monad m, DI.CESType t, Typeable d) => DI.EntityID -> t -> d -> CESMonad t m ()
add ent ty dat = ces (DI.addEntityType ent ty dat) >>= \case
  DI.Success -> return ()
  f -> fail (DI.err f)

set :: (Monad m, DI.CESType t, Typeable d) => DI.EntityID -> t -> d -> CESMonad t m ()
set ent ty dat = ces (DI.setEntityType ent ty dat) >>= \case
  DI.Success -> return ()
  f -> fail (DI.err f)

remove :: (Monad m, DI.CESType t) => DI.EntityID -> t -> CESMonad t m ()
remove ent ty = ces (DI.killEntityType ent ty) >>= \case
  DI.Success -> return ()
  f -> fail (DI.err f)

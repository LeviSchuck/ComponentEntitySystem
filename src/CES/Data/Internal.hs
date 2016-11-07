{-# LANGUAGE ScopedTypeVariables #-}
module CES.Data.Internal where

import Data.Word
import Data.Typeable
import Data.Type.Equality
import Unsafe.Coerce
import GHC.Exts

import qualified Data.Map.Strict as M
import qualified Data.Set as S

newtype EntityID = EntityID { unEntID :: Int } deriving (Eq, Ord)

instance Show EntityID where
  show (EntityID e) = show e

class Ord a => CESType a where
  cesTypeOf :: a -> TypeRep

data CESContext a = CESContext
  { cesEntities :: M.Map EntityID (M.Map a Any)
  , cesNextEntity :: EntityID
  }

instance (Show a) => Show (CESContext a) where
  show c =  "CESContext "
         ++ "{ cesEntities=" ++ show (M.toList (M.map M.keys (cesEntities c)))
         ++ ", cesNextEntity=" ++ show (cesNextEntity c)
         ++ "}"

data CESResult
  = NoEntity
  | NoData
  | NoType
  | DataExists
  | BadType
  | Success
  deriving (Eq,Ord,Show)

err :: CESResult -> String
err NoEntity = "CES: No such entity"
err NoData = "CES: No such entity type data"
err NoType = "CES: No such type"
err BadType = "CES: Invalid type pulled"
err DataExists = "CES: Entity type data exists"
err Success = "CES: Success"

emptyContext :: CESContext a
emptyContext = CESContext
  { cesEntities = M.empty
  , cesNextEntity = EntityID 1
  }

nextEntity :: CESContext a -> (EntityID, CESContext a)
nextEntity ctx = (EntityID e, nctx)
  where
    EntityID e = cesNextEntity ctx
    ee = EntityID e
    ce = cesEntities ctx
    nctx = ctx  { cesNextEntity = EntityID (e + 1)
                , cesEntities = M.insert ee M.empty ce
                }

addEntityType :: (CESType a, Typeable b) => EntityID -> a -> b -> CESContext a -> (CESResult, CESContext a)
addEntityType ent ty dat ctx = if cesTypeOf ty == typeOf dat
  then case M.lookup ent ce of
    Nothing -> (NoEntity, ctx)
    Just me -> case M.lookup ty me of
      Nothing -> (Success, ctx { cesEntities = M.insert ent (M.insert ty cdat me) ce })
      Just _ -> (DataExists, ctx)
  else (BadType, ctx)
  where
    cdat = unsafeCoerce dat
    ce = cesEntities ctx

setEntityType :: (CESType a, Typeable b) => EntityID -> a -> b -> CESContext a -> (CESResult, CESContext a)
setEntityType ent ty dat ctx = if cesTypeOf ty == typeOf dat
  then case M.lookup ent ce of
    Nothing -> (NoEntity, ctx)
    Just me -> case M.lookup ty me of
      Nothing -> (NoData, ctx)
      Just _ -> (Success, ctx { cesEntities = M.insert ent (M.insert ty cdat me) ce })
  else (BadType, ctx)
  where
    cdat = unsafeCoerce dat
    ce = cesEntities ctx

hasEntityType :: (CESType a) => EntityID -> a -> CESContext a -> ((CESResult, Bool), CESContext a)
hasEntityType ent ty ctx = case M.lookup ent ce of
    Nothing -> ((NoEntity, False), ctx)
    Just me -> case M.lookup ty me of
      Nothing -> ((Success, False), ctx)
      Just _ -> ((Success, True), ctx)
  where
    ce = cesEntities ctx



killEntity :: EntityID -> CESContext a -> (CESResult, CESContext a)
killEntity ent ctx = case M.lookup ent ce of
  Nothing -> (NoEntity, ctx)
  Just _ -> (Success, ctx { cesEntities = M.delete ent ce })
  where
    ce = cesEntities ctx

killEntityType :: CESType a => EntityID -> a -> CESContext a -> (CESResult, CESContext a)
killEntityType ent ty ctx = case M.lookup ent ce of
  Nothing -> (NoEntity, ctx)
  Just e -> case M.lookup ty e of
    Nothing -> (NoData, ctx)
    Just _ -> (Success, ctx { cesEntities = M.insert ent (M.delete ty e) ce })
  where
    ce = cesEntities ctx

getEntityType :: (CESType a, Typeable b) => EntityID -> a -> CESContext a -> ((CESResult, Maybe b), CESContext a)
getEntityType ent ty ctx = case M.lookup ent (cesEntities ctx) of
  Nothing -> ((NoEntity, Nothing), ctx)
  Just e -> case M.lookup ty e of
    Nothing -> ((NoData, Nothing), ctx)
    Just r -> ((Success, Just (unsafeCoerce r)), ctx)

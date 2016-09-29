{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module CES.Data.Internal where

import qualified GHC.Base(Any(..))
import Data.List(intersperse)

import qualified Data.Map as M


class ComponentPhantom a where
    type ComponentType a
    componentPhantomInt :: a -> Int
    componentName :: a -> String

data ComponentClass = forall a. ComponentPhantom a => ComponentClass a

instance Eq ComponentClass where
    ComponentClass a == ComponentClass b = componentPhantomInt a == componentPhantomInt b

instance Ord ComponentClass where
    compare (ComponentClass a) (ComponentClass b) = compare (componentPhantomInt a) (componentPhantomInt b)

instance Show ComponentClass where
    show (ComponentClass a) = componentName a

newtype ComponentID = ComponentID { getCompID :: Int } deriving(Eq,Ord)

instance Show ComponentID where
    show (ComponentID n) = show n

data CESContext = CESContext
    { cesNextID :: ComponentID
    , entities :: M.Map ComponentClass (M.Map ComponentID GHC.Base.Any)
    }

instance Show CESContext where
    show CESContext{..} = "CESContext {cesNextID=" 
        ++ show cesNextID
        ++ ",entities=" ++ (foldl (++) "" (intersperse "," (map (f1) (M.toList entities)))) ++ "}"
        where
            f1 (c, cs) = "(" ++ show c ++ "," ++ (show (M.keys cs)) ++ ")"

data CESRet a
    = NormalRet a
    | FailureRet String
    | VoidRet
    deriving (Eq,Ord,Show)

instance Functor CESRet where
    fmap _ (FailureRet x) = FailureRet x
    fmap _ (VoidRet) = VoidRet
    fmap f (NormalRet y) = NormalRet (f y)

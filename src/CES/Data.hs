module CES.Data
    ( CESContext
    , ComponentID
    , ComponentClass(..)
    , ComponentPhantom(..)
    , defCES
    , CESReturn(..)
    )
where

import qualified Data.Map as M

import CES.Data.Internal

defCES = CESContext
    { cesNextID = ComponentID 1
    , entities = M.empty
    }

type CESReturn a = Either (Maybe String) a

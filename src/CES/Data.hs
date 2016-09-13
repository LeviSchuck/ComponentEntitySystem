module CES.Data where

import qualified Data.Map as M
import qualified Data.Set as S

type ComponentID = Int

data CESContext c = CESContext
    { cesNextID :: ComponentID
    , entities :: M.Map c (S.Set ComponentID)
    }
    deriving (Show)

defCES = CESContext
    { cesNextID = 1
    , entities = M.empty
    }



module Fresh.Module where

import Fresh.Types (Id, Class, ClassId, Instance)
import Fresh.Expr (EVarName)


import           Data.Map ( Map )

import Data.Monoid ((<>))

data Module t e
    = Module
    { moduleTypes :: Map Id t
    , moduleExprs :: Map EVarName (e)
    , moduleClasses :: Map ClassId (Class t e)
    , moduleInstances :: [ Instance t e ]
    }

instance Monoid (Module t e) where
    mempty =
        Module
        { moduleTypes = mempty
        , moduleExprs = mempty
        , moduleClasses = mempty
        , moduleInstances = mempty
        }
    mappend x y =
        Module
        { moduleTypes = moduleTypes x <> moduleTypes y
        , moduleExprs = moduleExprs x <> moduleExprs y
        , moduleClasses = moduleClasses x <> moduleClasses y
        , moduleInstances = moduleInstances x <> moduleInstances y
        }


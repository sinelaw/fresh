{-# LANGUAGE TemplateHaskell #-}
module Fresh.Kind where

import Control.Monad (join)
import Control.Applicative (liftA2)

#ifdef QC
import Test.QuickCheck
#endif

data Kind = Star | Kfun Kind Kind
    deriving (Eq, Ord, Show)

class HasKind t where
    kind :: t -> Kind


app :: Kind -> Kind -> Maybe Kind
app (Kfun k1 k2) kArg | k1 == kArg = Just k2
app _ _ = Nothing

compose :: Kind -> Kind -> Maybe Kind
compose (Kfun fb fc) (Kfun ga gb) | fb == gb = Just $ Kfun ga fc
compose _ _ = Nothing

prop_composeAssoc :: Kind -> Kind -> Kind -> Bool
prop_composeAssoc f g h = (f `compose` g) `mCompose` (Just h) == (Just f) `mCompose` (g `compose` h)
    where mCompose x y = join $ liftA2 compose x y

-- QC
#ifdef QC

instance Arbitrary Kind where
  arbitrary =
      oneof
      [ return Star
      , Kfun <$> arbitrary <*> arbitrary
      ]

return []
runTests = $quickCheckAll

#endif


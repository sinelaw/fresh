{-# LANGUAGE TemplateHaskell #-}
module Fresh.Kind where

import Fresh.Func (Func(..))
import qualified Fresh.Func as Func

#ifdef QC
import Test.QuickCheck
#endif

data Kind = Star | Kfun (Func Kind Kind)
    deriving (Eq, Ord, Show)

app :: Kind -> Kind -> Maybe Kind
app (Kfun f) k = Func.app f k
app _        _ = Nothing

compose :: Kind -> Kind -> Maybe (Kind)
compose (Kfun f) (Kfun g) = Kfun <$> f `Func.compose` g
compose _        _        = Nothing


#ifdef QC

instance Arbitrary Kind where
  arbitrary =
      oneof
      [ return Star
      , Kfun <$> arbitrary
      ]

return []
runTests = $quickCheckAll

#endif


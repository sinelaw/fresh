{-# LANGUAGE TemplateHaskell #-}
module Fresh.Func
       ( Func(..)
       , app
       , compose
#ifdef QC
       , runTests
#endif
       ) where

import Control.Monad (join)
import Control.Applicative (liftA2)

#ifdef QC
import Test.QuickCheck
#endif

-- | Functions from some domain to some range. Polymorphic in the type of the domain and range.
-- Values of type 'a' are domains, value of type 'b' are codomains.
data Func a b = Func a b
    deriving (Eq, Ord, Show)

app :: Eq a => Func a b -> a -> Maybe b
app (Func expectedArg res) arg
    | arg == expectedArg = Just res
    | otherwise          = Nothing

compose :: Eq b => Func b c -> Func a b -> Maybe (Func a c)
compose (Func fb fc) (Func ga gb)
    | fb == gb  = Just $ Func ga fc
    | otherwise = Nothing

prop_composeAssoc :: (Eq a, Eq b, Eq c, Eq d) => Func c d -> Func b c -> Func a b -> Bool
prop_composeAssoc f g h = (f `compose` g) `mCompose` (Just h) == (Just f) `mCompose` (g `compose` h)
    where mCompose x y = join $ liftA2 compose x y

-- QC
#ifdef QC

instance (Arbitrary a, Arbitrary b) => Arbitrary (Func a b) where
  arbitrary = Func <$> arbitrary <*> arbitrary

return []
runTests = $quickCheckAll

#endif


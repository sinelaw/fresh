{-# LANGUAGE TemplateHaskell #-}
module Fresh.Kind where

#ifdef QC
import Test.QuickCheck
#endif

data Kind = Star
          | Composite
          | KArrow Kind Kind
    deriving (Eq, Ord, Show)

app :: Kind -> Kind -> Maybe Kind
app (KArrow x y) x' | x == x' = Just y
app _            _            = Nothing

compose :: Kind -> Kind -> Maybe Kind
compose (KArrow y' z) (KArrow x y)
    | y' == y = Just $ KArrow x z
compose _            _             = Nothing


#ifdef QC

instance Arbitrary Kind where
  arbitrary =
      oneof
      [ return Star
      , KArrow <$> arbitrary <*> arbitrary
      ]

return []
runTests = $verboseCheckAll

#endif


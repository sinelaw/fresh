module Fresh.Kind where

data Kind = Star | Kfun Kind Kind
    deriving (Eq, Ord)

class HasKind t where
    kind :: t -> Kind


app :: Kind -> Kind -> Maybe Kind
app (Kfun k1 k2) kArg | k1 == kArg = Just k2
app _ _ = Nothing



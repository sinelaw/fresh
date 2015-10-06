-- | Typing Haskell in Haskell, https://web.cecs.pdx.edu/~mpj/thih/thih.pdf
module Main where

import           Control.Monad (msum)
import           Data.List     (intersect, nub, partition, union, (\\))

type Id = String

enumId :: Int -> Id
enumId n = "v" ++ show n

data Kind = Star | Kfun Kind Kind
    deriving Eq

data Type
    = TVar Tyvar
    | TCon Tycon
    | TAp Type Type
    | TGen Int
    deriving Eq

data Tyvar = Tyvar Id Kind
    deriving Eq

data Tycon = Tycon Id Kind
    deriving Eq

-- Example built-in types
tChar = TCon (Tycon "Char" Star )
tDouble = TCon (Tycon "Double" Star )
tInteger = TCon (Tycon "Integer" Star )
tList = TCon (Tycon "[]" (Kfun Star Star ))
tArrow = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star )))
tTuple2 = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star )))

-- Type synonyms expanded:
tString :: Type
tString = list tChar

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b
list :: Type -> Type
list t = TAp tList t


pair :: Type -> Type -> Type
pair a b = TAp (TAp tTuple2 a) b

class HasKind t where
    kind :: t -> Kind

instance HasKind Tyvar where
    kind (Tyvar v k) = k

instance HasKind Tycon where
    kind (Tycon v k) = k

instance HasKind Type where
    kind (TCon tc) = kind tc
    kind (TVar u) = kind u
    kind (TAp t _) = case (kind t) of (Kfun _ k) -> k


type Subst = [(Tyvar , Type)]

nullSubst :: Subst
nullSubst = []

-- map a single tyvar to a type
-- TODO: assert kinds match
(+->) :: Tyvar -> Type -> Subst
u +-> t = [(u, t)]

class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [Tyvar]

instance Types Type where
    apply s (TVar u) = case lookup u s of
        Just t -> t
        Nothing -> TVar u
    apply s (TAp l r ) = TAp (apply s l) (apply s r )
    apply s t = t

    tv (TVar u) = [u]
    tv (TAp l r ) = tv l `union` tv r
    tv t = []

instance Types a => Types [a] where
    apply s = map (apply s)
    tv = nub . concat . map tv

-- left-biased substition composition
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1@@s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1 ++ s2) else fail "merge fails"
    where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                      (map fst s1 `intersect` map fst s2)

mgu :: Monad m => Type -> Type -> m Subst
varBind :: Monad m => Tyvar -> Type -> m Subst

mgu (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2@@s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return nullSubst
mgu t1 t2 = fail "types do not unify"

varBind u t
    | t == TVar u = return nullSubst
    | u `elem` tv t = fail "occurs check fails"
    | kind u /= kind t = fail "kinds do not match"
    | otherwise = return (u +-> t)

-- match t1 t2 finds a substitution s such that: apply s t1 = t2
match :: Monad m => Type -> Type -> m Subst
match (TAp l r ) (TAp l' r') = do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
match (TVar u) t
    | kind u == kind t = return (u +-> t)
match (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return nullSubst
match t1 t2 = fail "types do not match"

-----------------------------------------------------------------
--- Type Classes ---

data Qual t = [Pred] :=> t
    deriving Eq

data Pred = IsIn Id Type
    deriving Eq

instance Types t => Types (Qual t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    tv (ps :=> t) = tv ps `union` tv t

instance Types Pred where
    apply s (IsIn i t) = IsIn i (apply s t)
    tv (IsIn i t) = tv t

mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu
matchPred = lift match

lift m (IsIn i t) (IsIn i' t')
    | i == i' = m t t'
    | otherwise = fail "classes differ"

-- Type classes: list of super classes + list of instances
-- TODO (missing): list of methods per class + method implementations per instance
type Class = ([Id], [Inst])
type Inst = Qual Pred

data ClassEnv =
    ClassEnv
    { classes  :: Id -> Maybe Class
    , defaults :: [Type] }

-- Extract superclass/instance info from a class env given class Id.
-- "These functions are intended to be used only in cases where it is known that the
--  class i is defined in the environment ce"
super :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of Just (is, its) -> is
insts :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of Just (is, its) -> its

defined :: Maybe a -> Bool
defined (Just x ) = True
defined Nothing = False

-- Building up class environments:

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce { classes = \j -> if i == j then Just c else classes ce j }

initialEnv :: ClassEnv
initialEnv =
    ClassEnv
    { classes = \i -> fail "class not defined"
    , defaults = [tInteger , tDouble] }

type EnvTransformer = ClassEnv -> Maybe ClassEnv

-- EnvTransformer composition
infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do ce' <- f ce
                  g ce'

addClass :: Id → [Id] → EnvTransformer
addClass i is ce
| defined (classes ce i) = fail “class already defined”
| any (not . defined . classes ce) is = fail “superclass not defined”
| otherwise = return (modify ce i (is, [ ]))



-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"


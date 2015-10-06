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
    | TGen Int -- quantified type variable, int is index into kinds list
               -- in Scheme
    deriving Eq

data Tyvar = Tyvar Id Kind
    deriving Eq

data Tycon = Tycon Id Kind
    deriving Eq

-- Example built-in types
tUnit = TCon (Tycon "()" Star )
tInt = TCon (Tycon "Int" Star )
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


-- Note: "The Haskell report imposes some further restrictions on class
-- and instance declarations that are not enforced by the definitions of
-- addClass and addInst. For example, the superclasses of a class should
-- have the same kind as the class itself; the parameters of any
-- predicates in an instance context should be type variables, each of
-- which should appear in the head of the instance; and the type
-- appearing in the head of an instance should consist of a type
-- constructor applied to a sequence of distinct type variable
-- arguments."

data ClassEnv =
    ClassEnv
    { classes  :: Id -> Maybe Class
    , defaults :: [Type] }

-- Extract superclass/instance info from a class env given class Id.
-- "These functions are intended to be used only in cases where it is
-- known that the class i is defined in the environment ce"
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

addClass :: Id -> [Id] -> EnvTransformer
addClass i is ce
    | defined (classes ce i) = fail "class already defined"
    | any (not . defined . classes ce) is = fail "superclass not defined"
    | otherwise = return (modify ce i (is, []))

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses <:> addNumClasses

addCoreClasses :: EnvTransformer
addCoreClasses =
    addClass "Eq" [ ]
    <:> addClass "Ord" ["Eq"]
    <:> addClass "Show" [ ]
    <:> addClass "Read" [ ]
    <:> addClass "Bounded" [ ]
    <:> addClass "Enum" [ ]
    <:> addClass "Functor" [ ]
    <:> addClass "Monad" [ ]

addNumClasses :: EnvTransformer
addNumClasses =
    addClass "Num" ["Eq", "Show"]
    <:> addClass "Real" ["Num", "Ord"]
    <:> addClass "Fractional" ["Num"]
    <:> addClass "Integral" ["Real", "Enum"]
    <:> addClass "RealFrac" ["Real", "Fractional"]
    <:> addClass "Floating" ["Fractional"]
    <:> addClass "RealFloat" ["RealFrac", "Floating"]


-- add new instance to existing class
addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
    | not (defined (classes ce i)) = fail "no class for instance"
    | any (overlap p) qs = fail "overlapping instance"
    | otherwise = return (modify ce i c)
    where
        its = insts ce i
        qs = [q | (_ :=> q) <- its]
        c = (super ce i, (ps :=> p) : its)

-- "Two instances for a class are said to overlap if there is some
-- predicate that is a substitution instance of the heads of both
-- instance declarations."
overlap :: Pred -> Pred -> Bool
overlap p q = defined (mguPred p q)


exampleInsts :: EnvTransformer
exampleInsts =
    addPreludeClasses
    <:> addInst [ ] (IsIn "Ord" tUnit)
    <:> addInst [ ] (IsIn "Ord" tChar )
    <:> addInst [ ] (IsIn "Ord" tInt)
    <:> addInst [IsIn "Ord" (TVar (Tyvar "a" Star )),
    IsIn "Ord" (TVar (Tyvar "b" Star ))]
    (IsIn "Ord" (pair (TVar (Tyvar "a" Star ))
    (TVar (Tyvar "b" Star ))))

-- if a class predicate is true about a type, also all super-class
-- predicates are true about it
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t)
    = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

-- find an instance that matches given pred (using matchPred) and return
-- the (substituted) super-class predicates as the next goals to check
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t) = msum [ tryInst it | it <- insts ce i ]
    where tryInst (ps :=> h) = do
            u <- matchPred h p
            Just (map (apply u) ps)

-- True if, and only if, the predicate p will hold whenever all of the
-- predicates in ps are satisfied:
entail  :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
                   case byInst ce p of
                     Nothing -> False
                     Just qs -> all (entail ce ps) qs

-- predicate "head normal form"
inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t
    where hnf (TVar v)  = True
          hnf (TCon tc) = False
          hnf (TAp t _) = hnf t


toHnfs :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | inHnf p   = return [p]
           | otherwise = case byInst ce p of
                           Nothing -> fail "context reduction"
                           Just ps -> toHnfs ce ps

-- Context reduction:
simplify   :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
    where loop rs []                            = rs
          loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
                         | otherwise            = loop (p:rs) ps

reduce      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  return (simplify ce qs)

-- Entailment only bySuper (according to THIH, 'reduce' could be using this instead of the full 'simplify')
scEntail        :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

--------------------------------------------------------
-- Type Schemes (section 8)

data Scheme = Forall [Kind] (Qual Type)
    deriving Eq

instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    tv (Forall ks qt)      = tv qt

-- quanitfy always constructs schemes in the same form: kinds are
-- ordered by where each tvar appears in the type, left-most first.
-- Allows easy comparison for alpha-equivalence.
quantify      :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
    where vs' = [ v | v <- tv qt, v `elem` vs ]
          ks  = map kind vs'
          s   = zip vs' (map TGen [0..])

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)

---------------------------------------------------------
-- Assumptions (section 9)

data Assump = Id :>: Scheme

instance Types Assump where
    apply s (i :>: sc) = i :>: (apply s sc)
    tv (i :>: sc)      = tv sc



-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"


-- | Typing Haskell in Haskell, https://web.cecs.pdx.edu/~mpj/thih/thih.pdf
module Main where

--import Control.Applicative (Applicative(..))
import Control.Monad (msum, ap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intersect, nub, partition, union, (\\))

type Id = String

enumId :: Int -> Id
enumId n = "v" ++ show n

data Kind = Star | Kfun Kind Kind
    deriving (Eq, Ord)

instance Show Kind where
    show Star = "*"
    show (Kfun k1 k2) = show k1 ++ " -> " ++ show k2

data Type
    = TVar Tyvar
    | TCon Tycon
    | TAp Type Type
    | TGen Int -- quantified type variable, int is index into kinds list
               -- in Scheme
    deriving (Eq)

instance Show Type where
    show (TVar t1) = show t1
    show (TCon tc) = "TCon " ++ show tc
    show (TAp t1 t2) = show t1 ++ " -> " ++ show t2
    show (TGen n) = 't' : show n

data Tyvar = Tyvar Id Kind
    deriving (Show, Eq, Ord)

data Tycon = Tycon Id Kind
    deriving (Eq)

instance Show Tycon where
    show (Tycon i k) = "(" ++ show i ++ " :: " ++ show k ++ ")"

-- Example built-in types
tUnit :: Type
tUnit = TCon (Tycon "()" Star )
tInt :: Type
tInt = TCon (Tycon "Int" Star )
tChar :: Type
tChar = TCon (Tycon "Char" Star )
tDouble :: Type
tDouble = TCon (Tycon "Double" Star )
tInteger :: Type
tInteger = TCon (Tycon "Integer" Star )
tList :: Type
tList = TCon (Tycon "[]" (Kfun Star Star ))
tArrow :: Type
tArrow = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star )))
tTuple2 :: Type
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
    kind (Tyvar _v k) = k

instance HasKind Tycon where
    kind (Tycon _v k) = k

instance HasKind Type where
    kind (TCon tc) = kind tc
    kind (TVar u) = kind u
    kind (TAp t _) = case (kind t) of (Kfun _ k) -> k


newtype Subst = Subst { unSubst :: Map Tyvar Type }

nullSubst :: Subst
nullSubst = Subst Map.empty

-- map a single tyvar to a type
-- TODO: assert kinds match
(+->) :: Tyvar -> Type -> Subst
u +-> t = Subst $ Map.singleton u t

class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [Tyvar]

instance Types Type where
    apply (Subst sm) (TVar u) = case Map.lookup u sm of
        Just t -> t
        Nothing -> TVar u
    apply s (TAp l r ) = TAp (apply s l) (apply s r )
    apply _s t = t

    tv (TVar u) = [u]
    tv (TAp l r ) = tv l `union` tv r
    tv _t = []

instance Types a => Types [a] where
    apply s = map (apply s)
    tv = nub . concat . map tv

-- left-biased substition composition
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
(Subst s1)@@(Subst s2) = Subst $ Map.map (apply $ Subst s1) s2 `Map.union` s1

merge :: Monad m => Subst -> Subst -> m Subst
merge (Subst s1) (Subst s2) = if agree then return (Subst $ Map.union s1 s2) else fail "merge fails"
    where agree = all
                  (\v -> apply (Subst s1) (TVar v) == apply (Subst s2) (TVar v))
                  (Map.keys $ Map.intersection s1 s2)

mgu :: Monad m => Type -> Type -> m Subst
mgu (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2@@s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return nullSubst
mgu _t1 _t2 = fail "types do not unify"

varBind :: Monad m => Tyvar -> Type -> m Subst
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
match _t1 _t2 = fail "types do not match"

-----------------------------------------------------------------
--- Type Classes ---

data Qual t = [Pred] :=> t
    deriving (Eq)

instance Show t => Show (Qual t) where
    show (p :=> t) = show p ++ " => " ++ show t

data Pred = IsIn Id Type
    deriving (Show, Eq)

instance Types t => Types (Qual t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    tv (ps :=> t) = tv ps `union` tv t

instance Types Pred where
    apply s (IsIn i t) = IsIn i (apply s t)
    tv (IsIn _i t) = tv t

mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu
matchPred = lift match

lift :: Monad m =>
        (Type -> Type -> m a) -> Pred -> Pred -> m a
lift m (IsIn i t) (IsIn i' t')
    | i == i' = m t t'
    | otherwise = fail "classes differ"

-- Type classes: list of super classes + list of instances
-- TODO (missing): list of methods per class + method implementations per instance
data Class = Class { classSupers :: [Id], classInstances :: [Inst] }
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
super ce i = case classes ce i of Just c -> classSupers c

insts :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of Just c -> classInstances c

defined :: Maybe a -> Bool
defined (Just _x ) = True
defined Nothing = False

-- Building up class environments:

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce { classes = \j -> if i == j then Just c else classes ce j }

initialEnv :: ClassEnv
initialEnv =
    ClassEnv
    { classes = \_ -> fail "class not defined"
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
    | otherwise = return (modify ce i (Class { classSupers = is, classInstances = [] }))

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
        c = Class { classSupers = super ce i, classInstances = (ps :=> p) : its }

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
byInst ce p@(IsIn i _t) = msum [ tryInst it | it <- insts ce i ]
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
inHnf (IsIn _c t) = hnf t
    where hnf (TVar _v)  = True
          hnf (TCon _tc) = False
          hnf (TAp t' _) = hnf t'


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
    deriving (Eq)

instance Show Scheme where
    show (Forall ks qt) = "forall " ++ (concatMap (\(i,k) -> ('t':show i) ++ " :: " ++ show k) $ zip ([0..] :: [Integer]) ks) ++ ". " ++ show qt

instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    tv (Forall _ks qt)      = tv qt

-- quanitfy always constructs schemes in the same form: kinds are
-- ordered by where each tvar appears in the type, left-most first.
-- Allows easy comparison for alpha-equivalence.
quantify      :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
    where vs' = [ v | v <- tv qt, v `elem` vs ]
          ks  = map kind vs'
          s   = Subst . Map.fromList $ zip vs' (map TGen [0..])

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)

---------------------------------------------------------
-- Assumptions (section 9)

data Assump = Id :>: Scheme
            deriving (Show)
instance Types Assump where
    apply s (i :>: sc) = i :>: (apply s sc)
    tv (_i :>: sc)      = tv sc

-- lookup variable in list of assumptions
find                 :: Monad m => Id -> [Assump] -> m Scheme
find i []             = fail ("unbound identifier: " ++ i)
find i ((i':>:sc):as) = if i==i' then return sc else find i as

---------------------------------------------------------
-- Type Inference Monad

newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

instance Monad TI where
    return x   = TI (\s n -> (s,n,x))
    TI f >>= g = TI (\s n -> case f s n of
                              (s',m,x) -> let TI gx = g x
                                          in  gx s' m)

instance Applicative TI where
    pure = return
    (<*>) = ap

instance Functor TI where
    fmap f x = pure f <*> x

runTI       :: TI a -> a
runTI (TI f) = x where (_s,_n,x) = f nullSubst 0

getSubst   :: TI Subst
getSubst    = TI (\s n -> (s,n,s))

unify      :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst   :: Subst -> TI ()
extSubst s' = TI (\s n -> (s'@@s, n, ()))

newTVar    :: Kind -> TI Type
newTVar k   = TI (\s n -> let v = Tyvar (enumId n) k
                          in  (s, n+1, TVar v))

freshInst               :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

class Instantiate t where
  inst  :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst _ts t         = t
instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

data Literal = LitInt  Integer
             | LitChar Char
             | LitRat  Rational
             | LitStr  String

tiLit            :: Literal -> TI ([Pred],Type)
tiLit (LitChar _) = return ([], tChar)
tiLit (LitInt _)  = do v <- newTVar Star
                       return ([IsIn "Num" v], v)
tiLit (LitStr _)  = return ([], tString)
tiLit (LitRat _)  = do v <- newTVar Star
                       return ([IsIn "Fractional" v], v)

data Pat        = PVar Id
                | PWildcard
                | PAs  Id Pat -- x@Pat...
                | PLit Literal
                | PNpk Id Integer
                | PCon Assump [Pat]

tiPat :: Pat -> TI ([Pred], [Assump], Type)
-- assumes each name 'i' appears exactly once per pattern
tiPat (PVar i) = do v <- newTVar Star
                    return ([], [i :>: toScheme v], v)
tiPat PWildcard   = do v <- newTVar Star
                       return ([], [], v)
tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
                       return (ps, (i:>:toScheme t):as, t)
tiPat (PLit l) = do (ps, t) <- tiLit l
                    return (ps, [], t)
tiPat (PNpk i _k)  = do t <- newTVar Star
                        return ([IsIn "Integral" t], [i:>:toScheme t], t)
tiPat (PCon (_i:>:sc) pats) = do (ps,as,ts) <- tiPats pats
                                 t'         <- newTVar Star
                                 (qs :=> t) <- freshInst sc
                                 unify t (foldr fn t' ts)
                                 return (ps++qs, as, t')

tiPats     :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do psasts <- mapM tiPat pats
                 let ps = concat [ ps' | (ps',_,_) <- psasts ]
                     as = concat [ as' | (_,as',_) <- psasts ]
                     ts = [ t | (_,_,t) <- psasts ]
                 return (ps, as, ts)

----------------------------------------------------------------------

data Expr = Var   Id
          | Lit   Literal
          | Const Assump
          | Ap    Expr Expr
          | Let   BindGroup Expr


----------------------------------------------------------------------

tiExpr                       :: Infer Expr Type
tiExpr _ce as (Var i)          = do sc         <- find i as
                                    (ps :=> t) <- freshInst sc
                                    return (ps, t)
tiExpr _ce _as (Const (_i:>:sc)) = do (ps :=> t) <- freshInst sc
                                      return (ps, t)
tiExpr _ce _as (Lit l)          = do (ps,t) <- tiLit l
                                     return (ps, t)
tiExpr ce as (Ap e f)         = do (ps,te) <- tiExpr ce as e
                                   (qs,tf) <- tiExpr ce as f
                                   t       <- newTVar Star
                                   unify (tf `fn` t) te
                                   return (ps++qs, t)
tiExpr ce as (Let bg e)       = do (ps, as') <- tiBindGroup ce as bg
                                   (qs, t)   <- tiExpr ce (as' ++ as) e
                                   return (ps ++ qs, t)

----------------------------------------------------------------------
-- Alternatives (function definitions by equations)

data Alt = Alt { altPats :: [Pat], altExpr :: Expr }

tiAlt                :: Infer Alt Type
tiAlt ce as (Alt pats e) =
    do (ps, as', ts) <- tiPats pats
       (qs,t)  <- tiExpr ce (as'++as) e
       return (ps++qs, foldr fn t ts)

tiAlts             :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
                         mapM_ (unify t) (map snd psts)
                         return (concat (map fst psts))

----------------------------------------------------------------------
-- 11.5


-- Split - partitions a list of predicates into those that can be
-- deferred to an outer scope, and those that apply on the currently
-- quantified type.
--
-- "... where we have inferred a type t and a list of predicates ps
-- for a function h, we can use split to rewrite and break ps into a
-- pair (ds, rs) of deferred predicates ds and `retained' predicates
-- rs. The predicates in rs will be used to form an inferred type (rs
-- :=> t) for h, while the predicates in ds will be passed out as
-- constraints to the enclosing scope"
--
-- "In addition to a list of predicates ps, the split function is
-- parameterized by two lists of type variables. The first, fs,
-- specifies the set of `fixed' variables, which are just the
-- variables appearing free in the assumptions. The second, gs,
-- specifies the set of variables over which we would like to
-- quantify."
split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
         -> m ([Pred], [Pred])
split ce fs gs ps = do ps' <- reduce ce ps
                       let (ds, rs) = partition (all (`elem` fs) . tv) ps'
                       rs' <- defaultedPreds ce (fs++gs) rs
                       return (ds, rs \\ rs')

-- a type scheme ps => t is ambiguous if ps contains generic variables
-- that do not also appear in t.

-- A type variable that was found to be ambiguous, paired with the
-- list of predicates that must be satisfied in order for a type to
-- server as an appropriate default
data Ambiguity       = Ambiguity { ambTyvar :: Tyvar, ambPreds :: [Pred] }

-- "...a type with a list of predicates ps and that vs lists all known
-- variables, both fixed and generic. An ambiguity occurs precisely if
-- there is a type variable that appears in ps but not in vs (i.e., in
-- tv ps \\ vs)"
ambiguities         :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities _ce vs ps = [ (Ambiguity v $ filter (elem v . tv) ps) | v <- tv ps \\ vs ]


numClasses :: [Id]
numClasses  = ["Num", "Integral", "Floating", "Fractional",
               "Real", "RealFloat", "RealFrac"]

stdClasses :: [Id]
stdClasses  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
               "Functor", "Monad", "MonadPlus"] ++ numClasses

-- candidates for defaulting
candidates           :: ClassEnv -> Ambiguity -> [Type]
candidates ce (Ambiguity v qs) =
    [ t' | let is = [ i | IsIn i _t <- qs ]
               ts = [ t | IsIn _i t <- qs ],
               all ((TVar v)==) ts,
               any (`elem` numClasses) is,
               all (`elem` stdClasses) is,
               t' <- defaults ce,
               all (entail ce []) [ IsIn i t' | i <- is ] ]

-- "...takes care of picking suitable defaults, and of checking
-- whether there are any ambiguities that cannot be eliminated. If
-- defaulting succeeds, then the list of predicates that can be
-- eliminated is obtained by concatenating the predicates in each
-- Ambiguity pair:"
withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a)
                -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
    | any null tss  = fail "cannot resolve ambiguity"
    | otherwise     = return (f vps (map head tss))
    where vps = ambiguities ce vs ps
          tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds  = withDefaults (\vps _ts -> concat (map ambPreds vps))

defaultSubst   :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst    = withDefaults (\vps ts -> Subst . Map.fromList $ zip (map ambTyvar vps) ts)

----------------------------------------------------------------------
-- Binding groups

-- Explicitly typed binding
data Expl = Expl { explId :: Id, explScheme :: Scheme, explAlts :: [Alt] }


tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (Expl _ sc alts)
    = do (qs :=> t) <- freshInst sc
         ps         <- tiAlts ce as alts t
         s          <- getSubst
         let qs'     = apply s qs
             t'      = apply s t
             fs      = tv (apply s as)
             gs      = tv t' \\ fs
             sc'     = quantify gs (qs':=>t')
             ps'     = filter (not . entail ce qs') (apply s ps)
         (ds,rs)    <- split ce fs gs ps'
         if sc /= sc'
             then fail "signature too general"
             else if not (null rs)
                  then fail "context too weak"
                  else return ds

-- Implicit bindings
data Impl = Impl { implId :: Id, implAlts :: [Alt] }

-- monomorphic restriction applies?
restricted   :: [Impl] -> Bool
restricted bs = any simple bs
    where simple (Impl _ alts) = any (null . altPats) alts

tiImpls         :: Infer [Impl] [Assump]
tiImpls ce as bs = do ts <- mapM (\_ -> newTVar Star) bs
                      let is    = map implId bs
                          scs   = map toScheme ts
                          as'   = zipWith (:>:) is scs ++ as
                          altss = map implAlts bs
                      pss <- sequence (zipWith (tiAlts ce as') altss ts)
                      s   <- getSubst
                      let ps'     = apply s (concat pss)
                          ts'     = apply s ts
                          fs      = tv (apply s as)
                          vss     = map tv ts'
                          gs      = foldr1 union vss \\ fs
                      (ds,rs) <- split ce fs (foldr1 intersect vss) ps'
                      if restricted bs then
                          let gs'  = gs \\ tv rs
                              scs' = map (quantify gs' . ([]:=>)) ts'
                          in return (ds++rs, zipWith (:>:) is scs')
                        else
                          let scs' = map (quantify gs . (rs:=>)) ts'
                          in return (ds, zipWith (:>:) is scs')

----------------------------------------------------------------------

data BindGroup = BindGroup { bgExpl :: [Expl], bgImpls :: [[Impl]] }


tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as bg =
  do let as' = [ v:>:sc | (Expl v sc _) <- bgExpl bg ]
     (ps, as'') <- tiSeq tiImpls ce (as'++as) (bgImpls bg)
     qss        <- mapM (tiExpl ce (as''++as'++as)) (bgExpl bg)
     return (ps++concat qss, as''++as')

-- "...typechecks a list of binding groups and accumulates assumptions
-- as it runs through the list:"
tiSeq                  :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq _ti _ce _as []       = return ([],[])
tiSeq ti ce as (bs:bss) = do (ps,as')  <- ti ce as bs
                             (qs,as'') <- tiSeq ti ce (as'++as) bss
                             return (ps++qs, as''++as')

----------------------------------------------------------------------
-- Top level program

newtype Program = Program [BindGroup]


tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram ce as (Program bgs) = runTI $
                      do (ps, as') <- tiSeq tiBindGroup ce as bgs
                         s         <- getSubst
                         rs        <- reduce ce (apply s ps)
                         s'        <- defaultSubst ce [] rs
                         return (apply (s'@@s) as')


----------------------------------------------------------------------
idAlt :: Alt
idAlt = Alt { altPats = [PVar "x"], altExpr = Var "x" }
testImplBinding :: Impl
testImplBinding = Impl { implId = "id", implAlts = [idAlt] }
testProg :: Program
testProg = Program [BindGroup { bgExpl = [], bgImpls = [[testImplBinding]] }]
----------------------------------------------------------------------

-- | The main entry point.
main :: IO ()
main = print $ tiProgram initialEnv [] testProg


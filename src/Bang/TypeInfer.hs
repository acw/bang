{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
module Bang.TypeInfer(typeInfer)
 where

import           Bang.Syntax.AST
import           Bang.Syntax.Location(unknownLocation)
import           Control.Lens(view, over)
import           Control.Lens.TH(makeLenses)
import           Data.List(union, nub, concat, intersect)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Text.Lazy(pack)
import           MonadLib(StateT, ExceptionT, Id,
                          StateM(..), ExceptionM(..), RunExceptionM(..),
                          runStateT, runExceptionT, runId,
                          get, raise)

-- -----------------------------------------------------------------------------

type Substitution = Map Name Type

class Types t where
  apply :: Substitution -> t -> t
  tv    :: t          -> [Name]

nullSubstitution :: Substitution
nullSubstitution = Map.empty

(⟼) :: Name -> Type -> Substitution
(⟼) = Map.singleton

infixr 4 @@
(@@) :: Substitution -> Substitution -> Substitution
(@@) s1 s2 =
  let s2' = Map.map (\ t -> apply s1 t) s1 
  in Map.union s2' s1

-- -----------------------------------------------------------------------------

data InferenceError = UnificationError  Type  Type
                    | OccursCheckFails  Name  Type
                    | KindCheckFails    Name  Type
                    | MatchFailure      Type  Type
                    | UnboundIdentifier       Name
                    | MergeFailure      Substitution Substitution
 deriving (Show)

data InferenceState = InferenceState {
       _istCurrentSubstitution :: Substitution
     , _istNextIdentifier      :: Word
     }

makeLenses ''InferenceState

newtype Infer a = Infer {
          unInfer :: StateT InferenceState (ExceptionT InferenceError Id) a
        }
 deriving (Functor, Applicative, Monad)

instance StateM Infer InferenceState where
  get = Infer   get
  set = Infer . set

instance ExceptionM Infer InferenceError where
  raise = Infer . raise

instance RunExceptionM Infer InferenceError where
  try m = Infer (try (unInfer m))

-- -----------------------------------------------------------------------------

merge :: Substitution -> Substitution -> Infer Substitution
merge s1 s2 | agree     = return (Map.union s1 s2)
            | otherwise = raise (MergeFailure s1 s2)
 where
   names = Map.keys (Map.intersection s1 s2)
   agree = all (\ v ->
                 let refv = TypeRef (error "Internal error, TypeInfer") Star v
                 in apply s1 refv == apply s2 refv)
               names

mostGeneralUnifier :: Type -> Type -> Infer Substitution
mostGeneralUnifier t1 t2 =
  case (t1, t2) of
    (TypeApp _ _ l r, TypeApp _ _ l' r') ->
      do s1 <- mostGeneralUnifier l l'
         s2 <- mostGeneralUnifier (apply s1 r) (apply s1 r')
         return (s2 @@ s1)
    (u@(TypeRef _ _ _), t) -> varBind u t
    (t, u@(TypeRef _ _ _)) -> varBind u t
    (TypePrim _ _ tc1, TypePrim _ _ tc2) | tc1 == tc2 -> return nullSubstitution
    (t1, t2) -> raise (UnificationError t1 t2)

varBind :: Type -> Type -> Infer Substitution
varBind (TypeRef _ k u) t
  | TypeRef _ _ u' <- t, u' == u = return nullSubstitution
  | u `elem` tv t                = raise (OccursCheckFails u t)
  | k /= kind t                  = raise (KindCheckFails u t)
  | otherwise                    = return (u ⟼  t)

match :: Type -> Type -> Infer Substitution
match t1 t2 =
  case (t1, t2) of
    (TypeApp _ _ l r, TypeApp _ _ l' r') ->
      do sl <- match l l'
         sr <- match r r'
         merge sl sr
    (TypeRef _ k u, t) | k == kind t -> return (u ⟼  t)
    (TypePrim _ _ tc1, TypePrim _ _ tc2) | tc1 == tc2 -> return nullSubstitution
    (t1, t2) -> raise (MatchFailure t1 t2)

data Scheme = Forall [Kind] Type
 
instance Types Scheme where
  apply s (Forall ks t) = Forall ks (apply s t)
  tv (Forall ks qt)     = tv qt

data Assumption = Name :>: Scheme

instance Types Assumption where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv      (i :>: sc) = tv sc

find :: Name -> [Assumption] -> Infer Scheme
find i [] = raise (UnboundIdentifier i)
find i ((i' :>: sc) : as) | i == i'   = return sc
                          | otherwise = find i as

instance Types Type where
  apply s v@(TypeRef _ _ n) = case Map.lookup n s of
                                Just t  -> t
                                Nothing -> v
  apply s (TypeApp l k t u) = TypeApp l k (apply s t) (apply s u)
  apply _ t                 = t
  --
  tv (TypeRef _ _ n)   = [n]
  tv (TypeApp _ _ t u) = tv t `union` tv u
  tv _                 = []

instance Types [Type] where
  apply s = map (apply s)
  tv      = nub . concat . map tv

getSubstitution :: Infer Substitution
getSubstitution = view istCurrentSubstitution `fmap` get

extendSubstitution :: Substitution -> Infer ()
extendSubstitution s' =
  do s <- get
     set (over istCurrentSubstitution (s' @@) s)

unify :: Type -> Type -> Infer ()
unify t1 t2 =
  do s <- getSubstitution
     u <- mostGeneralUnifier (apply s t1) (apply s t2)
     extendSubstitution u

gensym :: Kind -> Infer Type
gensym k =
  do s <- get
     set (over istNextIdentifier (+1) s)
     let num  = view istNextIdentifier s
         str  = "gensym:" ++ show num
         name = Name unknownLocation TypeEnv num (pack str)
     return (TypeRef unknownLocation k name)

data Predicate = IsIn String Type
  deriving (Eq)

inferConstant :: ConstantValue -> Infer ([Predicate], Type)
inferConstant c =
  do v <- gensym Star
     let constraint | ConstantInt    _ _ <- c = IsIn "IntLike" v
                    | ConstantChar     _ <- c = IsIn "CharLake" v
                    | ConstantString   _ <- c = IsIn "StringLike" v
                    | ConstantFloat    _ <- c = IsIn "FloatLike" v
     return ([constraint], v)

data ClassEnvironment = [Predicate] :=> Type

freshInst :: Scheme -> Infer ClassEnvironment
freshInst = undefined

inferExpression :: ClassEnvironment -> [Assumption] ->
                   Expression ->
                   Infer ([Predicate], Type)
inferExpression classEnv assumpts expr =
  case expr of
    ConstantExp  _ cv   -> inferConstant cv
    ReferenceExp _ n    -> do sc <- find n assumpts
                              (ps :=> t) <- freshInst sc
                              return (ps, t)
    LambdaExp    _ n  e -> error "FIXME, here"

infer :: Module -> Infer Module
infer = undefined

typeInfer :: Word -> Module -> Either InferenceError Module
typeInfer gensymState mdl =
  let inferM    = unInfer (infer mdl)
      excM      = runStateT (InferenceState nullSubstitution gensymState) inferM
      idM       = runExceptionT excM
      resWState = runId idM
  in fmap fst resWState

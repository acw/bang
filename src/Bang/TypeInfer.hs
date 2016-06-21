{-# LANGUAGE FlexibleInstances #-}
module Bang.TypeInfer(typeInfer)
 where

import Bang.Syntax.AST
import Data.List(union, nub, concat, intersect)

type Subst = [(Name, Type)]

nullSubst :: Subst
nullSubst  = []

(⟼) :: Name -> Type -> Subst
(⟼) n t = [(n,t)]

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
(@@) s1 s2 = [(u, apply s1 t) | (u,t) <- s2] ++ s1

merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 | agree     = return (s1 ++ s2)
            | otherwise = fail "merge failed"
 where
   agree = all (\ v ->
                 let refv = TypeReef genLoc Star v
                 in apply s1 refv == apply s2 refv)
               (map fst s1 `intersect` map fst s2)

mostGeneralUnifier :: Monad m => Type -> Type -> m Subst
mostGeneralUnifier t1 t2 =
  case (t1, t2) of
    (TypeApp _ _ l r, TypeApp l' r') ->
      do s1 <- mostGeneralUnifier l l'
         s2 <- mostGeneralUnifier (apply s1 r) (apply s1 r')
         return (s2 @@ s1)
    (TypeRef _ _ u, t) -> varBind u t
    (t, TypeRef _ _ u) -> varBind u t
    (TypePrim _ _ tc1, TypePrim _ _ tc2) | tc1 == tc2 -> return nullSubst
    (t1, t2) -> raise (UnificationError t1 t2)

varBind :: Monad m => Name -> Type -> m Subst
varBind u t | t == TypeRef _ _ u = return nullSubst
            | u `elem` tv t      = raise (OccursCheckFails u t)
            | kind u /= kind t   = raise (KindCheckFails u t)
            | otherwise          = return (u ⟼  t)

match :: Monad m => Type -> Type -> m Subst
match t1 t2 =
  case (t1, t2) of
    (TypeApp _ _ l r, TypeApp l' r') ->
      do sl <- match l l'
         sr <- match r r'
         merge sl sr
    (TypeRef _ _ u, t) | kind u == kind t -> return (u ⟼  t)
    (TypePrim tc1, TypePrim tc2) | tc1 == tc2 -> return nullSubst
    (t1, t2) -> raise (MatchFailure t1 t2)

data Assumption = Name :>: Scheme

instance Types Assumption where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv      (i :>: sc) = tv sc

find :: Monad m => Name -> [Assumption] -> m Scheme
find i [] = raise (UnboundIdentifier i)
find i ((i' :>: sc) : as) | i == i'   = return sc
                          | otherwise = find i as

class Types t where
  apply :: Subst -> t -> t
  tv    :: t          -> [Name]

instance Types Type where
  apply s v@(TypeRef _ _ n) = case lookup n s of
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

typeInfer :: Module -> Either String Module
typeInfer = undefined

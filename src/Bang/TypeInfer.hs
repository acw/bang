{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Bang.TypeInfer(runTypeInference)
 where

import           Bang.AST(Module, moduleDeclarations)
import           Bang.AST.Declaration(Declaration(..), ValueDeclaration,
                                      vdName, vdDeclaredType, vdValue,
                                      tdName, tdType)
import           Bang.AST.Expression(Expression(..), ConstantValue(..),
                                     lambdaArgumentNames, lambdaBody,
                                     constLocation, constValue, refName)
import           Bang.AST.Name(Name, NameEnvironment(..),
                               nameLocation, nameText, ppName)
import           Bang.AST.Type(Type(..), ppType, rtName, ftArgumentType,
                               ftResultType, taLeftType, taRightType,
                               mkPrimType, mkFunType, mkTypeRef,
                               Kind(..))
import           Bang.Monad(Compiler, BangError(..), BangWarning(..),
                            registerNewName, err', err, warn,
                            getPassState, mapPassState, runPass)
import           Bang.Syntax.Location(Location, fakeLocation)
import           Bang.Utils.FreeVars(CanHaveFreeVars(..))
import           Bang.Utils.Pretty(BangDoc, text')
import           Control.Lens(view, over)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Set(Set, (\\))
import qualified Data.Set as Set
import           Text.PrettyPrint.Annotated(text, nest, quotes, ($+$), (<+>))

runTypeInference :: Module -> Compiler ps Module
runTypeInference x =
  do _ <- runPass emptyEnvironment (mapM_ typeInferDecls (view moduleDeclarations x))
     return x

-- -----------------------------------------------------------------------------

type Infer a = Compiler TypeEnvironment a

getNamesTypeScheme :: Name -> Infer (Maybe Scheme)
getNamesTypeScheme n = Map.lookup n `fmap` getPassState

addToTypeEnvironment :: [Name] -> [Scheme] -> Infer ()
addToTypeEnvironment ns schms = mapPassState (add ns schms)
 where
  add :: [Name] -> [Scheme] -> TypeEnvironment -> TypeEnvironment
  add []         []         acc = acc
  add (n:restns) (s:rschms) acc =
    Map.insertWithKey errorFn n s (add restns rschms acc)
  add _          _          _   =
    error "Wackiness has insued."
  --
  errorFn k _ _ = error ("Redefinition of " ++ show k)

-- -----------------------------------------------------------------------------

type Substitution = Map Name Type

nullSubstitution :: Substitution
nullSubstitution = Map.empty

composeSubstitutions :: Substitution -> Substitution -> Substitution
composeSubstitutions s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class ApplySubst t where
  apply :: Substitution -> t -> t

instance ApplySubst Type where
  apply s (TypeUnit t) = TypeUnit t
  apply s (TypePrim t) = TypePrim t
  apply s (TypeRef t)  = case Map.lookup (view rtName t) s of
                           Nothing -> TypeRef t
                           Just t' -> t'
  apply s (TypeFun t)  = TypeFun (over ftArgumentType (apply s) $
                                  over ftResultType   (apply s) t)
  apply s (TypeApp t)  = TypeApp (over taLeftType     (apply s) $
                                  over taRightType    (apply s) t)

instance ApplySubst a => ApplySubst [a] where
  apply s = map (apply s)

-- -----------------------------------------------------------------------------

data Scheme = Scheme [Name] Type

instance CanHaveFreeVars Scheme where
  freeVariables (Scheme ns t) = freeVariables t \\ Set.fromList ns

instance ApplySubst Scheme where
  apply s (Scheme vars t) = Scheme vars (apply s t)

newTypeVar :: Name -> Infer Type
newTypeVar n =
  do let loc = view nameLocation n
     n' <- registerNewName TypeEnv (view nameText n)
     return (mkTypeRef loc Unknown n') 

instantiate :: Scheme -> Infer Type
instantiate (Scheme vars t) =
  do refs <- mapM newTypeVar vars
     let newSubsts = Map.fromList (zip vars refs)
     return (apply newSubsts t)

-- -----------------------------------------------------------------------------

mostGeneralUnifier :: Type -> Type -> Infer Substitution
mostGeneralUnifier a b =
  case (a, b) of
    (TypeUnit _,  TypeUnit _)  -> return nullSubstitution
    (TypePrim _,  TypePrim _)  -> return nullSubstitution
    (TypeRef  t1, t2)          -> varBind (view rtName t1) t2
    (t2,          TypeRef t1)  -> varBind (view rtName t1) t2
    (TypeFun  t1, TypeFun t2)  -> do let at1 = view ftArgumentType t1
                                         at2 = view ftArgumentType t2
                                     s1 <- mostGeneralUnifier at1 at2
                                     let rt1 = apply s1 (view ftResultType t1)
                                         rt2 = apply s1 (view ftResultType t2)
                                     s2 <- mostGeneralUnifier rt1 rt2
                                     return (s1 `composeSubstitutions` s2)
    (TypeApp t1,  TypeApp t2)  -> do let lt1 = view taLeftType t1
                                         lt2 = view taLeftType t2
                                     s1 <- mostGeneralUnifier lt1 lt2
                                     let rt1 = apply s1 (view taRightType t1)
                                         rt2 = apply s1 (view taRightType t2)
                                     s2 <- mostGeneralUnifier rt1 rt2
                                     return (s1 `composeSubstitutions` s2)
    _                          -> do err' (TypesDontUnify a b)
                                     return nullSubstitution

varBind :: Name -> Type -> Infer Substitution
varBind u t | TypeRef t' <- t,
              view rtName t' == u            = return nullSubstitution
            | u `Set.member` freeVariables t = do err' (OccursFail u t)
                                                  return nullSubstitution
            | otherwise                      = return (Map.singleton u t)

-- -----------------------------------------------------------------------------

type TypeEnvironment = Map Name Scheme

emptyEnvironment :: TypeEnvironment
emptyEnvironment = Map.empty

instance ApplySubst TypeEnvironment where
  apply s tenv = Map.map (apply s) tenv

instance CanHaveFreeVars TypeEnvironment where
  freeVariables tenv = freeVariables (Map.elems tenv)

generalize :: TypeEnvironment -> Type -> Scheme
generalize env t = Scheme vars t
 where vars = Set.toList (freeVariables t \\ freeVariables env)

-- -----------------------------------------------------------------------------

data InferenceError = InternalError
                    | TypesDontUnify  Type Type
                    | OccursFail      Name Type
                    | UnboundVariable Name

instance BangError InferenceError where
  ppError = prettyError

prettyError :: InferenceError -> (Maybe Location, BangDoc)
prettyError e =
  case e of
    InternalError        ->
      (Nothing, text "<internal error>")
    TypesDontUnify t1 t2 ->
      (Nothing, text "Types don't unify:" $+$
                  (nest 3 
                     (text "first type: "  <+> ppType t1 $+$
                      text "second type: " <+> ppType t2)))
    OccursFail n t ->
      (Just (view nameLocation n),
       text "Occurs check failed:" $+$
         (nest 3 (text "Type: " <+> ppType t)))
    UnboundVariable n ->
      (Just (view nameLocation n),
       text "Unbound variable (in type checker?):" <+> ppName n)

data InferenceWarning = TopLevelWithoutType Name Type
                      | DeclarationMismatch Name Type Type

instance BangWarning InferenceWarning where
  ppWarning = prettyWarning

prettyWarning :: InferenceWarning -> (Maybe Location, BangDoc)
prettyWarning w =
  case w of
    TopLevelWithoutType n t ->
      (Just (view nameLocation n),
       text "Variable" <+> quotes (text' (view nameText n)) <+>
       text "is defined without a type." $+$
       text "Inferred type:" $+$ nest 3 (ppType t))
    DeclarationMismatch n dt it ->
      (Just (view nameLocation n),
       text "Mismatch between declared and inferred type of" <+>
       quotes (text' (view nameText n)) $+$
       nest 3 (text "declared type:" <+> ppType dt $+$
               text "inferred type:" <+> ppType it))

-- -----------------------------------------------------------------------------

-- Infer the type of a group of declarations with cyclic dependencies.
typeInferDecls :: [Declaration] -> Infer ()
typeInferDecls decls =
  do (names, schemes, decls') <- getInitialSchemes decls
     addToTypeEnvironment names schemes
     mapM_ typeInferDecl decls'
 where
  getInitialSchemes [] =
    return ([], [], [])
  getInitialSchemes ((DeclType td) : rest) =
    do (rn, rs, rd) <- getInitialSchemes rest
       let n = view tdName td 
           s = Scheme [] (view tdType td)
       return (n:rn, s:rs, rd)
  getInitialSchemes ((DeclVal td) : rest) =
    do (rn, rs, rd) <- getInitialSchemes rest
       return (rn, rs, (td : rd))

typeInferDecl :: ValueDeclaration -> Infer ()
typeInferDecl vd =
  do (subs, t) <- typeInferExpr (view vdValue vd)
     let t' = apply subs t
     case view vdDeclaredType vd of
       Nothing ->
         warn (TopLevelWithoutType (view vdName vd) t')
       Just dt ->
         warn (DeclarationMismatch (view vdName vd) dt t)

typeInferConst :: Location -> ConstantValue ->
                  Infer (Substitution, Type)
typeInferConst l (ConstantInt _ _) =
  return (nullSubstitution, mkPrimType l "i64")
typeInferConst l (ConstantChar _) = 
  return (nullSubstitution, mkPrimType l "i8") -- FIXME
typeInferConst l (ConstantString _) =
  return (nullSubstitution, mkPrimType l "i8*") -- FIXME
typeInferConst l (ConstantFloat _) =
  return (nullSubstitution, mkPrimType l "double")

typeInferExpr :: Expression -> Infer (Substitution, Type)
typeInferExpr expr =
  case expr of
    ConstExp e ->
      typeInferConst (view constLocation e) (view constValue e)
    RefExp e ->
      do mscheme <- getNamesTypeScheme (view refName e)
         case mscheme of
           Nothing     -> err (UnboundVariable (view refName e))
           Just scheme -> do t <- instantiate scheme
                             return (nullSubstitution, t)
    LambdaExp e ->
      do let argNames = view lambdaArgumentNames e
         tvars <- mapM newTypeVar argNames
         let tvars'   = map (Scheme []) tvars
         addToTypeEnvironment argNames tvars'
         (s1, t1) <- typeInferExpr (view lambdaBody e)
         return (s1, mkFunType' (apply s1 tvars) t1) 
 where
  mkFunType' []       t = t
  mkFunType' (x:rest) t = mkFunType fakeLocation x (mkFunType' rest t)



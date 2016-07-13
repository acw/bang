{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Bang.TypeInfer(runTypeInference)
 where

import Bang.AST(Module)
import Bang.Monad(Compiler)
import Bang.Syntax.ParserMonad(NameDatabase)

runTypeInference :: NameDatabase -> Module -> Compiler ps Module
runTypeInference _ x = return x

{- Better version 
import           Bang.Monad(Compiler, BangError(..), err,
                            runPass, getPassState, setPassState,
                            viewPassState, overPassState,
                            registerNewName, genName)
import           Bang.Syntax.Location(Location, unknownLocation)
import           Bang.Syntax.ParserMonad(NameDatabase(..))
import           Bang.Utils.Pretty(BangDoc)
import           Control.Lens(set, view, over)
import           Control.Lens.TH(makeLenses)
import           Data.List(union, nub, concat)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Text.Lazy(Text, pack)
import           Text.PrettyPrint.Annotated(text, (<+>), quotes)

data InferenceState = InferenceState {
       _nameDatabase :: NameDatabase
     }

makeLenses ''InferenceState

type Infer a = Compiler InferenceState a

runInfer :: NameDatabase -> Infer a -> Compiler ps a
runInfer ndb action = snd `fmap` runPass initial action
 where initial = InferenceState ndb

-- -----------------------------------------------------------------------------

data InferError = UnboundVariable Location Name
 deriving (Show)

instance BangError InferError where
  ppError = prettyError

prettyError :: InferError -> (Maybe Location, BangDoc)
prettyError e =
  case e of
    UnboundVariable l n ->
      (Just l, text "Unbound variable '" <+> quotes (ppName n))

-- -----------------------------------------------------------------------------

type Substitutions = Map Name Type

noSubstitutions :: Substitutions
noSubstitutions  = Map.empty

composeSubstitutions :: Substitutions -> Substitutions -> Substitutions
composeSubstitutions s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Types a where
  freeTypeVariables :: a -> Set Name
  apply             :: Substitutions -> a -> a

instance Types Type where
  freeTypeVariables t =
    case t of
      TypeUnit   _ _      -> Set.empty
      TypePrim   _ _ _    -> Set.empty
      TypeRef    _ _ n    -> Set.singleton n
      TypeLambda _ _ ns e -> Set.unions (map freeTypeVariables ns) `Set.union`
                             freeTypeVariables e
      TypeApp    _ _ a  b -> freeTypeVariables a `Set.union` freeTypeVariables b
  apply substs t =
    case t of
      TypeRef    _ _ n    -> case Map.lookup n substs of
                             Nothing -> t
                             Just t' -> t'
      TypeLambda l k ns e -> TypeLambda l k (apply substs ns) (apply substs e)
      TypeApp    l k a  b -> TypeApp l k (apply substs a) (apply substs b)
      _                   -> t

instance Types a => Types [a] where
  freeTypeVariables l = Set.unions (map freeTypeVariables l)
  apply             s = map (apply s)

-- -----------------------------------------------------------------------------

inferModule :: Module -> Infer Module
inferModule = undefined

runTypeInference :: NameDatabase -> Module -> Compiler ps Module
runTypeInference ndb mod = runInfer ndb (inferModule mod)
-}
-- data Scheme = Scheme [Name] Type
-- 
-- getName :: NameEnvironment -> Text -> Infer Name
-- getName env nameText =
--   do namedb <- viewPassState nameDatabase
--      let key = (env, nameText)
--      case Map.lookup key namedb of
--        Nothing ->
--          do name <- registerNewName env nameText
--             overPassState (set nameDatabase (Map.insert key name namedb))
--             return name
--        Just name ->
--          return name
-- 
-- runTypeInference :: NameDatabase -> Module -> Compiler ps Module
-- runTypeInference nameDB mod =
--   snd `fmap` (runPass initialState (inferModule mod))
--  where initialState = InferenceState nameDB
-- 
-- type Substitutions = Map Name Type
-- 
-- nullSubst :: Substitutions
-- nullSubst = Map.empty
-- 
-- type TypeEnv = Map Name Scheme
-- 
-- class Substitutable a where
--   apply :: Substitutions -> a -> Type
-- 
-- instance Substitutable Type where
--   apply subs t =
--     case t of
--       TypeUnit _ _   -> t
--       TypePrim _ _ _ -> t
--       TypeRef  _ _ n -> case Map.lookup n subs of
--                           Nothing -> t
--                           Just t' -> t'
--       TypeLambda l k ats bt ->
--         TypeLambda l k (map (apply subs) ats) (apply subs bt)
--       TypeApp l k a b ->
--         TypeApp l k (apply subs a) (apply subs b)
--       TypeForAll ns t ->
--         TypeForAll ns (apply subs t)
-- 
-- instance Substitutable Name where
--   apply subs n =
--     case Map.lookup n subs of
--       Nothing               -> TypeRef unknownLocation Star n
--       Just t                -> t
-- 
-- instantiate :: Scheme -> Infer Type
-- instantiate =
--   do 
-- 
-- inferExpression :: TypeEnv -> Expression ->
--                    Infer (Substitutions, Type)
-- inferExpression typeEnv expr =
--   case expr of
--     ConstantExp  s cv   -> do memName <- getName TypeEnv "Memory"
--                               return (nullSubst, TypeRef s Star memName)
--     ReferenceExp s n    -> case Map.lookup n typeEnv of
--                              Nothing -> err (UnboundVariable s n)
--                              Just t  -> do t' <- instantiate t
--                                            return (nullSubst, t')
--     LambdaExp    s ns e -> do localTypeNames <- mapM (const (genName TypeEnv)) ns
--                               let localSchemes = map (Scheme [] . TypeRef s Star) localTypeNames
--                                   localEnv     = Map.fromList (zip ns localSchemes)
--                                   typeEnv'     = typeEnv `Map.union` localEnv
--                               (s1, t1) <- inferExpression typeEnv' e
--                               return (s1, TypeLambda s (Star `KindArrow` Star)
--                                                     (map (apply s1) localTypeNames)
--                                                     t1)
-- 
-- inferModule :: Module -> Infer Module
-- inferModule = undefined

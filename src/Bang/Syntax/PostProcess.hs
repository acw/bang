{-# LANGUAGE RankNTypes #-}
module Bang.Syntax.PostProcess(
         runPostProcessor
       )
 where

import           Bang.AST(Name, Module, moduleDeclarations, ppName,
                          nameText, nameLocation, nameEnvironment)
import           Bang.AST.Declaration(Declaration(..), declName,
                                      tdName, tdType,
                                      ValueDeclaration, vdName, vdLocation,
                                      vdDeclaredType, vdValue)
import           Bang.AST.Expression(Expression(..), isEmptyExpression, refName,
                                     lambdaArgumentNames, lambdaBody,
                                     isEmptyExpression)
import           Bang.AST.Type(Type(..), rtName, ftArgumentTypes, ftResultType,
                               taLeftType, taRightType)
import           Bang.Monad(Compiler, BangError(..), err, err', registerName)
import           Bang.Syntax.Location(Location, ppLocation)
import           Bang.Utils.FreeVars(CanHaveFreeVars(..))
import           Bang.Utils.Pretty(BangDoc, text')
import           Control.Lens(Lens', view, set)
import           Control.Monad(foldM)
import           Data.Char(isLower)
import           Data.Graph(SCC(..))
import           Data.Graph.SCC(stronglyConnComp)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Text.Lazy(uncons)
import           Text.PrettyPrint.Annotated(text, ($+$), (<+>), nest, quotes)

data PostProcessError = InternalError        Name
                      | UndefinedVariable    Name
                      | RedefinitionError    Name Location Location
                      | TypeDeclWithoutValue Name Location
 deriving (Show)

instance BangError PostProcessError where
  ppError = prettyError

prettyError :: PostProcessError -> (Maybe Location, BangDoc)
prettyError e =
  case e of
    InternalError n ->
      (Nothing, text "Serious post-processing error w.r.t. " <+> ppName n)
    UndefinedVariable n ->
      (Just (view nameLocation n), text "Undefined variable " <+> quotes (text' (view nameText n)))
    RedefinitionError n l1 l2 ->
      (Just l1, text "Name" <+> ppName n <+> text "redefined." $+$
                nest 2 (text "original definiton at " <+> ppLocation l2))
    TypeDeclWithoutValue n l ->
      (Just l, text "Type declaration provided, but no value provided." $+$
               nest 2 (text "variable name: " <+> ppName n))

runPostProcessor :: Module -> Compiler ps Module
runPostProcessor mdl =
  do let baseDecls = concat (view moduleDeclarations mdl)
     decls     <- linkNames baseDecls
     declTable <- makeDeclarationTable decls
     decls'    <- combineTypeValueDeclarations declTable decls
     return (set moduleDeclarations (orderDecls decls') mdl)

-- -----------------------------------------------------------------------------

linkNames :: [Declaration] -> Compiler ps [Declaration]
linkNames decls =
  do declaredNames <- foldM addNewNames Map.empty (map (view declName) decls)
     mapM (linkDecls declaredNames) decls
 where
  addNewNames t n =
    do n' <- registerName n
       let key = (view nameText n, view nameEnvironment n)
       return (Map.insert key n' t)
  --
  replaceName nameMap name =
    do let key = (view nameText name, view nameEnvironment name)
       case Map.lookup key nameMap of
         Nothing    -> err' (UndefinedVariable name) >> return name
         Just name' -> return name'
  --
  addOrReplaceName nameMap name =
    do let key  = (view nameText name, view nameEnvironment name)
       case Map.lookup key nameMap of
         Nothing | couldBeTypeVariable name ->
           do name' <- registerName name
              return (name', Map.insert key name' nameMap)
         Nothing ->
           err' (UndefinedVariable name) >> return (name, nameMap)
         Just name' ->
           return (name', nameMap)
  --
  couldBeTypeVariable n =
    case uncons (view nameText n) of
      Nothing ->
        error "Empty variable name?"
      Just (x,_) ->
        isLower x
  --
  linkDecls nameMap (DeclType td) =
    do td'  <- overM tdType (linkType' nameMap)    td
       td'' <- overM tdName (replaceName nameMap)  td'
       return (DeclType td'')
  linkDecls nameMap (DeclVal vd) =
    do vd'   <- overM vdDeclaredType (traverse (linkType' nameMap)) vd
       vd''  <- overM vdValue        (linkExpr nameMap)             vd'
       vd''' <- overM vdName         (replaceName nameMap)          vd''
       return (DeclVal vd''')
  --
  linkType' nm t = fst `fmap` linkType nm t
  --
  linkType nameMap x@(TypeUnit _) = return (x, nameMap)
  linkType nameMap x@(TypePrim _) = return (x, nameMap)
  linkType nameMap   (TypeRef t)  =
    do (name, nameMap') <- addOrReplaceName nameMap (view rtName t)
       let t' = set rtName name t
       return (TypeRef t', nameMap') 
  linkType nameMap   (TypeFun t)  =
    do (argTypes, nameMap')  <- foldM linkTypes ([], nameMap) (view ftArgumentTypes t)
       (resType,  nameMap'') <- linkType nameMap' (view ftResultType t)
       return (TypeFun (set ftArgumentTypes argTypes $
                        set ftResultType    resType  t),
               nameMap'')
  linkType nameMap   (TypeApp t)  =
    do (lt, nameMap')  <- linkType nameMap  (view taLeftType  t)
       (rt, nameMap'') <- linkType nameMap' (view taRightType t)
       return (TypeApp (set taLeftType lt (set taRightType rt t)), nameMap'')
  --
  linkTypes (acc, nameMap) argType =
    do (argType', nameMap') <- linkType nameMap argType
       return (acc ++ [argType'], nameMap')
  -- 
  linkExpr _ x | isEmptyExpression x = return x
  linkExpr _ x@(ConstExp  _) = return x
  linkExpr nameMap (RefExp    e) =
    RefExp `fmap` overM refName (replaceName nameMap) e
  linkExpr nameMap (LambdaExp e) =
    do let names = view lambdaArgumentNames e
       nameMap' <- foldM addNewNames nameMap names
       e'  <- overM lambdaArgumentNames (mapM (replaceName nameMap')) e
       e'' <- overM lambdaBody          (linkExpr nameMap')           e'
       return (LambdaExp e'')

overM :: Monad m => Lens' s a -> (a -> m a) -> s -> m s
overM field action input =
  do newval <- action (view field input)
     return (set field newval input)

-- -----------------------------------------------------------------------------

type DeclarationTable = Map Name (Maybe (Type, Location), Maybe ValueDeclaration)

makeDeclarationTable :: [Declaration] -> Compiler ps DeclarationTable
makeDeclarationTable decls = foldM combine Map.empty decls
 where
  combine table d =
    do let name = view declName d
       case d of
         DeclType _ ->
           return table
         DeclVal vd | Just t <- view vdDeclaredType vd,
                      isEmptyExpression (view vdValue vd) ->
           do let myLoc = view vdLocation vd
                  myVal = Just (t, myLoc)
              case Map.lookup name table of
                Nothing            ->
                  return (Map.insert name (myVal, Nothing) table)
                Just (Nothing, vd') ->
                  return (Map.insert name (myVal, vd') table)
                Just (Just (_, theirLoc), _) ->
                  err (RedefinitionError name myLoc theirLoc)
         DeclVal vd | Just _ <- view vdDeclaredType vd ->
           err (InternalError name)
         DeclVal vd | isEmptyExpression (view vdValue vd) ->
           err (InternalError name)
         DeclVal vd ->
           case Map.lookup name table of
             Nothing            ->
               return (Map.insert name (Nothing, Just vd) table)
             Just (td, Nothing) ->
               return (Map.insert name (td, Just vd) table)
             Just (_, Just vd') ->
               do let newLoc  = view vdLocation vd
                      origLoc = view vdLocation vd'
                  err (RedefinitionError name newLoc origLoc)

-- -----------------------------------------------------------------------------

combineTypeValueDeclarations :: DeclarationTable ->
                                [Declaration] ->
                                Compiler ps [Declaration]
combineTypeValueDeclarations table decls = process decls
 where
  process [] = return []
  process (x:rest) =
    case x of
      DeclType _ ->
        (x:) `fmap` process rest
      DeclVal vd | Just _ <- view vdDeclaredType vd,
                   isEmptyExpression (view vdValue vd) ->
        process rest
      DeclVal vd ->
        case Map.lookup (view vdName vd) table of
          Nothing ->
            err (InternalError (view vdName vd))
          Just (Nothing, _) ->
            (x:) `fmap` process rest
          Just (Just (t, _), _) ->
            do let vd' = set vdDeclaredType (Just t) vd
               (DeclVal vd' :) `fmap` process rest

-- -----------------------------------------------------------------------------

orderDecls :: [Declaration] -> [[Declaration]]
orderDecls decls = map unSCC (stronglyConnComp nodes)
 where
  unSCC (AcyclicSCC x) = [x]
  unSCC (CyclicSCC xs) = xs
  --
  nodes = map tuplify decls
  tuplify d = (d, view declName d, freeVariables d)

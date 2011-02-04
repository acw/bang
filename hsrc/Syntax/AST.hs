module Syntax.AST where

import Syntax.ParserCore

data Show a => Module a = Module {
    modName    :: QualifiedName
  , modImports :: [Import]
  , modDecls   :: [Decl a]
  }
 deriving (Show)

data QualifiedName = QualifiedName {
    qnPrefixes :: [String]
  , qnName     :: String
  }
 deriving (Show)

gensym :: Parser QualifiedName
gensym = do
  name <- genstr
  return (QualifiedName [] name)

data Import = Import {
    imName      :: QualifiedName
  , imQualified :: Bool
  , imList      :: Maybe [ImportName]
  , imAs        :: Maybe QualifiedName
  }
 deriving (Show)

data ImportName = ImportNamed QualifiedName
                | ImportRenamed QualifiedName QualifiedName
 deriving (Show)

data Show a => Decl a =
    DeclData a [Type] QualifiedName [QualifiedName] [DataClause a]
  | DeclType a [Type]
  | DeclNewtype a [Type]
  | DeclClass a [Type] QualifiedName [QualifiedName] [ClassClause a]
  | DeclInstance a [Type]
  | DeclValue a [Type] Type QualifiedName (Expr a)
  | DeclExport a (Decl a)
 deriving (Show)

addTypeRestrictions :: Show a => [Type] -> Decl a -> Decl a
addTypeRestrictions rs (DeclData     s _ a b c)   = DeclData s rs a b c
addTypeRestrictions rs (DeclType     s _)         = DeclType s rs
addTypeRestrictions rs (DeclNewtype  s _)         = DeclNewtype s rs
addTypeRestrictions rs (DeclClass    s _ a b c)   = DeclClass s rs a b c
addTypeRestrictions rs (DeclInstance s _)         = DeclInstance s rs
addTypeRestrictions rs (DeclValue    s _ n a b)   = DeclValue s rs n a b
addTypeRestrictions rs (DeclExport   s d)         =
  DeclExport s (addTypeRestrictions rs d)

data DataClause a = DataClause a QualifiedName [Maybe QualifiedName] [Type]
 deriving (Show)

data ClassClause a = ClassClause a QualifiedName Type (Maybe (Expr a))
 deriving (Show)

data Show a => Expr a =
    Const  a ConstVal
  | VarRef a QualifiedName
  | Cond a (Expr a) (Expr a) (Expr a)
  | App a (Expr a) [Expr a]
  | Block a [Stmt a]
  | Lambda a [QualifiedName] (Expr a)
  | Let a Type QualifiedName (Expr a) (Expr a)
 deriving (Show)

getSpecial :: Show a => Expr a -> a
getSpecial (Const a _)     = a
getSpecial (VarRef a _)    = a
getSpecial (Cond a _ _ _)  = a
getSpecial (App a _ _)     = a
getSpecial (Block a _)     = a
getSpecial (Lambda a _ _)  = a
getSpecial (Let a _ _ _ _) = a

data Show a => Stmt a =
    SExpr a (Expr a)
  | SBind a QualifiedName (Stmt a)
  | SLet  a Type QualifiedName (Expr a)
  | SCase a (Expr a) [(Pattern,Maybe (Expr a),Stmt a)]
 deriving (Show)

data Pattern =
    ListNull
  | PConst ConstVal
  | PVar QualifiedName
  | PNamed QualifiedName Pattern
  | PAp Pattern Pattern
 deriving (Show)

data Kind = Star | KFun Kind Kind
 deriving (Eq,Show)

data Type = TVar QualifiedName Kind
          | TCon QualifiedName Kind
          | TAp Type Type
          | TGen Int
 deriving (Show)

data ConstVal = ConstInteger Int String
              | ConstFloat String
              | ConstChar String
              | ConstString String
              | ConstEmpty
 deriving (Show)

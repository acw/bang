module Syntax.AST where

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
    DeclData a [Type] QualifiedName [QualifiedName] [DataClause]
  | DeclType a [Type]
  | DeclNewtype a [Type]
  | DeclClass a [Type] 
  | DeclInstance a [Type]
  | DeclValue a [Type] Type QualifiedName [Stmt a]
  | DeclExport a (Decl a)
 deriving (Show)

addTypeRestrictions :: Show a => [Type] -> Decl a -> Decl a
addTypeRestrictions rs (DeclData     s _ a b c) = DeclData s rs a b c
addTypeRestrictions rs (DeclType     s _)       = DeclType s rs
addTypeRestrictions rs (DeclNewtype  s _)       = DeclNewtype s rs
addTypeRestrictions rs (DeclClass    s _)       = DeclClass s rs
addTypeRestrictions rs (DeclInstance s _)       = DeclInstance s rs
addTypeRestrictions rs (DeclValue    s _ a b c) = DeclValue s rs a b c
addTypeRestrictions rs (DeclExport   s d)     =
  DeclExport s (addTypeRestrictions rs d)

data DataClause = DataClause QualifiedName [Type]
 deriving (Show)

data Show a => Expr a =
    Const  a ConstVal
  | VarRef a QualifiedName
  | Cond a (Expr a) (Expr a)
  | App a (Expr a) (Expr a)
  | Block a [Expr a]
  | Lambda a [QualifiedName] (Expr a)
 deriving (Show)

data Show a => Stmt a =
    SExpr a (Expr a)
  | SBind a QualifiedName (Stmt a)
  | SCase a
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

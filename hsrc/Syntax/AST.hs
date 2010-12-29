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
    DeclData [Type] QualifiedName [QualifiedName] [DataClause]
  | DeclType
  | DeclNewtype
  | DeclClass
  | DeclInstance
  | DeclValue Type QualifiedName (Expr a)
 deriving (Show)

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

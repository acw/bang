{
{-# OPTIONS_GHC -w #-}

-- vim: filetype=haskell

module Syntax.Parser where

import Syntax.AST
import Syntax.Lexer
import Syntax.ParserCore

import MonadLib
import qualified Codec.Binary.UTF8.Generic as UTF8

}

%token

-- reserved words
  'module'    { Lexeme $$ (TokValIdent "module"  ) }
  'export'    { Lexeme $$ (TokValIdent "export"  ) }
  'import'    { Lexeme $$ (TokValIdent "import"  ) }
  'data'      { Lexeme $$ (TokValIdent "data"    ) }
  'type'      { Lexeme $$ (TokValIdent "type"    ) }
  'newtype'   { Lexeme $$ (TokValIdent "newtype" ) }
  'class'     { Lexeme $$ (TokValIdent "class"   ) }
  'instance'  { Lexeme $$ (TokValIdent "instance") }
  'qualified' { Lexeme $$ (TokValIdent "instance") }
  'as'        { Lexeme $$ (TokValIdent "instance") }

-- symbols
  '='        { Lexeme $$ (TokOpIdent  "=")        }
  '->'       { Lexeme $$ (TokOpIdent  "->")       }
  '=>'       { Lexeme $$ (TokOpIdent  "=>")       }
  '\\'       { Lexeme $$ (TokOpIdent  "\\")       }
  '('        { Lexeme $$ LParen  }
  ')'        { Lexeme $$ RParen  }
  '['        { Lexeme $$ LSquare }
  ']'        { Lexeme $$ RSquare }
  '{'        { Lexeme $$ LBrace  }
  '}'        { Lexeme $$ RBrace  }
  '|'        { Lexeme $$ Bar     }
  ';'        { Lexeme $$ Semi    }
  ','        { Lexeme $$ Comma   }

-- identifiers
  TYPE_IDENT { Lexeme _ (TokTypeIdent $$) }
  VAL_IDENT  { Lexeme _ (TokValIdent $$) }
  OP_IDENT   { Lexeme _ (TokOpIdent $$) }

-- values
  INTVAL     { Lexeme _ (TokInt $$) }
  FLOATVAL   { Lexeme _ (TokFloat $$) }
  CHARVAL    { Lexeme _ (TokChar $$) }
  STRVAL     { Lexeme _ (TokString $$) }

%monad { Parser } { (>>=) } { return }
%name parseModule top_module
%tokentype { Lexeme }

%lexer { lexer } { Lexeme initPosition TokEOF }

%%

top_module :: { Module () } : 'module' TYPE_IDENT module_decls {
    let (imports,items) = $3
    in Module (makeQualified $2) imports items
  }

module_decls :: { ([Import], [Decl ()]) }
  : module_decls module_decl { $1 `pappend` $2 }
  | module_decl              { $1              }

module_decl :: { ([Import], [Decl ()]) }
  : data_decl     { ([],   [$1]) }
  | type_decl     { ([],   [$1]) }
  | newtype_decl  { ([],   [$1]) }
  | class_decl    { ([],   [$1]) }
  | instance_decl { ([],   [$1]) }
  | value_decl    { ([],   [$1]) }
  | import_decl   { ([$1], []  ) } 

-- Data Declarations --------------------------------------------------------

data_decl :: { Decl () }
  : 'data' mqualifiers TYPE_IDENT data_args dataclauses
  { DeclData $2 (makeQualified $3) $4 $5 }

mqualifiers :: { [Type] }
  :                          { [] }
  | '(' tqualifiers ')' '=>' { $2 }

tqualifiers :: { [Type] }
  : tqualifier                 { [$1] }
  | tqualifiers ',' tqualifier { $1 ++ [$3] }

tqualifier :: { Type }
  : TYPE_IDENT VAL_IDENT
    { TAp (TCon (makeQualified $1) Star) (TVar (makeQualified $2) Star) }
  | tqualifier VAL_IDENT
    { TAp $1 (TVar (makeQualified $2) Star) }

data_args :: { [QualifiedName] }
  :                            { [] }
  | data_args VAL_IDENT        { $1 ++ [makeQualified $2] }

dataclauses :: { [DataClause] }
  : '=' dataclause             { [$2] }
  | dataclauses '|' dataclause { $1 ++ [$3] }

dataclause :: { DataClause }
  : TYPE_IDENT                 { DataClause (makeQualified $1) [] }
  | dataclause bangtype        { let DataClause name items = $1
                                 in DataClause name (items ++ [$2]) }

-- Type alias Declarations --------------------------------------------------

type_decl :: { Decl () }
  : 'type'        { DeclType }

newtype_decl :: { Decl () }
  : 'newtype'     { DeclNewtype }

class_decl :: { Decl () }
  : 'class'       { DeclClass }

instance_decl :: { Decl () }
  : 'instance'    { DeclInstance }

value_decl :: { Decl () }
  : VAL_IDENT '=' expr { DeclValue undefined (makeQualified $1) $3 }

import_decl :: { Import }
  : 'import' mqualified TYPE_IDENT mimport_list mas
    { Import (makeQualified $3) $2 $4 $5 }

mqualified :: { Bool }
  :             { False }
  | 'qualified' { True  }

mimport_list :: { Maybe [ImportName] }
  :                     { Nothing }
  | '(' ')'             { Just [] }
  | '(' import_list ')' { Just $2 }

mas :: { Maybe QualifiedName }
  :                     { Nothing }
  | 'as' TYPE_IDENT     { Just (makeQualified $2) }

import_list :: { [ImportName] }
  : import_name                 { [$1] }
  | import_list ',' import_name { $1 ++ [$3] }

import_name :: { ImportName }
  : either_ident                   { ImportNamed $1 }
  | either_ident 'as' either_ident { ImportRenamed $1 $3 }

either_ident :: { QualifiedName }
  : TYPE_IDENT          { makeQualified $1 }
  | VAL_IDENT           { makeQualified $1 }

-- Types --------------------------------------------------------------------

bangtype :: { Type }
  : bangtype1            { $1 }

bangtype1 :: { Type }
  : bangtype1 VAL_IDENT  { TAp $1 (TVar (makeQualified $2) Star) }
  | bangtype2            { $1 }

bangtype2 :: { Type }
  : bangtype2 '->' bangtype3
    { TAp (TAp (TCon (QualifiedName [] "->") Star) $1) $3 }
  | bangtype3            { $1 }

bangtype3 :: { Type }
  : TYPE_IDENT             { TVar (makeQualified $1) Star }
  | '(' bangtype ')'       { $2 }

-- Expressions --------------------------------------------------------------

expr :: { Expr () }
  : '\\' arglist '->' expr1 { Lambda () $2 $4 }
  | expr1                   { $1 }

arglist :: { [QualifiedName] }
  : VAL_IDENT           { [makeQualified $1] }
  | arglist VAL_IDENT   { $1 ++ [makeQualified $2] }

expr1 :: { Expr () }
  : '{' exprs '}'       { Block () $2 }
  | expr2               { $1 }

exprs :: { [Expr ()] }
  : expr ';'            { [$1] }
  | exprs expr ';'      { $1 ++ [$2] }

expr2 :: { Expr () }
  : '[' list_exprs ']'  { unwindList $2 }
  | expr3               { $1 }

list_exprs :: { [Expr ()] }
  :                      { [] }
  | list_exprs ',' expr3 { $1 ++ [$3] }

expr3 :: { Expr () }
  : bottom_expr         { $1 }

bottom_expr :: { Expr () }
  : INTVAL              { let (b,v) = $1 in Const () (ConstInteger b v) }
  | FLOATVAL            { Const () (ConstFloat $1) }
  | CHARVAL             { Const () (ConstChar $1) }
  | STRVAL              { Const () (ConstString  $1) }
  | VAL_IDENT           { VarRef () (makeQualified $1) }
  | '(' expr ')'        { $2 }

{
lexer :: (Lexeme -> Parser a) -> Parser a
lexer k = scan >>= k

happyError :: Parser a
happyError  = raiseP "Parse Error"

pappend :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
pappend (a,b) (c,d) = (a++c,b++d)

unwindList :: [Expr ()] -> Expr ()
unwindList [] = Const () ConstEmpty
unwindList (a:rest) = 
  App () (App () (VarRef () (QualifiedName ["Data","List"] ":")) a)
    (unwindList rest)

makeQualified :: String -> QualifiedName
makeQualified str = QualifiedName prefixes name
 where
  (prefixes,name) = loop str
  loop val =
    let (pre,rest) = span (/= '.') val
    in if rest == ""
         then ([], pre)
         else let (pres, name) = loop rest
              in (pre:pres, name)
}


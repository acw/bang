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
  'module'    { Lexeme $$ (TokValIdent "module"  )  }
  'export'    { Lexeme $$ (TokValIdent "export"  )  }
  'import'    { Lexeme $$ (TokValIdent "import"  )  }
  'datatype'  { Lexeme $$ (TokValIdent "datatype")  }
  'type'      { Lexeme $$ (TokValIdent "type"    )  }
  'newtype'   { Lexeme $$ (TokValIdent "newtype" )  }
  'class'     { Lexeme $$ (TokValIdent "class"   )  }
  'instance'  { Lexeme $$ (TokValIdent "instance")  }
  'qualified' { Lexeme $$ (TokValIdent "qualified") }
  'as'        { Lexeme $$ (TokValIdent "as")        }
  'case'      { Lexeme $$ (TokValIdent "case")      }
  'of'        { Lexeme $$ (TokValIdent "of")        }
  'restrict'  { Lexeme $$ (TokValIdent "restrict")  }

-- symbols
  '='        { Lexeme $$ (TokOpIdent  "=")        }
  '->'       { Lexeme $$ (TokOpIdent  "->")       }
  '=>'       { Lexeme $$ (TokOpIdent  "=>")       }
  '::'       { Lexeme $$ (TokOpIdent  "::")       }
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

top_module :: { Module Position } : 'module' TYPE_IDENT module_decls {
    let (imports,items) = $3
    in Module (makeQualified $2) imports items
  }

module_decls :: { ([Import], [Decl Position]) }
  : module_decls module_decl { $1 `pappend` $2 }
  | module_decl              { $1              }

module_decl :: { ([Import], [Decl Position]) }
  : import_decl ';' { ([$1], [])   }
  | decl        ';' { ([],   [$1]) }

-- Import Declarations ------------------------------------------------------

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

-- Actual Declarations ------------------------------------------------------

-- A declaration starts with an optional export flag and an optional type
-- restriction flag, and then has the declaration. We apply the restrictions /
-- exports post-hoc because we're lazy.
decl :: { Decl Position }
  : optional_decl_flags decl2 { $1 $2 }

optional_decl_flags :: { Decl Position -> Decl Position }
  :                           { id }
  | opt_export                { $1 }
  | opt_restrict              { $1 }
  | opt_export opt_restrict   { $1 . $2 }
  | opt_restrict opt_export   { $1 . $2 }

opt_export :: { Decl Position -> Decl Position }
  : 'export'                  { DeclExport $1 }

opt_restrict :: { Decl Position -> Decl Position }
  : 'restrict' '(' type_restrictions ')' { addTypeRestrictions $3 }

type_restrictions :: { [Type] }
  : type_restriction                       { [$1] }
  | type_restrictions ',' type_restriction { $1 ++ [$3] }

type_restriction :: { Type }
  : TYPE_IDENT VAL_IDENT
    { TAp (TVar (makeQualified $1) Star) (TVar (makeQualified $2) Star) }
  | type_restriction VAL_IDENT
    { TAp $1 (TVar (makeQualified $2) Star) }

decl2 :: { Decl Position }
  : data_decl     { $1 }
  | type_decl     { $1 }
  | newtype_decl  { $1 }
  | class_decl    { $1 }
  | instance_decl { $1 }
  | value_decl    { $1 }

-- Data Declarations --------------------------------------------------------

data_decl :: { Decl Position }
  : 'datatype' TYPE_IDENT type_args '=' data_clauses
    { DeclData $1 [] (makeQualified $2) $3 $5 }

type_args :: { [QualifiedName] }
  :                     { [] }
  | type_args VAL_IDENT { $1 ++ [makeQualified $2] }

data_clauses :: { [DataClause Position] }
  : data_clause                  { [] }
  | data_clauses '|' data_clause { $1 ++ [$3] }

data_clause :: { DataClause Position }
  : constructor_name '(' constructor_args ')'
    { DataClause $2 $1 (map fst $3) (map snd $3) }

constructor_name :: { QualifiedName }
  : TYPE_IDENT       { makeQualified $1 }
  | '(' OP_IDENT ')' { makeQualified $2 }

constructor_args :: { [(Maybe QualifiedName,Type)] }
  : constructor_arg                      { [$1] }
  | constructor_args ',' constructor_arg { $1 ++ [$3] }

constructor_arg :: { (Maybe QualifiedName,Type) }
  : bang_type                            { (Nothing, $1) }
  | VAL_IDENT '::' bang_type             { (Just (makeQualified $1), $3) }

-- Type Declarations --------------------------------------------------------

type_decl :: { Decl Position }
  : 'type'        { undefined }

-- Newtype Declarations -----------------------------------------------------

newtype_decl :: { Decl Position }
  : 'newtype'     { undefined }

-- Class Declarations -------------------------------------------------------

class_decl :: { Decl Position }
  : 'class'       { undefined }

-- Instance Declarations ----------------------------------------------------

instance_decl :: { Decl Position }
  : 'instance'    { undefined }

-- Value Declaration --------------------------------------------------------

value_decl :: { Decl Position }
  : value_ident optional_args optional_type { undefined }

optional_args :: { Maybe [(QualifiedName, Maybe Type)] }
  : '(' optional_args2 ')'   { Just $2 }
  |                          { Nothing }

optional_args2 :: { [(QualifiedName, Maybe Type)] }
  : optional_arg                     { [$1] }
  | optional_args2 ',' optional_arg  { $1 ++ [$3] }

optional_arg :: { (QualifiedName, Maybe Type) }
  : value_ident optional_type        { ($1, $2) }

optional_type :: { Maybe Type }
  :                      { Nothing }
  | '::' bang_type       { Just $2 }

value_ident :: { QualifiedName }
  : VAL_IDENT            { makeQualified $1 }
  | '(' OP_IDENT ')'     { makeQualified $2 }

-- Types in Bang ------------------------------------------------------------

bang_type :: { Type }
  : TYPE_IDENT           { TVar (makeQualified $1) Star }

-- 
-- data_decl :: { Decl Position }
--   : 'datatype' mqualifiers TYPE_IDENT data_args dataclauses
--   { DeclData $2 (makeQualified $3) $4 $5 }
-- 
-- mqualifiers :: { [Type] }
--   :                          { [] }
--   | '(' tqualifiers ')' '=>' { $2 }
-- 
-- tqualifiers :: { [Type] }
--   : tqualifier                 { [$1] }
--   | tqualifiers ',' tqualifier { $1 ++ [$3] }
-- 
-- tqualifier :: { Type }
--   : TYPE_IDENT VAL_IDENT
--     { TAp (TCon (makeQualified $1) Star) (TVar (makeQualified $2) Star) }
--   | tqualifier VAL_IDENT
--     { TAp $1 (TVar (makeQualified $2) Star) }
-- 
-- data_args :: { [QualifiedName] }
--   :                            { [] }
--   | data_args VAL_IDENT        { $1 ++ [makeQualified $2] }
-- 
-- dataclauses :: { [DataClause] }
--   : '=' dataclause             { [$2] }
--   | dataclauses '|' dataclause { $1 ++ [$3] }
-- 
-- dataclause :: { DataClause }
--   : constructor_id             { DataClause $1 [] }
--   | dataclause bangtype3       { let DataClause name items = $1
--                                  in DataClause name (items ++ [$2]) }
-- 
-- constructor_id :: { QualifiedName }
--   : TYPE_IDENT                 { makeQualified $1 }
--   | '(' OP_IDENT ')'           { makeQualified $2 }
-- 
-- -- Type alias Declarations --------------------------------------------------
-- 
-- type_decl :: { Decl Position }
--   : 'type'        { DeclType }
-- 
-- -- Newtype Declarations -----------------------------------------------------
-- 
-- newtype_decl :: { Decl Position }
--   : 'newtype'     { DeclNewtype }
-- 
-- -- Type class Declarations --------------------------------------------------
-- 
-- class_decl :: { Decl Position }
--   : 'class'       { DeclClass }
-- 
-- -- Instance Declarations ----------------------------------------------------
-- 
-- instance_decl :: { Decl Position }
--   : 'instance'    { DeclInstance }
-- 
-- -- Data value Declarations --------------------------------------------------
-- 
-- value_decl :: { Decl Position }
--   : value_name '=' expr { DeclValue Nothing $1 (Just $3) }
-- 
-- value_name :: { QualifiedName }
--   : VAL_IDENT    { makeQualified $1 }
--   | '(' OP_IDENT ')' { makeQualified $2 }
-- 
-- -- Data value type Declarations ---------------------------------------------
-- 
-- vtype_decl :: {Decl Position }
--  : value_name '::' bangtype { DeclValue (Just $3) $1 Nothing }
-- 
-- -- Types --------------------------------------------------------------------
-- 
-- bangtype :: { Type }
--   : bangtype1            { $1 }
-- 
-- bangtype1 :: { Type }
--   : bangtype1 VAL_IDENT  { TAp $1 (TVar (makeQualified $2) Star) }
--   | bangtype2            { $1 }
-- 
-- bangtype2 :: { Type }
--   : bangtype2 '->' bangtype3
--     { TAp (TAp (TCon (QualifiedName [] "->") Star) $1) $3 }
--   | bangtype3            { $1 }
-- 
-- bangtype3 :: { Type }
--   : '[' bangtype3 ']'
--     { TAp (TVar (QualifiedName ["Data","List"] "List") Star) $2 }
--   | bangtype4              { $1 }
-- 
-- bangtype4 :: { Type }
--   : TYPE_IDENT             { TVar (makeQualified $1) Star }
--   | '(' bangtype ')'       { $2 }
-- 
-- -- Statements ---------------------------------------------------------------
-- 
-- statement :: { Statement }
--   : expr ';'               { }
--   | 'case' expr 'of'       { }
-- 
-- -- Expressions --------------------------------------------------------------
-- 
-- expr :: { Expr Position }
--   : '\\' arglist '->' expr1 { Lambda Position $2 $4 }
--   | expr1                   { $1 }
-- 
-- arglist :: { [QualifiedName] }
--   : VAL_IDENT           { [makeQualified $1] }
--   | arglist VAL_IDENT   { $1 ++ [makeQualified $2] }
-- 
-- expr1 :: { Expr Position }
--   : '{' exprs '}'       { Block Position $2 }
--   | expr2               { $1 }
-- 
-- exprs :: { [Expr Position] }
--   : expr ';'            { [$1] }
--   | exprs expr ';'      { $1 ++ [$2] }
-- 
-- expr2 :: { Expr Position }
--   : '[' list_exprs ']'  { unwindList $2 }
--   | expr3               { $1 }
-- 
-- list_exprs :: { [Expr Position] }
--   :                      { [] }
--   | list_exprs ',' expr3 { $1 ++ [$3] }
-- 
-- expr3 :: { Expr Position }
--   : bottom_expr         { $1 }
-- 
-- bottom_expr :: { Expr Position }
--   : INTVAL              { let (b,v) = $1 in Const () (ConstInteger b v) }
--   | FLOATVAL            { Const Position (ConstFloat $1) }
--   | CHARVAL             { Const Position (ConstChar $1) }
--   | STRVAL              { Const Position (ConstString  $1) }
--   | VAL_IDENT           { VarRef Position (makeQualified $1) }
--   | '[' ']'             { VarRef () (QualifiedName ["Data","List"] "Null") }
--   | '(' expr ')'        { $2 }

{
lexer :: (Lexeme -> Parser a) -> Parser a
lexer k = scan >>= k

happyError :: Parser a
happyError  = raiseP "Parse Error"

pappend :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
pappend (a,b) (c,d) = (a++c,b++d)

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


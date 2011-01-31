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
  '@'        { Lexeme $$ (TokOpIdent  "@")       }
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
  '`'        { Lexeme $$ BTick   }

-- identifiers
  TYPE_IDENT { Lexeme _ (TokTypeIdent _) }
  VAL_IDENT  { Lexeme _ (TokValIdent _) }
  OP_IDENT   { Lexeme _ (TokOpIdent _) }

-- values
  INTVAL     { Lexeme _ (TokInt _) }
  FLOATVAL   { Lexeme _ (TokFloat _) }
  CHARVAL    { Lexeme _ (TokChar _) }
  STRVAL     { Lexeme _ (TokString _) }

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
  : constructor_name '(' ')'
    { DataClause $2 $1 [] [] }
  | constructor_name '(' constructor_args ')'
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
  : value_ident optional_args optional_type value_body
    {% postProcessDeclVal $1 $2 $3 $4 }


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

value_body :: { (Position, Expr Position) }
  : '=' expression       { ($1, $2) }
  | '{' statements '}'   { ($1, Block $1 $2) }

-- Types in Bang ------------------------------------------------------------

primary_type :: { Type }
  : TYPE_IDENT           { TVar (makeQualified $1) Star }
  | VAL_IDENT            { TVar (makeQualified $1) Star }
  | '(' bang_type ')'    { $2 }

type_application_type :: { Type }
  : type_application_type primary_type
    { TAp $1 $2 }
  | primary_type
    { $1 }

function_type :: { Type }
  : function_type '->' type_application_type
    { TAp (TVar (QualifiedName ["--INTERNAL--"] "->") Star) $3 }
  | type_application_type
    { $1 }

list_type :: { Type }
  : '[' list_type ']'
    { TAp (TVar (QualifiedName ["Data","List"] "List") Star) $2 }
  | function_type
    { $1 }

bang_type :: { Type }
  : list_type            { $1 }

-- Statements in bang

statements :: { [Stmt Position] }
  :                      { [] }
  | statements statement { $1 ++ [$2] }

statement :: { Stmt Position }
  : assignment_statement ';' { $1 }
  | case_statement ';'       { $1 }
  | expression ';'           { SExpr $2 $1 }

assignment_statement :: { Stmt Position }
  : value_ident '=' expression -- FIXME: Too restrictive!
    { SBind $2 $1 (SExpr $2 $3) }

case_statement :: { Stmt Position }
  : 'case' expression 'of' case_items
    { SCase $1 $2 $4 }

case_items :: { [(Pattern,Maybe (Expr Position),(Expr Position))] }
  : case_item            { [$1] }
  | case_items case_item { $1 ++ [$2] }

case_item :: { (Pattern, Maybe (Expr Position), (Expr Position)) }
  : pattern mguard '->' expression { ($1, $2, $4) }

mguard :: { Maybe (Expr Position) }
  :                 { Nothing }
  | '|' expression  { Just $2 }

-- Patterns for pattern matching

infix_operator :: { QualifiedName }
  : OP_IDENT             { makeQualified $1 }
  | '`' VAL_IDENT '`'    { makeQualified $2 }

pattern_primary :: { Pattern }
  : TYPE_IDENT           { PVar (makeQualified $1) }
  | VAL_IDENT            { PVar (makeQualified $1) }
  | '[' ']'               { PVar (QualifiedName ["Data","List"] "NULL") }
  | INTVAL                { let (Lexeme _ (TokInt (base, val))) = $1
                            in PConst (ConstInteger base val) }
  | FLOATVAL              { let (Lexeme _ (TokFloat val)) = $1
                            in PConst (ConstFloat val) }
  | CHARVAL               { let (Lexeme _ (TokChar val)) = $1
                            in PConst (ConstChar val) }
  | STRVAL                { let (Lexeme _ (TokString val)) = $1
                            in PConst (ConstString val) }
   | '(' pattern ')'      { $2 }

pattern_infix :: { Pattern }
  : pattern_infix infix_operator pattern_primary { PAp (PAp $1 (PVar $2)) $3 }
  | pattern_primary                              { $1 }

pattern_ap :: { Pattern }
  : pattern_ap pattern_infix { PAp $1 $2 }
  | pattern_infix            { $1 }

pattern_name :: { Pattern }
  : value_ident '@' pattern_name { PNamed $1 $3 }
  | pattern_ap                   { $1 }

pattern :: { Pattern }
  : pattern_name         { $1 }

-- Expressions in bang

primary_expression :: { Expr Position }
  : '(' expression ')'    { $2 }
  | '[' ']'               { VarRef $1 (QualifiedName ["Data","List"] "NULL") }
  | INTVAL                { let (Lexeme src (TokInt (base, val))) = $1
                            in Const src (ConstInteger base val) }
  | FLOATVAL              { let (Lexeme src (TokFloat val)) = $1
                            in Const src (ConstFloat val) }
  | CHARVAL               { let (Lexeme src (TokChar val)) = $1
                            in Const src (ConstChar val) }
  | STRVAL                { let (Lexeme src (TokString val)) = $1
                            in Const src (ConstString val) }
  | VAL_IDENT             { let l@(Lexeme src (TokValIdent name)) = $1
                            in VarRef src (makeQualified l) }

conditional_expression :: { Expr Position }
  : primary_expression { $1 }

infix_expression :: { Expr Position }
  : infix_expression infix_operator conditional_expression
    { App (getSpecial $1) (VarRef (getSpecial $1) $2) [$1, $3] }
  | conditional_expression
    { $1 }

lambda_expression :: { Expr Position }
  : '\\' arguments '->' infix_expression
    { Lambda $1 $2 $4 }
  | infix_expression
    { $1 }

arguments :: { [QualifiedName] }
  : value_ident               { [$1] }
  | arguments ',' value_ident { $1 ++ [$3] }

application_expression :: { Expr Position }
  : application_expression '(' app_args ')'
    { App $2 $1 $3 }
  | application_expression '(' ')'
    { App $2 $1 [] }
  | lambda_expression
    { $1 }

app_args :: { [Expr Position] }
  : expression                { [$1] }
  | app_args ',' expression   { $1 ++ [$3] }

block_expression :: { Expr Position }
  : '{' statements '}'        { Block $1 $2 }
  | application_expression    { $1 }

expression :: { Expr Position }
  : block_expression          { $1 }

{
lexer :: (Lexeme -> Parser a) -> Parser a
lexer k = scan >>= k

happyError :: Parser a
happyError  = raiseP "Parse Error"

pappend :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
pappend (a,b) (c,d) = (a++c,b++d)

makeQualified :: Lexeme -> QualifiedName
makeQualified (Lexeme _ (TokTypeIdent str)) = makeQualified' str
makeQualified (Lexeme _ (TokValIdent  str)) = makeQualified' str
makeQualified (Lexeme _ (TokOpIdent   str)) = makeQualified' str
makeQualified _                             = error "makeQualified bad arg"

makeQualified' :: String -> QualifiedName
makeQualified' str = QualifiedName prefixes name
 where
  (prefixes,name) = loop str
  loop val =
    let (pre,rest) = span (/= '.') val
    in if rest == ""
         then ([], pre)
         else let (pres, name) = loop rest
              in (pre:pres, name)

postProcessDeclVal :: QualifiedName ->
                      Maybe [(QualifiedName, Maybe Type)] ->
                      Maybe Type ->
                      (Position, Expr Position) ->
                      Parser (Decl Position)
postProcessDeclVal name margs mrettype (src, body) = do
  final_type <- case mrettype of
                   Nothing -> do
                     name <- gensym
                     return (TVar name Star)
                   Just x  ->
                     return x
  case margs of
    Nothing ->
      return (DeclValue src [] final_type body)
    Just [] ->
      fail "Need to figure out empty arg items."
    Just args ->
      fail "Need to figure out non-empty arg items."

}


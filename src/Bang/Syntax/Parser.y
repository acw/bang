-- -*- mode: haskell -*-
-- vi: set ft=haskell :
{
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTION_GHC -w              #-}
module Bang.Syntax.Parser(
         runParser
       , parseModule
       )
 where

import Bang.Monad(err)
import Bang.AST(Name, Module, NameEnvironment(..), mkModule, mkName, emptyExpression)
import Bang.AST.Declaration(Declaration, mkTypeDecl, mkValueDecl)
import Bang.AST.Expression(ConstantValue(..), Expression, mkConstExp, mkRefExp, mkLambdaExp)
import Bang.AST.Type(Type, Kind(..), mkTypeRef, mkFunType, mkTypeApp, mkPrimType)
import Bang.Syntax.Location(Located(..), Origin, Position)
import Bang.Syntax.ParserError(ParserError(..))
import Bang.Syntax.ParserMonad(Parser, addFixities, parseError, runNextToken, runParser)
import Bang.Syntax.Token(Token(..), Fixity(..))
import Control.Monad(forM)
import Data.List(union)
import Data.Text.Lazy(Text)

}

%name      top_module
%tokentype { Located Token }
%monad     { Parser        }
%error     { parseError    }
%lexer     { runNextToken  } { Located _ EOFTok }

%token
 '::'        { Located $$ (OpIdent   _ "::")         }
 '='         { Located $$ (OpIdent   _ "=")          }
 ','         { Located $$ (OpIdent   _ ",")          }
 '->'        { Located $$ (OpIdent   _ "->")         }
 'infixl'    { Located $$ (ValIdent    "infixl")     }
 'infixr'    { Located $$ (ValIdent    "infixr")     }
 'infix'     { Located $$ (ValIdent    "infix")      }
 'module'    { Located $$ (ValIdent    "module")     }
 'primitive' { Located $$ (ValIdent    "primitive")  }
 'type'      { Located $$ (ValIdent    "type")       }
 Integer     { Located _  (IntTok    _ _)            }
 Float       { Located _  (FloatTok  _)              }
 Char        { Located _  (CharTok   _)              }
 String      { Located _  (StringTok _)              }
 OpIdent     { Located _  (OpIdent   _ _)            }
 TypeIdent   { Located _  (TypeIdent _)              }
 ValIdent    { Located _  (ValIdent  _)              }
 OPL0        { Located _  (OpIdent (LeftAssoc  0) _) }
 OPR0        { Located _  (OpIdent (RightAssoc 0) _) }
 OPN0        { Located _  (OpIdent (NonAssoc   0) _) }
 OPL1        { Located _  (OpIdent (LeftAssoc  1) _) }
 OPR1        { Located _  (OpIdent (RightAssoc 1) _) }
 OPN1        { Located _  (OpIdent (NonAssoc   1) _) }
 OPL2        { Located _  (OpIdent (LeftAssoc  2) _) }
 OPR2        { Located _  (OpIdent (RightAssoc 2) _) }
 OPN2        { Located _  (OpIdent (NonAssoc   2) _) }
 OPL3        { Located _  (OpIdent (LeftAssoc  3) _) }
 OPR3        { Located _  (OpIdent (RightAssoc 3) _) }
 OPN3        { Located _  (OpIdent (NonAssoc   3) _) }
 OPL4        { Located _  (OpIdent (LeftAssoc  4) _) }
 OPR4        { Located _  (OpIdent (RightAssoc 4) _) }
 OPN4        { Located _  (OpIdent (NonAssoc   4) _) }
 OPL5        { Located _  (OpIdent (LeftAssoc  5) _) }
 OPR5        { Located _  (OpIdent (RightAssoc 5) _) }
 OPN5        { Located _  (OpIdent (NonAssoc   5) _) }
 OPL6        { Located _  (OpIdent (LeftAssoc  6) _) }
 OPR6        { Located _  (OpIdent (RightAssoc 6) _) }
 OPN6        { Located _  (OpIdent (NonAssoc   6) _) }
 OPL7        { Located _  (OpIdent (LeftAssoc  7) _) }
 OPR7        { Located _  (OpIdent (RightAssoc 7) _) }
 OPN7        { Located _  (OpIdent (NonAssoc   7) _) }
 OPL8        { Located _  (OpIdent (LeftAssoc  8) _) }
 OPR8        { Located _  (OpIdent (RightAssoc 8) _) }
 OPN8        { Located _  (OpIdent (NonAssoc   8) _) }
 OPL9        { Located _  (OpIdent (LeftAssoc  9) _) }
 OPR9        { Located _  (OpIdent (RightAssoc 9) _) }
 OPN9        { Located _  (OpIdent (NonAssoc   9) _) }

%right    OPL0
%left     OPR0
%nonassoc OPN0
%right    OPL1
%left     OPR1
%nonassoc OPN1
%right    OPL2
%left     OPR2
%nonassoc OPN2
%right    OPL3
%left     OPR3
%nonassoc OPN3
%right    OPL4
%left     OPR4
%nonassoc OPN4
%right    OPL5
%left     OPR5
%nonassoc OPN5
%right    OPL6
%left     OPR6
%nonassoc OPN6
%right    OPL7
%left     OPR7
%nonassoc OPN7
%right    OPL8
%left     OPR8
%nonassoc OPN8
%right    OPL9
%left     OPR9
%nonassoc OPN9

%%

top_module :: { Module }
  : 'module' TypeIdent listopt(Declaration)
    {%
       do let Located src (TypeIdent rawName) = $2
          return (mkModule (mkName rawName ModuleEnv src 0) [$3]) }

Declaration :: { Maybe Declaration }
  : ValueDeclaration  { Just $1 }
  | FixityDeclaration { Nothing }
  | TypeDeclaration   { Just $1 }

ValueDeclaration :: { Declaration }
  : list1(ValIdent) '=' Expression
    {%
       case $1 of
         [] ->
            err (InternalError $2 "ValDeclLHS")
         [Located src (ValIdent rawName)] ->
           do let name = mkName rawName VarEnv src 0
              return (mkValueDecl name src Nothing $3)
         ((Located src (ValIdent rawName)) : args) ->
           do let name     = mkName rawName VarEnv src 0
                  argNames = map (\ (Located arsrc (ValIdent argName)) ->
                                   mkName argName VarEnv arsrc 0)
                                 args
              return (mkValueDecl name src Nothing
                       (mkLambdaExp $2 argNames $3))
    }

FixityDeclaration :: { () }
  : 'infixl' Integer sep(',',OpIdent)
    {% addFixities $1 LeftAssoc  $2 $3 }
  | 'infixr' Integer sep(',',OpIdent)
    {% addFixities $1 RightAssoc $2 $3 }
  | 'infix'  Integer sep(',',OpIdent)
    {% addFixities $1 NonAssoc   $2 $3 }

TypeDeclaration :: { Declaration }
  : ValIdent '::' Type
    {%
       do let Located src (ValIdent rawName) = $1
              name = mkName rawName VarEnv src 0
          return (mkValueDecl name src (Just $3) emptyExpression) }
  | 'type' TypeIdent '=' Type
    {%
       do let Located src (TypeIdent rawName) = $2
              name = mkName rawName TypeEnv src 0
          return (mkTypeDecl name src $4) }
  | 'primitive' 'type' TypeIdent '=' String
    {%
       do let Located nsrc (TypeIdent rawName) = $3
              Located tsrc (StringTok rawText) = $5
              name = mkName rawName TypeEnv nsrc 0
          return (mkTypeDecl name $2 (mkPrimType tsrc rawText)) }

-- -----------------------------------------------------------------------------

Type :: { Type }
  : RawType                      { $1 }

RawType :: { Type }
  : RawType '->' BaseType        { mkFunType $2 [$1] $3 }
  | BaseType                     { $1                   }

BaseType :: { Type }
  : TypeIdent   {%
                  let Located src (TypeIdent rawName) = $1
                      name = mkName rawName TypeEnv src 0
                  in return (mkTypeRef src Unknown name) }
  | ValIdent    {%
                  let Located src (ValIdent rawName) = $1
                      name = mkName rawName TypeEnv src 0
                  in return (mkTypeRef src Unknown name) }

-- -----------------------------------------------------------------------------

Expression :: { Expression }
  : BaseExpression { $1 }

BaseExpression :: { Expression }
  : OpIdent  {%
               let Located src (OpIdent _ rawName) = $1
                   name = mkName rawName VarEnv src 0
               in return (mkRefExp src name) }
  | ValIdent {%
               let Located src (ValIdent rawName) = $1
                   name = mkName rawName VarEnv src 0
               in return (mkRefExp src name) }
  | Integer  { let Located src (IntTok base val) = $1
               in mkConstExp src (ConstantInt base val) }
  | String   { let Located src (StringTok val) = $1
               in mkConstExp src (ConstantString val) }
  | Float    { let Located src (FloatTok val) = $1
               in mkConstExp src (ConstantFloat val) }
  | Char     { let Located src (CharTok val) = $1
               in mkConstExp src (ConstantChar val) }

-- -----------------------------------------------------------------------------

opt(p)
  : {- empty -} { Nothing }
  | p           { Just $1 }

sep(p,q)
  : {- empty -}   { []         }
  | sep_body(p,q) { reverse $1 }

sep1(p,q)
  : sep_body(p,q) { reverse $1 }

sep_body(p,q)
  : q                 { [$1]    }
  | sep_body(p,q) p q { $3 : $1 }

list(p)
  : {- empty -}  { []         }
  | list_body(p) { reverse $1 }

list1(p)
  : list_body(p) { reverse $1 }

list_body(p)
  : p              { [$1]    }
  | list_body(p) p { $2 : $1 }

listopt(p)
  : {- empty -}    { [] }
  | listopt(p) p   { case $2 of
                       Nothing -> $1
                       Just x  -> $1 ++ [x]
                   }

{

parseModule :: Parser Module
parseModule = top_module

}

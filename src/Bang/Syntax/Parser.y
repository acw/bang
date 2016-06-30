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
import Bang.Syntax.AST(Module(..), Name(..), NameEnvironment(..),
                       Declaration(..), Expression(..), Type(..), Kind(..),
                       ConstantValue(..))
import Bang.Syntax.Location(Located(..), Origin, Position)
import Bang.Syntax.ParserError(ParserError(..))
import Bang.Syntax.ParserMonad(Parser, addFixities, registerName,
                               unregisterNames, lookupName, parseError,
                               runNextToken, runParser)
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
          name <- registerName False src ModuleEnv rawName
          return (Module name $3) }

Declaration :: { Maybe Declaration }
  : ValueDeclaration  { Just $1 }
  | FixityDeclaration { Nothing }
  | TypeDeclaration   { Just $1 }

ValueDeclaration :: { Declaration }
  : ValueDeclLHS Expression
    {%
        do let (builder, argNames) = $1
           unregisterNames VarEnv argNames
           return (builder $2)
    }

ValueDeclLHS :: { (Expression -> Declaration, [Name]) }
 : list1(ValIdent) '='
    {%
       case $1 of
         [] ->
            err (InternalError $2 "ValDeclLHS")
         [Located src (ValIdent rawName)] ->
           do name <- registerName True src VarEnv rawName
              return (ValueDeclaration name, [name])
         ((Located src (ValIdent rawName)) : args) ->
           do name <- registerName True src VarEnv rawName
              argNames <- forM args $ \ (Located asrc (ValIdent argName)) ->
                            registerName True asrc VarEnv argName
              let builder = ValueDeclaration name . LambdaExp $2 argNames
              return (builder, argNames)
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
          name <- registerName True src VarEnv rawName
          return (TypeDeclaration name $3) }
  | 'primitive' 'type' TypeIdent '=' String
    {%
       do let Located src (TypeIdent rawName) = $3
              Located _   (StringTok rawText) = $5
          name <- registerName False src TypeEnv rawName
          return (PrimTypeDeclaration name rawText) }

-- -----------------------------------------------------------------------------

Type :: { Type }
  : RawType {%
      do let (result, names) = $1
         case names of
           [] -> return result
           xs ->
             do unregisterNames TypeEnv xs
                return (TypeForAll xs result)
      }

RawType :: { (Type, [Name]) }
  : RawType '->' BaseType {%
      do let (p1, names1) = $1
             (p2, names2) = $3
         return (TypeLambda $2 (Star `KindArrow` Star) p1 p2, union names1 names2)
     }
  | BaseType           { $1 }

BaseType :: { (Type, [Name]) }
  : TypeIdent   {%
       do let Located src (TypeIdent rawName) = $1
          name <- lookupName src TypeEnv rawName
          return (TypeRef src Star name, []) }
  | ValIdent    {%
       do let Located src (ValIdent rawName) = $1
          name <- registerName True src TypeEnv rawName
          return (TypeRef src Star name, [name])
     }

-- -----------------------------------------------------------------------------

Expression :: { Expression }
  : BaseExpression { $1 }

BaseExpression :: { Expression }
  : OpIdent  {%
                do let Located src (OpIdent _ rawName) = $1
                   name <- lookupName src VarEnv rawName
                   return (ReferenceExp src name) }
  | ValIdent {%
                do let Located src (ValIdent rawName) = $1
                   name <- lookupName src VarEnv rawName
                   return (ReferenceExp src name) }
  | Integer  {%
                do let Located src (IntTok base val) = $1
                   return (ConstantExp src (ConstantInt base val)) }
  | String   {%
                do let Located src (StringTok val) = $1
                   return (ConstantExp src (ConstantString val)) }
  | Float    {%
                do let Located src (FloatTok val) = $1
                   return (ConstantExp src (ConstantFloat val)) }
  | Char     {%
                do let Located src (CharTok val) = $1
                   return (ConstantExp src (ConstantChar val)) }

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

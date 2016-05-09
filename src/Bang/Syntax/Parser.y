-- -*- mode: haskell -*-
-- vi: set ft=haskell :
{
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTION_GHC -w              #-}
module Bang.Syntax.Parser(
         parseModule
       , ParseError, showError
       , lexWithLayout
       )
 where

import           Bang.Syntax.AST
import           Bang.Syntax.Lexer
import           Bang.Syntax.Location
import           Bang.Syntax.Token
import           Data.Map.Strict(Map)
import           Data.Map.Strict as Map
import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as T
import           MonadLib

}

%tokentype { Located Token }

%token
 '::'      { Located $$ (OpIdent   _ "::")     }
 '='       { Located $$ (OpIdent   _ "=")      }
 ','       { Located $$ (OpIdent   _ ",")      }
 'infixl'  { Located $$ (ValIdent    "infixl") }
 'infixr'  { Located $$ (ValIdent    "infixr") }
 'infix'   { Located $$ (ValIdent    "infix")  }
 'module'  { Located $$ (ValIdent    "module") }
 Integer   { Located _  (IntTok    _ _)        }
 OpIdent   { Located _  (OpIdent   _ _)        }
 TypeIdent { Located _  (TypeIdent _)          }
 ValIdent  { Located _  (ValIdent  _)          }
 OPL0 { Located _  (OpIdent (LeftAssoc  0) _)   }
 OPR0 { Located _  (OpIdent (RightAssoc 0) _)   }
 OPN0 { Located _  (OpIdent (NonAssoc   0) _)   }
 OPL1 { Located _  (OpIdent (LeftAssoc  1) _)   }
 OPR1 { Located _  (OpIdent (RightAssoc 1) _)   }
 OPN1 { Located _  (OpIdent (NonAssoc   1) _)   }
 OPL2 { Located _  (OpIdent (LeftAssoc  2) _)   }
 OPR2 { Located _  (OpIdent (RightAssoc 2) _)   }
 OPN2 { Located _  (OpIdent (NonAssoc   2) _)   }
 OPL3 { Located _  (OpIdent (LeftAssoc  3) _)   }
 OPR3 { Located _  (OpIdent (RightAssoc 3) _)   }
 OPN3 { Located _  (OpIdent (NonAssoc   3) _)   }
 OPL4 { Located _  (OpIdent (LeftAssoc  4) _)   }
 OPR4 { Located _  (OpIdent (RightAssoc 4) _)   }
 OPN4 { Located _  (OpIdent (NonAssoc   4) _)   }
 OPL5 { Located _  (OpIdent (LeftAssoc  5) _)   }
 OPR5 { Located _  (OpIdent (RightAssoc 5) _)   }
 OPN5 { Located _  (OpIdent (NonAssoc   5) _)   }
 OPL6 { Located _  (OpIdent (LeftAssoc  6) _)   }
 OPR6 { Located _  (OpIdent (RightAssoc 6) _)   }
 OPN6 { Located _  (OpIdent (NonAssoc   6) _)   }
 OPL7 { Located _  (OpIdent (LeftAssoc  7) _)   }
 OPR7 { Located _  (OpIdent (RightAssoc 7) _)   }
 OPN7 { Located _  (OpIdent (NonAssoc   7) _)   }
 OPL8 { Located _  (OpIdent (LeftAssoc  8) _)   }
 OPR8 { Located _  (OpIdent (RightAssoc 8) _)   }
 OPN8 { Located _  (OpIdent (NonAssoc   8) _)   }
 OPL9 { Located _  (OpIdent (LeftAssoc  9) _)   }
 OPR9 { Located _  (OpIdent (RightAssoc 9) _)   }
 OPN9 { Located _  (OpIdent (NonAssoc   9) _)   }


%monad { Parser       }
%error { parseError   }
%lexer { runNextToken } { Located initialPosition EOFTok }

%name top_module

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
  : 'module' TypeIdent listopt(declaration)
    { Module (identToName $2) $3 }

declaration :: { Maybe Declaration }
  : ValIdent '::' Type
    { Just TypeDeclaration }
  | ValIdent '=' Expression
    { Just ValueDeclaration }
  | 'infixl' Integer sep(',',OpIdent)
    {% addFixities $1 LeftAssoc  $2 $3 >> return Nothing }
  | 'infixr' Integer sep(',',OpIdent)
    {% addFixities $1 RightAssoc $2 $3 >> return Nothing }
  | 'infix'  Integer sep(',',OpIdent)
    {% addFixities $1 NonAssoc   $2 $3 >> return Nothing }

Type :: { Type }
  : {- empty -} { Type }

Expression :: { Expression }
  : {- empty -} { Expression }

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
  : {- empty -}    { []          }
  | listopt(p) p   { case $2 of
                       Nothing -> $1
                       Just x  -> $1 ++ [x]
                   }

{

newtype Parser a = Parser {
          unParser :: StateT ParserState (ExceptionT ParseError Id) a
        }
 deriving (Functor, Applicative, Monad)

data ParseError = LexError      Location Text
                | ParseError    Location Token
                | SemanticError Location Text
                | UnexpectedEOF
 deriving (Show)

showError :: ParseError -> String
showError (LexError      l t) = show l ++ ": lexer error around " ++ T.unpack t
showError (ParseError    l t) = show l ++ ": parse error around " ++ showToken t
showError  UnexpectedEOF      =           "Unexpected end of file"

data ParserState = ParserState {
       psPrecTable   :: Map Text Word
     , psTokenStream :: [Located Token]
     }

instance StateM Parser ParserState where
  get = Parser   get
  set = Parser . set

instance ExceptionM Parser ParseError where
  raise = Parser . raise

instance RunExceptionM Parser ParseError where
  try m = Parser (try (unParser m))

addFixities :: Location ->
               (Word -> Fixity) -> Located Token -> [Located Token] ->
               Parser ()
addFixities = undefined

runNextToken :: (Located Token -> Parser a) -> Parser a
runNextToken action =
  do state <- get
     case psTokenStream state of
       []         -> raise UnexpectedEOF
       (x : rest) ->
         do set (state{ psTokenStream = rest })
            action x

lexWithLayout :: Origin -> Position -> Text -> [Located Token]
lexWithLayout src pos txt = lexer src (Just pos) txt

parseModule :: Origin -> Text -> Either ParseError Module
parseModule src txt =
  let parserM   = unParser top_module
      excM      = runStateT initialState (parserM :: StateT ParserState (ExceptionT ParseError Id) Module)
      idM       = runExceptionT (excM :: ExceptionT ParseError Id (Module, ParserState))
      resWState = runId idM
  in fmap fst resWState
 where
  tokenStream  = lexWithLayout src initialPosition txt
  initialState = ParserState Map.empty tokenStream

parseError :: Located Token -> Parser a
parseError t =
  case t of
    Located _ EOFTok       -> raise UnexpectedEOF
    Located p (ErrorTok t) -> raise (LexError p t)
    Located p t            -> raise (ParseError p t)

}

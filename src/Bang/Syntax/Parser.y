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
import           Data.Char(digitToInt)
import           Data.Map.Strict(Map)
import           Data.Map.Strict as Map
import           Data.Maybe(catMaybes)
import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as T
import           MonadLib

import Debug.Trace

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
  : 'module' TypeIdent listopt(declaration)
    {%
       do let Located src (TypeIdent rawName) = $2
          name <- registerName False src ModuleEnv rawName
          return (Module name $3) }

declaration :: { Maybe Declaration }
  : ValIdent '::' Type
    {%
       do let Located src (ValIdent rawName) = $1
          name <- registerName True src VarEnv rawName
          return (Just (TypeDeclaration name $3)) }
  | ValIdent '=' Expression
    {%
       do let Located src (ValIdent rawName) = $1
          name <- registerName True src VarEnv rawName
          return (Just (ValueDeclaration name $3)) }
  | 'infixl' Integer sep(',',OpIdent)
    {% addFixities $1 LeftAssoc  $2 $3 >> return Nothing }
  | 'infixr' Integer sep(',',OpIdent)
    {% addFixities $1 RightAssoc $2 $3 >> return Nothing }
  | 'infix'  Integer sep(',',OpIdent)
    {% addFixities $1 NonAssoc   $2 $3 >> return Nothing }
  | 'primitive' 'type' TypeIdent '=' String
    {%
       do let Located src (TypeIdent rawName) = $3
              Located _   (StringTok rawText) = $5
          name <- registerName False src TypeEnv rawName
          return (Just (PrimTypeDecl (PrimType name rawText))) }

Type :: { Type }
  : TypeIdent   {%
       do let Located src (TypeIdent rawName) = $1
          name <- lookupName src TypeEnv rawName
          return (TypeRef src name) }

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
                   return (ReferenceExp src (trace "NAME" name)) }
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
  : p p p { catMaybes [$1, $2, $3] }
--  : {- empty -}    { [] }
--  | listopt(p) p   { case $2 of
--                       Nothing -> $1
--                       Just x  -> $1 ++ [x]
--                   }

{

newtype Parser a = Parser {
          unParser :: StateT ParserState (ExceptionT ParseError Id) a
        }
 deriving (Functor, Applicative, Monad)

data ParseError = LexError          Location Text
                | ParseError        Location Token
                | RedefinitionError Location Location Text
                | InternalError     Location Text
                | UnboundVariable   Location Text
                | UnexpectedEOF
 deriving (Show)

showError :: ParseError -> String
showError (LexError      l t) = show l ++ ": lexer error around " ++ T.unpack t
showError (ParseError    l t) = show l ++ ": parse error around " ++ showToken t
showError  UnexpectedEOF      =           "Unexpected end of file"

data ParserState = ParserState {
       psPrecTable    :: Map Text Fixity
     , psTokenStream  :: [Located Token]
     , psNameDatabase :: Map (NameEnvironment, Text) Name
     , psNextIdent    :: Word
     }

initialState :: [Located Token] -> ParserState
initialState tokenStream = ParserState {
    psPrecTable    = Map.empty
  , psTokenStream  = tokenStream
  , psNameDatabase = Map.empty
  , psNextIdent    = 1
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
addFixities src fixityBuilder lval names =
  do value <- processInteger lval
     let fixity = fixityBuilder value
     forM_ names $ \ tok ->
       do state <- get
          name <- forceNameDefined VarEnv src tok state
          let table' = Map.insert name fixity (psPrecTable state)
          set state{ psPrecTable = table' }
 where
  processInteger x =
    case x of
      Located _ (IntTok base text) ->
        return (makeNumeric base text 0)
      _ ->
        raise (InternalError src "Non-number in fixity?")

  --
  makeNumeric base text acc =
    case T.uncons text of
      Nothing        -> acc
      Just (x, rest) ->
        let acc' = (acc * base) + charValue x
        in makeNumeric base rest acc'
  --
  charValue = fromIntegral . digitToInt
  --
  tokenName t =
    case t of
      Located _ (TypeIdent   x) -> x
      Located _ (ValIdent    x) -> x
      Located _ (OpIdent   _ x) -> x
      _                       ->
        error "Internal error (tokenName in Parser.y)"
  --
  forceNameDefined env src token state =
    do let name = tokenName token
       case Map.lookup (env, name) (psNameDatabase state) of
         Just _  -> return name
         Nothing -> raise (UnboundVariable src name)

registerName :: Bool -> Location -> NameEnvironment -> Text -> Parser Name
registerName redefOk loc env name =
  do state <- get
     let key = (env, name)
         db  = psNameDatabase state
     case Map.lookup key db of
       Nothing ->
         do let res    = Name loc env (psNextIdent state) name
                state' = state {
                           psNameDatabase = Map.insert key res db
                         , psNextIdent    = 1 + psNextIdent state
                         }
            set state'
            return res
       Just res | redefOk ->
         return res
       Just (Name origLoc _ _ _) ->
         raise (RedefinitionError loc origLoc name)

lookupName :: Location -> NameEnvironment -> Text -> Parser Name
lookupName loc env name =
  do state <- get
     case Map.lookup (env, name) (psNameDatabase state) of
       Nothing ->
         raise (UnboundVariable loc name)
       Just name ->
         return name

runNextToken :: (Located Token -> Parser a) -> Parser a
runNextToken action =
  do state <- get
     case psTokenStream state of
       [] ->
         raise (InternalError unknownLocation "End of stream, but no EOF?")
       (eof@(Located _ EOFTok) : _)    ->
         action eof -- leave this on at the end of the stream
       (x : rest) ->
         do set (state{ psTokenStream = rest })
            action x

lexWithLayout :: Origin -> Position -> Text -> [Located Token]
lexWithLayout src pos txt = lexer src (Just pos) txt

parseModule :: Origin -> Text -> Either ParseError Module
parseModule src txt =
  let parserM   = unParser top_module
      excM      = runStateT (initialState tokenStream)
                    (parserM :: StateT ParserState (ExceptionT ParseError Id) Module)
      idM       = runExceptionT (excM :: ExceptionT ParseError Id (Module, ParserState))
      resWState = runId idM
  in fmap fst resWState
 where
  tokenStream  = lexWithLayout src initialPosition txt

parseError :: Located Token -> Parser a
parseError t =
  case t of
    Located _ EOFTok       -> raise UnexpectedEOF
    Located p (ErrorTok t) -> raise (LexError p t)
    Located p t            -> raise (ParseError p t)

}

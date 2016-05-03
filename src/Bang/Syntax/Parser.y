{
{-# OPTION_GHC -w              #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGAUGE OverloadedStrings #-}
module Bang.Syntax.Parser(
         parseModule
       , parseExpression
       , lexWithLayout
       )
 where

import Bang.Syntax.Lexer
import Bang.Syntax.Location
import Bang.Syntax.Token
import MonadLib

}

%tokentype { Located Token }

%token
 '::' { Located $$ (OpIdent "::") }

%monad { Parser }
%error { parseError }

%name top_module

%%

top_module :: { () }
 : '::' { () }

{

newtype Parser a = Parser {
          unParser :: StateT ParserState (ExceptionT Error Id) a
        }
 deriving (Functor, Applicative, Monad)

data ParserState = ParserState {
       psPrecTable :: Map Text Word
     , psPosition  :: Location
     }

instance StateM Parser ParserState where
  get = Parser   get
  set = Parser . set

instance ExceptionM Parser Error where
  raise = Parser . raise

instance RunExceptionM Parser Error where
  try m = Parser (try (unParser m))

lexWithLayout :: Source -> Position -> L.Text -> [Located Token]
lexWithLayout = undefined

parseModule :: Source -> L.Text -> Parser Module
parseModule = undefined

parseExpression :: Source -> L.Text -> Parser Module
parseModule = undefined

parseError :: [Located Token] -> Parser a
parseError toks =
  case toks of
    [] ->
      raise (Error 
      addError Nothing ErrParser "Unexpected end of file."
    (Located p (ErrorTok _) : _) ->
      addError (Just p) ErrLexer "Lexer error"
    (Located p _ : _) ->
      addError (Just p) ErrParser "Parser error"

}

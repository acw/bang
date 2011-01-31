{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax.ParserCore where

import Control.Applicative(Applicative)
import qualified Data.ByteString as S
import MonadLib

-- --------------------------------------------------------------------------
-- Positions
--

data Position = Position {
    posOff  :: !Int
  , posLine :: !Int
  , posCol  :: !Int
  , posFile :: !FilePath
  }
 deriving (Show)

initPosition :: FilePath -> Position
initPosition = Position 0 1 1

movePos :: Position -> Char -> Position
movePos (Position o l c f) '\t' = Position (o+1) l     (c+8) f
movePos (Position o l _ f) '\n' = Position (o+1) (l+1) 0     f
movePos (Position o l c f) _    = Position (o+1) l     (c+1) f

pprtPosition :: Position -> String
pprtPosition p = posFile p ++ ":" ++ show (posLine p) ++ ":" ++ show (posCol p)

-- --------------------------------------------------------------------------
-- Tokens
--

data Token = LParen | RParen
           | LSquare | RSquare
           | LBrace | RBrace
           | Bar | Semi | Comma | BTick
           | TokTypeIdent String
           | TokValIdent String
           | TokOpIdent String
           | TokInt (Int,String)
           | TokFloat String
           | TokChar String
           | TokString String
           | TokEOF
 deriving (Eq, Show)

-- --------------------------------------------------------------------------
-- Lexemes
--

data Lexeme = Lexeme {
    lexPos :: !Position
  , lexTok :: Token
  }
 deriving (Show)

instance Eq Lexeme where
  a == b    = lexTok a == lexTok b

-- --------------------------------------------------------------------------
-- Errors
--

data ErrorType =
    LexerError
  | ParserError
 deriving (Show)

data Error = Error ErrorType String Position
 deriving (Show)

printError :: Error -> IO ()
printError (Error etype str pos) = putStrLn errstr
 where
  errstr = pprtPosition pos ++ ":" ++ etypeStr ++ ": " ++ str
  etypeStr = case etype of
               LexerError  -> "LEX"
               ParserError -> "PARSE"

-- --------------------------------------------------------------------------
-- ParserState
--

data ParserState = ParserState {
    psInput   :: !S.ByteString
  , psChar    :: !Char
  , psPos     :: !Position
  , psLexCode :: !Int
  , psGenNum  :: !Int
  }
 deriving (Show)

initParserState :: FilePath -> S.ByteString -> ParserState
initParserState path bs = ParserState {
    psInput   = bs
  , psChar    = '\n'
  , psPos     = initPosition path
  , psLexCode = 0
  , psGenNum  = 0
  }

-- --------------------------------------------------------------------------
-- Parser
--

newtype Parser a = Parser {
    unParser :: StateT ParserState (ExceptionT Error Id) a
  } deriving (Functor, Applicative, Monad)

instance StateM Parser ParserState where
  get = Parser   get
  set = Parser . set

instance ExceptionM Parser Error where
  raise = Parser . raise

instance RunExceptionM Parser Error where
  try m = Parser (try (unParser m))

-- |Raise a lexer error
raiseL :: String -> Parser a
raiseL msg = do
  st <- get
  raise (Error LexerError msg (psPos st))

-- |Raise a parser error
raiseP :: String -> Parser a
raiseP msg = do
  st <- get
  raise (Error ParserError msg (psPos st))

-- |Run the parser over the given file
runParser :: FilePath -> S.ByteString -> Parser a -> Either Error a
runParser path bs (Parser m) =
  case runM m (initParserState path bs) of
    Right (a,_) -> Right a
    Left  err   -> Left err

genstr :: Parser String
genstr = do
  st <- get
  set st{ psGenNum = psGenNum st + 1 }
  return $ "--gen" ++ show (psGenNum st)


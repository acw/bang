{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Bang.Syntax.ParserMonad(
         Parser
       , runParser
       , addFixities
       , parseError
       , runNextToken
       )
 where

import           Bang.Monad(Compiler, err, runPass,
                            setPassState, overPassState, viewPassState)
import           Bang.Syntax.Lexer(AlexReturn(..), AlexInput(..), alexScan)
import           Bang.Syntax.Location(Location(..), Located(..),
                                      Origin(..), initialPosition,
                                      advanceWith', locatedAt)
import           Bang.Syntax.ParserError(ParserError(..))
import           Bang.Syntax.Token(Token(..), Fixity)
import           Control.Lens.TH(makeLenses)
import           Control.Monad(forM_)
import           Data.Char(digitToInt, isSpace)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as T

data ParserState = ParserState {
       _psPrecTable    :: Map Text Fixity
     , _psOrigin       :: Origin
     , _psLexerState   :: AlexInput
     }

makeLenses ''ParserState

type Parser a = Compiler ParserState a

runParser :: Origin -> Text -> Parser a -> Compiler ps a
runParser origin stream action = snd `fmap` runPass pstate action
 where
  initInput = AlexInput initialPosition stream
  pstate    = ParserState Map.empty origin initInput

-- -----------------------------------------------------------------------------

addFixities :: Location ->
               (Word -> Fixity) -> Located Token -> [Located Token] ->
               Parser ()
addFixities src fixityBuilder lval names =
  do value <- processInteger lval
     let fixity = fixityBuilder value
     forM_ names $ \ tok ->
       overPassState psPrecTable (Map.insert (tokenName tok) fixity)
 where
  processInteger x =
    case x of
      Located _ (IntTok base text) ->
        return (makeNumeric base text 0)
      _ ->
        err (InternalError src "Non-number in fixity?")

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

getFixities :: Parser (Map Text Fixity)
getFixities = viewPassState psPrecTable

-- -----------------------------------------------------------------------------

runNextToken :: (Located Token -> Parser a) ->
                Parser a
runNextToken parseAction = go =<< getLexerState
 where
  go state@(AlexInput initPos _) =
     case alexScan state 0 of
       AlexEOF ->
         do orig <- getOrigin
            parseAction (EOFTok `locatedAt` Location orig initPos initPos)
       AlexError (AlexInput pos text) ->
         do let (as, bs) = T.break isSpace text
                pos'     = advanceWith' pos as
                input'   = AlexInput pos' bs
            setLexerState input'
            orig <- getOrigin
            parseAction (ErrorTok as `locatedAt` Location orig initPos initPos)
       AlexSkip input' _ ->
         go input'
       AlexToken input' len lexAction ->
         do setLexerState input'
            src <- getOrigin
            table <- getFixities
            parseAction (lexAction src table len state)

-- -----------------------------------------------------------------------------

getOrigin :: Parser Origin
getOrigin = viewPassState psOrigin

getLexerState :: Parser AlexInput
getLexerState = viewPassState psLexerState

setLexerState :: AlexInput -> Parser ()
setLexerState = setPassState psLexerState

-- -----------------------------------------------------------------------------

parseError :: Located Token -> Parser a
parseError t =
  case t of
    Located _ EOFTok         -> err UnexpectedEOF
    Located p (ErrorTok tok) -> err (LexError p tok)
    Located p tok            -> err (ParseError p tok)



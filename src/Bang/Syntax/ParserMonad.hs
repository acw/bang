{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Bang.Syntax.ParserMonad(
         Parser
       , NameDatabase
       , runParser
       , addFixities
       , registerName
       , unregisterNames
       , lookupName
       , parseError
       , runNextToken
       )
 where

import           Bang.Monad(Compiler, err, runPass,
                            getPassState, overPassState, viewPassState)
import           Bang.Syntax.AST(Name(..), NameEnvironment(..))
import           Bang.Syntax.Lexer(AlexReturn(..), AlexInput(..), alexScan)
import           Bang.Syntax.Location(Location(..), Located(..),
                                      Origin(..), initialPosition,
                                      advanceWith', locatedAt)
import           Bang.Syntax.ParserError(ParserError(..))
import           Bang.Syntax.Token(Token(..), Fixity)
import           Control.Lens(view, set, over, _1)
import           Control.Lens.TH(makeLenses)
import           Control.Monad(forM_)
import           Data.Char(digitToInt, isSpace)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as T

type NameDatabase = Map (NameEnvironment, Text) Name

data ParserState = ParserState {
       _psPrecTable    :: Map Text Fixity
     , _psNameDatabase :: Map (NameEnvironment, Text) Name
     , _psNextIdent    :: Word
     , _psOrigin       :: Origin
     , _psLexerState   :: AlexInput
     }

makeLenses ''ParserState

type Parser a = Compiler ParserState a

runParser :: Origin -> Text -> Parser a -> Compiler ps (NameDatabase,  a)
runParser origin stream action =
  over _1 (view psNameDatabase) `fmap` runPass pstate action
 where
  initInput = AlexInput initialPosition stream
  pstate    = ParserState Map.empty Map.empty 1 origin initInput

-- -----------------------------------------------------------------------------

addFixities :: Location ->
               (Word -> Fixity) -> Located Token -> [Located Token] ->
               Parser ()
addFixities src fixityBuilder lval names =
  do value <- processInteger lval
     let fixity = fixityBuilder value
     forM_ names $ \ tok ->
       do state <- getPassState
          name <- forceNameDefined VarEnv src tok state
          overPassState (over psPrecTable (Map.insert name fixity))
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
  --
  forceNameDefined env loc token state =
    do let name = tokenName token
       case Map.lookup (env, name) (view psNameDatabase state) of
         Just _  -> return name
         Nothing -> err (UnboundVariable loc name)

getFixities :: Parser (Map Text Fixity)
getFixities = viewPassState psPrecTable

-- -----------------------------------------------------------------------------

registerName :: Bool -> Location -> NameEnvironment -> Text -> Parser Name
registerName redefOk loc env name =
  do state <- getPassState
     let key = (env, name)
     case Map.lookup key (view psNameDatabase state) of
       Nothing ->
         do let res = Name loc env (view psNextIdent state) name
            overPassState (over psNameDatabase (Map.insert key res) .
                           over psNextIdent (+1))
            return res
       Just res | redefOk ->
         return res
       Just (Name origLoc _ _ _) ->
         err (RedefinitionError loc origLoc name)

unregisterNames :: NameEnvironment -> [Name] -> Parser ()
unregisterNames env names =
  do db <- viewPassState psNameDatabase
     let db' = foldr (\ (Name _ _ _ n) m -> Map.delete (env, n) m) db names
     overPassState (set psNameDatabase db')

lookupName :: Location -> NameEnvironment -> Text -> Parser Name
lookupName loc env name =
  do state <- getPassState
     case Map.lookup (env, name) (view psNameDatabase state) of
       Nothing ->
         err (UnboundVariable loc name)
       Just realName ->
         return realName

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
setLexerState lst = overPassState (set psLexerState lst)

-- -----------------------------------------------------------------------------

parseError :: Located Token -> Parser a
parseError t =
  case t of
    Located _ EOFTok         -> err UnexpectedEOF
    Located p (ErrorTok tok) -> err (LexError p tok)
    Located p tok            -> err (ParseError p tok)



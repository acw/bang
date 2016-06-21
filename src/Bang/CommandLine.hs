{-# LANGUAGE TemplateHaskell #-}
module Bang.CommandLine(
         Verbosity(..)
       , CommandsWithInputFile(..)
       , CommandsWithOutputFile(..)
       , CommandsWithVerbosity(..)
       , BangCommand(..)
       , LexerOptions(..)
       , ParserOptions(..)
       , getCommand
       , helpString
       )
 where

import Control.Applicative((<|>))
import Control.Lens(Lens')
import Control.Lens.TH(makeLenses)
import Data.Monoid((<>))
import Options.Applicative(Parser, ParserInfo, ParserPrefs(..), flag,
                           short, long, strOption, command, subparser, info,
                           progDesc, execParser, helper, metavar, str, argument,
                           showDefault, value, help)
import Options.Applicative.Help(parserHelp)

class CommandsWithInputFile opts where
  inputFile :: Lens' opts FilePath

class CommandsWithOutputFile opts where
  outputFile :: Lens' opts FilePath

class CommandsWithVerbosity opts where
  verbosity :: Lens' opts Verbosity

-- -----------------------------------------------------------------------------

data Verbosity = Silent | Normal | Verbose
 deriving (Eq, Show)

verboseOption :: Parser Verbosity
verboseOption = flag Normal Silent  (short 'q' <> long "quiet")
            <|> flag Normal Verbose (short 'v' <> long "verbose")

optOutputFile :: Parser FilePath
optOutputFile = strOption (short 'o' <> long "output-file" <> metavar "FILE"
                      <> help "The file to output as a result of this action."
                      <> value "/dev/stdout" <> showDefault)

-- -----------------------------------------------------------------------------

data LexerOptions = LexerOptions {
       _lexInputFile  :: FilePath
     , _lexOutputFile :: FilePath
     , _lexVerbosity  :: Verbosity
     }
 deriving (Show)

makeLenses ''LexerOptions

parseLexOptions :: Parser LexerOptions
parseLexOptions = LexerOptions <$> argument str (metavar "FILE")
                               <*> optOutputFile
                               <*> verboseOption


instance CommandsWithInputFile LexerOptions where
  inputFile = lexInputFile

instance CommandsWithOutputFile LexerOptions where
  outputFile = lexOutputFile

instance CommandsWithVerbosity LexerOptions where
  verbosity = lexVerbosity

-- -----------------------------------------------------------------------------

data ParserOptions = ParserOptions {
       _parseInputFile  :: FilePath
     , _parseOutputFile :: FilePath
     , _parseVerbosity  :: Verbosity
     }
 deriving (Show)

makeLenses ''ParserOptions

parseParseOptions :: Parser ParserOptions
parseParseOptions = ParserOptions <$> argument str (metavar "FILE")
                                  <*> optOutputFile
                                  <*> verboseOption

instance CommandsWithInputFile ParserOptions where
  inputFile = parseInputFile

instance CommandsWithOutputFile ParserOptions where
  outputFile = parseOutputFile

instance CommandsWithVerbosity ParserOptions where
  verbosity = parseVerbosity

-- -----------------------------------------------------------------------------

data TypeCheckOptions = TypeCheckOptions {
       _tcheckInputFile  :: FilePath
     , _tcheckOutputFile :: FilePath
     , _tcheckVerbosity  :: Verbosity
     }
 deriving (Show)

makeLenses ''TypeCheckOptions

parseTypeCheckOptions :: Parser TypeCheckOptions
parseTypeCheckOptions = TypeCheckOptions <$> argument str (metavar "FILE")
                                         <*> optOutputFile
                                         <*> verboseOption

instance CommandsWithInputFile TypeCheckOptions where
  inputFile = tcheckInputFile

instance CommandsWithOutputFile TypeCheckOptions where
  outputFile = tcheckOutputFile

instance CommandsWithVerbosity TypeCheckOptions where
  verbosity = tcheckVerbosity

-- -----------------------------------------------------------------------------

data BangCommand = Help
                 | Lex       LexerOptions
                 | Parse     ParserOptions
                 | TypeCheck TypeCheckOptions
                 | Version
 deriving (Show)

bangOperation :: Parser BangCommand
bangOperation = subparser $
  command "help"      (pure Help    `withInfo` "Describe common commands.") <>
  command "version"   (pure Version `withInfo` "Display version information.") <>
  command "lex"       (parseLex     `withInfo` "Lex a file into its component tokens.") <>
  command "parse"     (parseParse   `withInfo` "Parse a file into its AST.") <>
  command "typeCheck" (parseTCheck  `withInfo` "Type check a file.")
 where
  parseLex    = Lex       <$> parseLexOptions
  parseParse  = Parse     <$> parseParseOptions
  parseTCheck = TypeCheck <$> parseTypeCheckOptions

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) (progDesc desc)

helpString :: String
helpString = show (parserHelp (ParserPrefs "" False False True 80) bangOperation)

getCommand :: IO BangCommand
getCommand = execParser (bangOperation `withInfo` "Run a bang language action.")

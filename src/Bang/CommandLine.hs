module Bang.CommandLine(
         BangCommand(..)
       , BangOperation(..)
       , LexerOptions(..)
       , getCommand
       , helpString
       )
 where

import Options.Applicative
import Options.Applicative.Help

data BangCommand = BangCommand {
       cmdVerbosity  :: Verbosity
     , cmdOutputFile :: FilePath
     , cmdCommand    :: BangOperation
     }
 deriving (Show)

data Verbosity = Silent | Normal | Verbose
 deriving (Eq, Show)

verboseOption :: Parser Verbosity
verboseOption = flag Normal Silent  (short 'q' <> long "quiet")
            <|> flag Normal Verbose (short 'v' <> long "verbose")

outputFile :: Parser FilePath
outputFile = strOption (short 'o' <> long "output-file" <> metavar "FILE"
                      <> help "The file to output as a result of this action."
                      <> value "/dev/stdout" <> showDefault)

data BangOperation = Help
                   | Version
                   | Lex LexerOptions
 deriving (Show)

bangOperation :: Parser BangOperation
bangOperation = subparser $
  command "help" (pure Help `withInfo` "Describe common commands.") <>
  command "version" (pure Version `withInfo` "Display version information.") <>
  command "lex" (parseLex `withInfo` "Lex a file into its component tokens.")

withInfo :: Parser a -> String -> ParserInfo a 
withInfo opts desc = info (helper <*> opts) (progDesc desc)

data LexerOptions = LexerOptions {
       lexInputFile :: FilePath
     }
 deriving (Show)

parseLex :: Parser BangOperation
parseLex = Lex <$> parseLexOptions

parseLexOptions :: Parser LexerOptions
parseLexOptions = LexerOptions <$> argument str (metavar "FILE")

parseOptions :: Parser BangCommand
parseOptions = BangCommand <$> verboseOption <*> outputFile <*> bangOperation

helpString :: String
helpString = show (parserHelp (ParserPrefs "" False False True 80) parseOptions)

getCommand :: IO BangCommand
getCommand = execParser (parseOptions `withInfo` "Run a bang language action.")

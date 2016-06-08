import           Bang.CommandLine
import           Bang.Syntax.Lexer
import           Bang.Syntax.Location
import           Bang.Syntax.Parser
import           Control.Exception(tryJust)
import           Control.Monad(guard)
import qualified Data.Text.Lazy.IO as T
import           Data.Version(showVersion)
import           Paths_bang(version)
import           System.IO.Error(isDoesNotExistError)

main :: IO ()
main = getCommand >>= \ cmd ->
  case cmdCommand cmd of
    Lex   o -> runLexer  cmd o
    Parse o -> runParser cmd o
    Help    -> putStrLn helpString
    Version -> putStrLn ("Bang tool, version " ++ showVersion version)

runLexer :: BangCommand -> LexerOptions -> IO ()
runLexer _cmd opts =
  do let path = lexInputFile opts
     mtxt <- tryJust (guard . isDoesNotExistError) (T.readFile path)
     case mtxt of
       Left _    -> fail ("Unable to open file: " ++ path)
       Right txt ->
         do let tokens = lexer (File path) (Just initialPosition) txt
            mapM_ (putStrLn . show) tokens

runParser :: BangCommand -> ParserOptions -> IO ()
runParser _cmd opts =
  do let path = parseInputFile opts
     mtxt <- tryJust (guard . isDoesNotExistError) (T.readFile path)
     case mtxt of
       Left  _   -> fail ("Unable to open file: " ++ path)
       Right txt ->
         do let mod = parseModule (File path) txt
            putStrLn (show mod)

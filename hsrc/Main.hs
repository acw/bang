import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import System.Environment
import System.Exit
import System.IO.Error

import Syntax.AST
import Syntax.Parser
import Syntax.ParserCore

main :: IO ()
main = do
  [file] <- getArgs
  ast <- loadModule file
  putStrLn "Successful parse!"
  putStrLn (show ast)

loadModule :: FilePath -> IO (Module ())
loadModule path = do
  mtxt <- tryJust (guard . isDoesNotExistError) $ S.readFile path
  case mtxt of
    Left _ -> fail $ "Unable to open file: " ++ path
    Right txt ->
      case runParser path txt parseModule of
        Left err  -> printError err >> exitWith (ExitFailure 1)
        Right ast -> return ast

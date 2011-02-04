import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import System.Environment
import System.Exit
import System.IO.Error

import Syntax.AST
import Syntax.Lexer
import Syntax.Parser
import Syntax.ParserCore

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file]        -> do
      ast <- loadModule file
      putStrLn "Successful parse!"
      putStrLn (show ast)
    ["-lex",path] -> do
      mtxt <- tryJust (guard . isDoesNotExistError) $ S.readFile path
      case mtxt of
        Left _    -> fail $ "Unable to open file: " ++ path
        Right txt -> do
          case runParser path txt pullTokens of
            Left err -> printError err >> exitWith (ExitFailure 1)
            Right ress -> do
              mapM_ putStrLn ress
              putStrLn "Successful lex."
    _ -> fail "Unacceptable arguments."

pullTokens :: Parser [String]
pullTokens = do
  tok <- scan
  case tok of
    Lexeme pos tok' -> do
      let res = show pos ++ " " ++ show tok'
      if tok' == TokEOF
        then return [res]
        else return (res :) `ap` pullTokens

loadModule :: FilePath -> IO (Module Position)
loadModule path = do
  mtxt <- tryJust (guard . isDoesNotExistError) $ S.readFile path
  case mtxt of
    Left _ -> fail $ "Unable to open file: " ++ path
    Right txt ->
      case runParser path txt parseModule of
        Left err  -> printError err >> exitWith (ExitFailure 1)
        Right ast -> return ast

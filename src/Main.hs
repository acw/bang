import           Bang.CommandLine
import           Bang.Error(exit)
import           Bang.Monad
import           Bang.Syntax.AST(Module)
import           Bang.Syntax.Lexer(lexer)
import           Bang.Syntax.Location
import           Bang.Syntax.Parser(parseModule)
import           Bang.Syntax.Pretty(ppModule)
import           Bang.TypeInfer(typeInfer)
import           Control.Exception(tryJust)
import           Control.Lens(view)
import           Control.Monad(guard)
import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy.IO as T
import           Data.Version(showVersion)
import           Paths_bang(version)
import           System.IO.Error(isDoesNotExistError)
import           Text.PrettyPrint.Annotated(render)

main :: IO ()
main = getCommand >>= \ cmd ->
  case cmd of
    Lex       o -> run o $ \ path body ->
                     do let ts = lexer (File path) (Just initialPosition) body
                        mapM_ (putStrLn . show) ts
    Parse     o -> run o $ withParsed $ \ mdl ->
                     putStrLn (render (ppModule mdl))
    TypeCheck o -> run o $ withParsed $ withInferred $ \ mdl ->
                     putStrLn (render (ppModule mdl))
--    Compile   o -> run o $ withParsed $ withInferred $ \ mod ->
--                     putStrLn (render (ppModule mod))
    Help        -> putStrLn helpString
    Version     -> putStrLn ("Bang tool, version " ++ showVersion version)

run :: CommandsWithInputFile o => o -> (FilePath -> Text -> IO ()) -> IO ()
run opts action =
  do let path = view inputFile opts
     mtxt <- tryJust (guard . isDoesNotExistError) (T.readFile path)
     case mtxt of
       Left  _   -> exit ("Unable to open file '" ++ path ++ "'")
       Right txt -> action path txt

withParsed :: (Module -> IO ()) -> FilePath -> Text -> IO ()
withParsed action path body =
  case parseModule (File path) body of
    Left  err -> exit (show err)
    Right mdl -> action mdl

withInferred :: (Module -> IO ()) -> Module -> IO ()
withInferred action mdl =
  case typeInfer 0 mdl of
    Left err   -> exit (show err)
    Right mdl' -> action mdl'

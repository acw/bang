import           Bang.CommandLine
import           Bang.Monad
import           Bang.Syntax.Lexer()
import           Bang.Syntax.Parser(runParser, parseModule)
import           Bang.Syntax.Pretty(ppModule)
import           Data.Version(showVersion)
import           Paths_bang(version)
import           Text.PrettyPrint.Annotated(render)

main :: IO ()
main = getCommand >>= \ cmd ->
  case cmd of
    Parse     o -> do mdl <- runCompiler cmd o (\ r t -> runParser r t parseModule)
                      putStrLn (render (ppModule mdl))
    TypeCheck _ -> undefined
    Help        -> putStrLn helpString
    Version     -> putStrLn ("Bang tool, version " ++ showVersion version)

-- run :: CommandsWithInputFile o => o -> (FilePath -> Text -> IO ()) -> IO ()
-- run opts action =
--   do let path = view inputFile opts
--      mtxt <- tryJust (guard . isDoesNotExistError) (T.readFile path)
--      case mtxt of
--        Left  _   -> exit ("Unable to open file '" ++ path ++ "'")
--        Right txt -> action path txt
-- 
-- withParsed :: (Module -> IO ()) -> FilePath -> Text -> IO ()
-- withParsed action path body =
--   case parseModule (File path) body of
--     Left  err -> exit (show err)
--     Right mdl -> action mdl
-- 
-- withInferred :: (Module -> IO ()) -> Module -> IO ()
-- withInferred action mdl =
--   case typeInfer 0 mdl of
--     Left err   -> exit (show err)
--     Right mdl' -> action mdl'

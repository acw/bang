import           Bang.CommandLine(getCommand, BangCommand(..), helpString)
import           Bang.AST(ppModule)
import           Bang.Monad(runCompiler)
import           Bang.Syntax.Parser(runParser, parseModule)
import           Bang.TypeInfer(runTypeInference)
import           Data.Version(showVersion)
import           Paths_bang(version)
import           Text.PrettyPrint.Annotated(render)

main :: IO ()
main = getCommand >>= \ cmd ->
  case cmd of
    Parse     o -> do (_, mdl) <- runCompiler cmd o (\ r t -> runParser r t parseModule)
                      putStrLn (render (ppModule mdl))
    TypeCheck o -> do mdl <- runCompiler cmd o (\ r t ->
                                                 do (ndb, mdl) <- runParser r t parseModule
                                                    runTypeInference ndb mdl)
                      putStrLn (render (ppModule mdl))
    Help        -> putStrLn helpString
    Version     -> putStrLn ("Bang tool, version " ++ showVersion version)

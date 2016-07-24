import           Bang.CommandLine(getCommand, BangCommand(..), helpString)
import           Bang.AST(ppModule)
import           Bang.Monad(runCompiler)
import           Bang.Syntax.Parser(runParser, parseModule)
import           Bang.Syntax.PostProcess(runPostProcessor)
import           Bang.TypeInfer(runTypeInference)
import           Data.Version(showVersion)
import           Paths_bang(version)
import           Text.PrettyPrint.Annotated(render)

main :: IO ()
main = getCommand >>= \ cmd ->
  case cmd of
    Parse     o -> do mdl <- runCompiler cmd o (\ r t -> runParser r t parseModule)
                      putStrLn (render (ppModule mdl))
    TypeCheck o -> do mdl <- runCompiler cmd o (\ r t ->
                                                 do mdl  <- runParser r t parseModule
                                                    mdl' <- runPostProcessor mdl
                                                    runTypeInference mdl')
                      putStrLn (render (ppModule mdl))
    Help        -> putStrLn helpString
    Version     -> putStrLn ("Bang tool, version " ++ showVersion version)

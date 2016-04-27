{-# LANGUAGE DeriveDataTypeable #-}
module Syntax.CommandLine(
         Bang(..), getCommand
       )
 where

import Data.Version(showVersion)
import Paths_bang(version)
import System.Console.CmdArgs hiding (verbosity)

data Bang = Bang {
       verbosity :: Word
     , files     :: [FilePath]
     , mode      :: BangMode
     }
 deriving (Data, Typeable, Show, Eq)

data BangMode = Lex | Parse
 deriving (Data, Typeable, Show, Eq)

lexer :: Bang
lexer = Bang {
         verbosity = 0  &= name "verbose" &= name "v"
       , files     = [] &= args
       , mode      = Lex
       } &= name "lex"

parser :: Bang
parser = Bang {
         verbosity = 0  &= name "verbose" &= name "v"
       , files     = [] &= args
       , mode      = Parse
       } &= name "parse"

bang :: Bang
bang = modes [lexer, parser]
  &= versionArg [explicit, name "version", summary programInfo]
  &= summary (programInfo ++ ", " ++ copyright)
  &= help "A nifty little compiler for a new language"
  &= helpArg [explicit, name "help", name "h", name "?"]
  &= program "bang"
 where
  programInfo = "bang version " ++ showVersion version
  copyright = "(C) 2016 Adam Wick"

getCommand :: IO Bang
getCommand = cmdArgs bang

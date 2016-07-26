{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Bang.Monad(
         Compiler
       , BangError(..)
       , BangWarning(..)
       , runCompiler
       , runPass
       , getPassState, setPassState, mapPassState, overPassState, viewPassState
       , registerName, registerNewName, genName, genTypeRef, genVarRef
       , warn, err, err'
       )
 where

import           Bang.AST.Expression(Expression, mkRefExp)
import           Bang.AST.Name(NameEnvironment(..), Name, mkName, nameIndex)
import           Bang.AST.Type(Kind(..), Type, mkTypeRef)
import           Bang.CommandLine(BangCommand, CommandsWithInputFile(..))
import           Bang.Error(exit)
import           Bang.Syntax.Location(Location(..), Origin(..),
                                      unknownLocation, ppLocation)
import           Bang.Utils.Pretty(BangDoc)
import           Control.Exception(tryJust)
import           Control.Lens(Lens', over, set, view)
import           Control.Lens.TH(makeLenses)
import           Control.Monad(guard, when)
import           Data.Text.Lazy(Text, pack)
import qualified Data.Text.Lazy.IO as T
import           System.Exit(ExitCode(..), exitWith)
import           System.IO.Error(isDoesNotExistError)
import           Text.PrettyPrint.Annotated(text, ($+$), nest, render)

class BangError e where
  ppError :: e -> (Maybe Location, BangDoc)

class BangWarning w where
  ppWarning :: w -> (Maybe Location, BangDoc)

data CompilerState state = CompilerState {
       _csNextIdent       :: !Word
     , _csPromoteWarnings :: !Bool
     , _csWarnings        :: [BangDoc]
     , _csPassState       :: !state
     }

makeLenses ''CompilerState

initialState :: BangCommand -> CompilerState ()
initialState _ = CompilerState 1 False [] ()

-- -----------------------------------------------------------------------------

newtype Compiler s a =
  Compiler { unCompiler :: CompilerState s -> IO (CompilerState s, a) }

instance Applicative (Compiler s) where
  pure a = Compiler (\ st -> return (st, a))
  mf <*> ma = Compiler (\ st ->
                          do (st', f) <- unCompiler mf st 
                             (st'', a) <- unCompiler ma st'
                             return (st'', f a))

instance Functor (Compiler s) where
  fmap f m = return f <*> m

instance Monad (Compiler s) where
  return a = Compiler (\ st -> return (st, a))
  m >>= k  = Compiler (\ st ->
                         do (st', a) <- unCompiler m st
                            unCompiler (k a) st')

runCompiler :: CommandsWithInputFile o =>
               BangCommand -> o ->
               (Origin -> Text -> Compiler () a) ->
               IO a
runCompiler cmd opts action =
  do let path = view inputFile opts
         orig = File path
     mtxt <- tryJust (guard . isDoesNotExistError) (T.readFile path)
     case mtxt of
       Left  _   -> exit ("Unable to open file '" ++ path ++ "'")
       Right txt -> snd `fmap` unCompiler (action orig txt) (initialState cmd)

runPass :: s2 -> (Compiler s2 a) -> Compiler s1 (s2, a)
runPass s2 action =
  Compiler (\ cst1 ->
             do let cst2 = set csPassState s2 cst1
                    s1   = view csPassState cst1
                (cst2', v) <- unCompiler action cst2
                let retval = (view csPassState cst2', v)
                return (set csPassState s1 cst2', retval))

getPassState :: Compiler s s
getPassState = Compiler (\ st -> return (st, view csPassState st))

setPassState :: Lens' s b -> b -> Compiler s ()
setPassState passLens v =
  Compiler (\ st -> return (set (csPassState . passLens) v st, ()))

mapPassState :: (s -> s) -> Compiler s ()
mapPassState f = Compiler (\ st -> return (over csPassState f st, ()))

overPassState :: Lens' s b -> (b -> b) -> Compiler s ()
overPassState passLens f =
  Compiler (\ st -> return (over (csPassState . passLens) f st, ()))

viewPassState :: Lens' s b -> Compiler s b
viewPassState l = Compiler (\ st -> return (st, view (csPassState . l) st))

-- -----------------------------------------------------------------------------

registerName :: Name -> Compiler s Name
registerName name =
  Compiler (\ st ->
             do let current = view csNextIdent st
                return (over csNextIdent (+1) st, set nameIndex current name))

registerNewName :: NameEnvironment -> Text -> Compiler s Name
registerNewName env name =
  Compiler (\ st ->
              do let current = view csNextIdent st
                     res     = mkName name env unknownLocation current
                 return (over csNextIdent (+1) st, res))

genName :: NameEnvironment -> Compiler s Name
genName env =
  Compiler (\ st ->
             do let current = view csNextIdent st
                    str = "gen:" ++ show current
                    res = mkName (pack str) env unknownLocation current
                return (over csNextIdent (+1) st, res))

genTypeRef :: Kind -> Compiler s Type
genTypeRef k = mkTypeRef unknownLocation k `fmap` genName TypeEnv

genVarRef :: Compiler s Expression
genVarRef = mkRefExp unknownLocation `fmap` genName VarEnv

-- -----------------------------------------------------------------------------

data WErrorWarning w = WErrorWarning w

instance BangWarning w => BangError (WErrorWarning w) where
  ppError (WErrorWarning w) =
    let (loc, wdoc) = ppWarning w
        edoc        = text "Warning lifted to error by -WError:" $+$ nest 3 wdoc
    in (loc, edoc)

warn :: BangWarning w => w -> Compiler s ()
warn w = Compiler (\ st ->
                     if view csPromoteWarnings st
                        then runError (WErrorWarning w) False >> return (st, ())
                        else runWarning w >> return (st, ()))

err :: BangError w => w -> Compiler s a
err w = Compiler (\ _ -> runError w True >> undefined)

err' :: BangError e => e -> Compiler s ()
err' e = Compiler (\ st -> runError e False >> return (st, ()))

runWarning :: BangWarning w => w -> IO ()
runWarning w = putStrLn (go (ppWarning w))
 where
   go (Nothing, doc) = render doc
   go (Just a, doc) = render (ppLocation a $+$ nest 3 doc)

runError :: BangError w => w -> Bool -> IO ()
runError e die =
  do putStrLn (go (ppError e))
     when die $ exitWith (ExitFailure 1)
 where
   go (Nothing, doc) = render doc
   go (Just a, doc) = render (ppLocation a $+$ nest 3 doc)

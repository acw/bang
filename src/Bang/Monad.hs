{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
module Bang.Monad(
         Compiler
       , (==>), (==>|)
       , genName, genTypeRef, genVarRef
       , warn, err
       )
 where

import Bang.Syntax.AST
import Bang.Syntax.Location(unknownLocation)
import Bang.Utils.Pretty(BangDoc)
import Data.Text.Lazy(pack)
import MonadLib
import Text.PrettyPrint.Annotated(Doc)

class BangError e where
  ppError :: e -> BangDoc

class BangWarning w where
  ppWarning :: w -> BangDoc

instance BangWarning w => BangError w where
  ppError = ppWarning

data CompilerState state = CompilerState {
       csNextIdent       :: Word
     , csPromoteWarnings :: Bool
     , csWarnings        :: [BangDoc]
     , csPassState       :: state
     }

initialState :: CompilerState ()
initialState = CompilerState 1 False [] ()

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

class PassTransition s1 s2 where
  transition :: s1 -> s2

(==>) :: PassTransition s1 s2 =>
         Compiler s1 a ->
         (a -> Compiler s2 b) ->
         Compiler s1 b
m1 ==> k = Compiler (\ st ->
                        do (st', a) <- unCompiler m1 st
                           let next = k a
                               ps'  = transition (csPassState st')
                               st'' = st'{ csPassState = ps' }
                           (_, b) <- unCompiler next st''
                           return (st', b))

(==>|) :: PassTransition s1 s2 =>
          Compiler s1 a ->
          Compiler s2 b ->
          Compiler s1 b
m1 ==>| m2 = m1 ==> (const m2)

genName :: NameEnvironment -> Compiler s Name
genName env = Compiler (\ st ->
                         do let current = csNextIdent st
                                str = "gen:" ++ show current
                                res = Name unknownLocation env current (pack str)
                            return (st{ csNextIdent = current + 1 }, res))

genTypeRef :: Kind -> Compiler s Type
genTypeRef k = TypeRef unknownLocation k `fmap` genName TypeEnv

genVarRef :: Compiler s Expression
genVarRef = ReferenceExp unknownLocation `fmap` genName VarEnv

warn :: BangWarning w => w -> Compiler s ()
warn w = Compiler (\ st ->
                     if csPromoteWarnings st
                        then runError w
                        else runWarning w >> return (st, ()))

err :: BangError w => w -> Compiler s a
err w = Compiler (\ _ -> runError w)

runWarning :: BangWarning w => w -> IO ()
runWarning = undefined

runError :: BangError w => w -> IO a
runError = undefined


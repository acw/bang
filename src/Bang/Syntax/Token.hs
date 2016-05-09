module Bang.Syntax.Token(
         Token(..)
       , Fixity(..)
       , showToken
       )
 where

import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as T

data Token = CharTok   Text
           | FloatTok  Text
           | IntTok    Word    Text
           | OpIdent   Fixity  Text
           | Special   Text
           | StringTok Text
           | TypeIdent Text
           | ValIdent  Text
           | ErrorTok  Text
           | EOFTok
 deriving (Show)

data Fixity = LeftAssoc  Word
            | RightAssoc Word
            | NonAssoc   Word
 deriving (Show)

showToken :: Token -> String
showToken (CharTok   t)   = "'" ++ T.unpack t ++ "'"
showToken (FloatTok  t)   = T.unpack t
showToken (IntTok    _ t) = T.unpack t
showToken (OpIdent   _ t) = T.unpack t
showToken (Special   t)   = T.unpack t
showToken (StringTok t)   = "\"" ++ T.unpack t ++ "\""
showToken (TypeIdent t)   = T.unpack t
showToken (ValIdent  t)   = T.unpack t
showToken (ErrorTok  t)   = "ERROR(" ++ T.unpack t ++ ")"
showToken EOFTok          = "EOF"

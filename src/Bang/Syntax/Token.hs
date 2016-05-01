module Bang.Syntax.Token(
         Token(..)
       )
 where

import Data.Text.Lazy(Text)

data Token = CharTok   Text
           | FloatTok  Text
           | IntTok    Word    Text
           | OpIdent   Text
           | Special   Text
           | StringTok Text
           | TypeIdent Text
           | ValIdent  Text
           | ErrorTok  Text
           | EOFTok
 deriving (Show)

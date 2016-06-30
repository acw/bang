module Bang.Syntax.Token(
         Token(..)
       , Fixity(..)
       , ppToken
       )
 where

import Bang.Utils.Pretty(BangDoc, text')
import Data.Monoid((<>))
import Data.Text.Lazy(Text)
import Text.PrettyPrint.Annotated(quotes, doubleQuotes, text, parens)

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

ppToken :: Token -> BangDoc
ppToken (CharTok   t)   = quotes (text' t)
ppToken (FloatTok  t)   = text' t
ppToken (IntTok    _ t) = text' t
ppToken (OpIdent   _ t) = text' t
ppToken (Special   t)   = text' t
ppToken (StringTok t)   = doubleQuotes (text' t)
ppToken (TypeIdent t)   = text' t
ppToken (ValIdent  t)   = text' t
ppToken (ErrorTok  t)   = text "ERROR" <> parens (text' t)
ppToken EOFTok          = text "<EOF>"

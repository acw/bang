-- -*- mode: haskell -*-
-- vi: set ft=haskell :
{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w                 #-}
module Bang.Syntax.Lexer(
         AlexReturn(..)
       , AlexInput(..)
       , alexScan
       )
 where

import           Bang.Syntax.Location(Location(..), Located(..), Origin(..),
                                      Position(..), advanceWith, advanceWith',
                                      locatedAt, initialPosition)
import           Bang.Syntax.Token(Token(..), Fixity(..))
import           Data.Char(isAscii, ord)
import           Data.Int(Int64)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe(fromMaybe)
import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as T
import           Data.Word(Word8)

}

-- Digits
$decdigit = 0-9
$hexdigit = [0-9a-fA-f]
$octdigit = 0-7
$bindigit = [01]

-- Identifier Characters
$typestart   = [A-Z\_]
$valstart    = [a-z\_]
$identrest   = [a-zA-Z0-9\_\.]
$opident     = [\~\!\@\#\$\%\^\&\*\+\-\=\.\<\>\?\_\|:]
$escape_char = [abfnrtv'\"\\] --"

:-

-- Whitespace
  $white+                                          ;
  "/*"[.\n]*"*/"                                   ;

-- Numbers
  $decdigit+                                       { emitI 0 (IntTok 10)  }
  "0x"$hexdigit+                                   { emitI 2 (IntTok 16) }
  "0o"$octdigit+                                   { emitI 2 (IntTok 8)  }
  "0b"$bindigit+                                   { emitI 2 (IntTok 2)  }
  $decdigit+"."$decdigit+ ("e""-"?$decdigit+)?     { emitS FloatTok}
  $decdigit+"e""-"?$decdigit+                      { emitS FloatTok}

-- Identifier
  $typestart $identrest*                           { emitS TypeIdent               }
  $valstart $identrest*                            { emitS ValIdent                }
  $opident+                                        { emitO                         }

-- Characters and Strings
  ['].[']                                          { emitS CharTok }
  ['] [\\] $escape_char [']                        { emitS CharTok }
  [\"] ([^\"] | [\n] | ([\\] $escape_char))* [\"]  { emitS StringTok } --"

-- Symbols
  "("                                             { emitT "("     }
  ")"                                             { emitT ")"     }
  "["                                             { emitT "["     }
  "]"                                             { emitT "]"     }
  "{"                                             { emitT "{"     }
  "}"                                             { emitT "}"     }
  ";"                                             { emitT ";"     }
  ","                                             { emitT ","     }
  "`"                                             { emitT "`"     }
  [\\]                                            { emitT "\\"    }

{

type AlexAction = Origin -> Map Text Fixity -> Int -> AlexInput -> Located Token

data AlexInput = AlexInput !Position Text

emitT :: Text -> AlexAction
emitT t = emitS (const (Special t))

emitS :: (Text -> Token) -> AlexAction
emitS mk src _ len (AlexInput pos t) = token `locatedAt` loc
 where
  txt   = T.take (fromIntegral len) t
  token = mk txt
  loc   = Location src pos (pos `advanceWith'` txt)

emitI :: Int64 -> (Text -> Token) -> AlexAction
emitI dropCount mk src _ len (AlexInput pos t) = token `locatedAt` loc
 where
  baseText = T.take (fromIntegral len) t
  txt      = T.drop dropCount baseText
  token    = mk txt
  loc      = Location src pos (pos `advanceWith'` baseText)

emitO :: AlexAction
emitO src fixTable len (AlexInput pos t) =
  case Map.lookup baseText fixTable of
    Nothing -> OpIdent (LeftAssoc 9) baseText `locatedAt` loc
    Just f  -> OpIdent f             baseText `locatedAt` loc
 where
  baseText = T.take (fromIntegral len) t
  loc      = Location src pos (pos `advanceWith'` baseText)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput p t) =
  do (c, rest) <- T.uncons t
     return (byteForChar c, (AlexInput (p `advanceWith` c) rest))
 where
  byteForChar c | isAscii c = fromIntegral (ord c)
                | otherwise = 0

}

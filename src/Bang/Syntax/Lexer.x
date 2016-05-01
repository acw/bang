{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w                 #-}
module Bang.Syntax.Lexer(lexer)
 where

import           Bang.Syntax.Location
import           Bang.Syntax.Name
import           Bang.Syntax.Token
import           Data.Char(isSpace, isAscii, ord)
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
$escape_char = [abfnrtv'\"\\]

:-

-- Whitespace
  $white+                                          ;
  "/*".*"*/"                                       ;

-- Numbers
  $decdigit+                                       { emitS (IntTok 10)  }
  "0x"$hexdigit+                                   { emitS (IntTok 16) }
  "0o"$octdigit+                                   { emitS (IntTok 8)  }
  "0b"$bindigit+                                   { emitS (IntTok 2)  }
  $decdigit+"."$decdigit+ ("e""-"?$decdigit+)?     { emitS FloatTok}
  $decdigit+"e""-"?$decdigit+                      { emitS FloatTok}

-- Identifier
  $typestart $identrest*                           { emitS TypeIdent    }
  $valstart $identrest*                            { emitS ValIdent     }
  $opident+                                        { emitS OpIdent      }

-- Characters and Strings
  ['].[']                                          { emitS CharTok }
  ['] [\\] $escape_char [']                        { emitS CharTok }
  [\"] ([^\"] | [\n] | ([\\] $escape_char))* [\"]  { emitS StringTok }

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

lexer :: Origin -> Maybe Position -> Text -> [Located Token]
lexer src mbPos txt = go (AlexInput startPos txt)
 where
  startPos = fromMaybe initialPosition mbPos
  go input =
    case alexScan input 0 of
       AlexEOF                  -> []
       AlexError input'         -> let AlexInput pos text = input'
                                       (as, bs) = T.break isSpace text
                                       pos' = advanceWith' pos as
                                       input'' = AlexInput pos' bs
                                       loc = Location src pos pos'
                                   in (ErrorTok as `locatedAt` loc) : go input''
       AlexSkip  input' _       -> go input'
       AlexToken input' len act -> act src len input  : go input'

data AlexInput = AlexInput !Position Text

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput p t) =
  do (c, rest) <- T.uncons t
     return (byteForChar c, (AlexInput (p `advanceWith` c) rest))
 where
  byteForChar c | isAscii c = fromIntegral (ord c)
                | otherwise = 0

type AlexAction = Origin -> Int -> AlexInput -> Located Token

emitT :: Text -> AlexAction
emitT str = emitS (const (Special str))

emitS :: (Text -> Token) -> AlexAction
emitS mk src len (AlexInput pos t) = token `locatedAt` loc
 where
  txt   = T.take (fromIntegral len) t
  token = mk txt
  loc   = Location src pos (pos `advanceWith'` txt)

}

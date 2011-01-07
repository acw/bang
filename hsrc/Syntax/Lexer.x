{
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -w #-}
module Syntax.Lexer where

import qualified Codec.Binary.UTF8.Generic as UTF8
import qualified Data.ByteString as S
import MonadLib

import Syntax.ParserCore

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
$opident     = [\~\!\@\#\$\%\^\&\*\+\-\=\.\:\<\>\?\/\_]
$escape_char = [abfnrtv'\"\\]

:-

-- Whitespace
  $white+                              ;
  "--".*                               ;

-- Numbers
  $decdigit+                                       { emitS (buildInt 10)  }
  "0x"$hexdigit+                                   { emitS (buildInt 16) }
  "0o"$octdigit+                                   { emitS (buildInt 8)  }
  "0b"$bindigit+                                   { emitS (buildInt 2)  }
  $decdigit+"."$decdigit+ ("e""-"?$decdigit+)?     { emitS TokFloat}
  $decdigit+"e""-"?$decdigit+                      { emitS TokFloat}

-- Identifier
  $typestart $identrest*                           { emitS TokTypeIdent }
  $valstart $identrest*                            { emitS TokValIdent  }
  $opident+                                        { emitS TokOpIdent   }

-- Characters and Strings
  ['].[']                                          { emitS TokChar }
  ['] [\\] $escape_char [']                        { emitS TokChar }
  [\"] ([^\"] | [\n] | ([\\] $escape_char))* [\"]  { emitS TokString }

-- Symbols
  "("                                             { emitT LParen  }
  ")"                                             { emitT RParen  }
  "["                                             { emitT LSquare }
  "]"                                             { emitT RSquare }
  "{"                                             { emitT LBrace  }
  "}"                                             { emitT RBrace  }
  "|"                                             { emitT Bar     }
  ";"                                             { emitT Semi    }
  ","                                             { emitT Comma   }

{

type AlexInput = (Position,Char,S.ByteString)

emitT :: Token -> AlexInput -> Int -> Parser Lexeme
emitT tok (pos,_,_) _ = return $! Lexeme pos tok

emitS :: (String -> Token) -> AlexInput -> Int -> Parser Lexeme
emitS mk (pos,c,bs) len = return $! Lexeme pos (mk input)
 where input = UTF8.toString (S.take len bs)

scan :: Parser Lexeme
scan  = do
  inp@(pos,_,_) <- alexGetInput
  sc            <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF                   -> return $! Lexeme pos TokEOF
    AlexError inp'            -> do
      let posStr = pprtPosition pos
      alexError $ posStr ++ ": Lexical error."
    AlexSkip inp' len'        -> alexSetInput inp' >> scan
    AlexToken inp' len action -> do
      alexSetInput inp'
      action inp len

alexGetInput :: Parser AlexInput
alexGetInput = do
  s <- get
  return (psPos s, psChar s, psInput s)

alexSetInput :: AlexInput -> Parser ()
alexSetInput (pos,c,bs) = do
  s <- get
  set $! s {
    psPos   = pos
  , psChar  = c
  , psInput = bs
  }

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,bs) = do
  (c,bs') <- UTF8.uncons bs
  return (c, (movePos p c, c, bs'))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

alexError :: String -> Parser a
alexError  = raiseL

alexGetStartCode :: Parser Int
alexGetStartCode  = psLexCode `fmap` get

alexSetStartCode :: Int -> Parser ()
alexSetStartCode code = do
  s <- get
  set $! s { psLexCode = code }

begin code _ _ = alexSetStartCode code >> scan

buildInt :: Int -> String -> Token
buildInt base val = TokInt (base, val)

}

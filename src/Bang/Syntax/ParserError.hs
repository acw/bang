module Bang.Syntax.ParserError(
         ParserError(..)
       )
 where

import Data.Text.Lazy(Text)
import Bang.Monad(BangError(..))
import Bang.Syntax.Location(Location, ppLocation)
import Bang.Syntax.Token(Token, ppToken)
import Bang.Utils.Pretty(BangDoc, text')
import Text.PrettyPrint.Annotated((<+>), ($+$), text, quotes, text, nest)

data ParserError = LexError          Location Text
                 | ParseError        Location Token
                 | RedefinitionError Location Location Text
                 | InternalError     Location Text
                 | UnboundVariable   Location Text
                 | UnexpectedEOF
 deriving (Show)

instance BangError ParserError where
  ppError = prettyError

prettyError :: ParserError -> (Maybe Location, BangDoc)
prettyError e =
  case e of
    LexError l t ->
      (Just l, text "Lexical error around token" <+> quotes (text' t))
    ParseError l t ->
      (Just l, text "Parser error around token" <+> quotes (ppToken t))
    RedefinitionError errLoc origLoc t ->
      let line1 = text "Variable" <+> quotes (text' t) <+> text "is redefined: "
          line2 = text "Original definition:" <+> ppLocation origLoc
          line3 = text "Redefinition:" <+> ppLocation errLoc
      in (Nothing, line1 $+$ nest 3 (line2 $+$ line3))
    InternalError loc t ->
      (Just loc, text' t)
    UnboundVariable loc t ->
      (Just loc, text "Unbound variable" <+> quotes (text' t))
    UnexpectedEOF ->
      (Nothing, text "Unexpected end of file.")



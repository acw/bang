{-# OPTIONS_GHC -w #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTION_GHC -w              #-}
module Bang.Syntax.Parser(
         parseModule
       , ParseError, showError
       , lexWithLayout
       )
 where

import           Bang.Syntax.AST
import           Bang.Syntax.Lexer
import           Bang.Syntax.Location
import           Bang.Syntax.Token
import           Data.Map.Strict(Map)
import           Data.Map.Strict as Map
import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as T
import           MonadLib
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t8 t9 t10
	= HappyTerminal (Located Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Module)
	| HappyAbsSyn5 (Maybe Declaration)
	| HappyAbsSyn6 (Type)
	| HappyAbsSyn7 (Expression)
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

action_0 (17) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (17) = happyShift action_2
action_1 _ = happyFail

action_2 (20) = happyShift action_4
action_2 _ = happyFail

action_3 (52) = happyAccept
action_3 _ = happyFail

action_4 (8) = happyGoto action_5
action_4 _ = happyReduce_7

action_5 (14) = happyShift action_7
action_5 (21) = happyShift action_8
action_5 (5) = happyGoto action_6
action_5 _ = happyReduce_1

action_6 _ = happyReduce_8

action_7 (18) = happyShift action_11
action_7 _ = happyFail

action_8 (11) = happyShift action_9
action_8 (12) = happyShift action_10
action_8 _ = happyFail

action_9 (6) = happyGoto action_16
action_9 _ = happyReduce_5

action_10 (7) = happyGoto action_15
action_10 _ = happyReduce_6

action_11 (19) = happyShift action_14
action_11 (9) = happyGoto action_12
action_11 (10) = happyGoto action_13
action_11 _ = happyReduce_9

action_12 _ = happyReduce_4

action_13 (13) = happyShift action_17
action_13 _ = happyReduce_10

action_14 _ = happyReduce_11

action_15 _ = happyReduce_3

action_16 _ = happyReduce_2

action_17 (19) = happyShift action_18
action_17 _ = happyFail

action_18 _ = happyReduce_12

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyTerminal happy_var_2)
	(HappyTerminal (Located happy_var_1 (ValIdent  _ "module")))
	 =  HappyAbsSyn4
		 (Module (identToName happy_var_1 happy_var_2)
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	_
	_
	 =  HappyAbsSyn5
		 (Just TypeDeclartion
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 _
	_
	_
	 =  HappyAbsSyn5
		 (Just ValueDeclaration
	)

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 _
	_
	_
	 =  HappyAbsSyn5
		 (return Nothing
	)

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 (Type
	)

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn7
		 (Expression
	)

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (case happy_var_2 of
                       Nothing -> happy_var_1
                       Just x  -> happy_var_1 ++ [x]
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  9 happyReduction_9
happyReduction_9  =  HappyAbsSyn9
		 ([]
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (reverse happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_3 : happy_var_1
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= runNextToken(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Located initialPosition EOFTok -> action 52 52 tk (HappyState action) sts stk;
	Located happy_dollar_dollar (OpIdent   _ "::") -> cont 11;
	Located happy_dollar_dollar (OpIdent   _ "=") -> cont 12;
	Located happy_dollar_dollar (OpIdent   _ ",") -> cont 13;
	Located happy_dollar_dollar (ValIdent  _ "infixl") -> cont 14;
	Located happy_dollar_dollar (ValIdent  _ "infixr") -> cont 15;
	Located happy_dollar_dollar (ValIdent  _ "infix") -> cont 16;
	Located happy_dollar_dollar (ValIdent  _ "module") -> cont 17;
	Located _  (IntTok    _ _) -> cont 18;
	Located _  (OpIdent   _) -> cont 19;
	Located _  (TypeIdent _) -> cont 20;
	Located _  (ValIdent  _) -> cont 21;
	Located _  (OpIdent (LeftAssoc  0) _) -> cont 22;
	Located _  (OpIdent (RightAssoc 0) _) -> cont 23;
	Located _  (OpIdent (NonAssoc   0) _) -> cont 24;
	Located _  (OpIdent (LeftAssoc  1) _) -> cont 25;
	Located _  (OpIdent (RightAssoc 1) _) -> cont 26;
	Located _  (OpIdent (NonAssoc   1) _) -> cont 27;
	Located _  (OpIdent (LeftAssoc  2) _) -> cont 28;
	Located _  (OpIdent (RightAssoc 2) _) -> cont 29;
	Located _  (OpIdent (NonAssoc   2) _) -> cont 30;
	Located _  (OpIdent (LeftAssoc  3) _) -> cont 31;
	Located _  (OpIdent (RightAssoc 3) _) -> cont 32;
	Located _  (OpIdent (NonAssoc   3) _) -> cont 33;
	Located _  (OpIdent (LeftAssoc  4) _) -> cont 34;
	Located _  (OpIdent (RightAssoc 4) _) -> cont 35;
	Located _  (OpIdent (NonAssoc   4) _) -> cont 36;
	Located _  (OpIdent (LeftAssoc  5) _) -> cont 37;
	Located _  (OpIdent (RightAssoc 5) _) -> cont 38;
	Located _  (OpIdent (NonAssoc   5) _) -> cont 39;
	Located _  (OpIdent (LeftAssoc  6) _) -> cont 40;
	Located _  (OpIdent (RightAssoc 6) _) -> cont 41;
	Located _  (OpIdent (NonAssoc   6) _) -> cont 42;
	Located _  (OpIdent (LeftAssoc  7) _) -> cont 43;
	Located _  (OpIdent (RightAssoc 7) _) -> cont 44;
	Located _  (OpIdent (NonAssoc   7) _) -> cont 45;
	Located _  (OpIdent (LeftAssoc  8) _) -> cont 46;
	Located _  (OpIdent (RightAssoc 8) _) -> cont 47;
	Located _  (OpIdent (NonAssoc   8) _) -> cont 48;
	Located _  (OpIdent (LeftAssoc  9) _) -> cont 49;
	Located _  (OpIdent (RightAssoc 9) _) -> cont 50;
	Located _  (OpIdent (NonAssoc   9) _) -> cont 51;
	_ -> happyError' tk
	})

happyError_ 52 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (>>=)
happyReturn :: () => a -> Parser a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> Parser a
happyReturn1 = happyReturn
happyError' :: () => (Located Token) -> Parser a
happyError' tk = parseError tk

top_module = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


newtype Parser a = Parser {
          unParser :: StateT ParserState (ExceptionT ParseError Id) a
        }
 deriving (Functor, Applicative, Monad)

data ParseError = LexError      Location Text
                | ParseError    Location Token
                | UnexpectedEOF
 deriving (Show)

showError :: ParseError -> String
showError (LexError      l t) = show l ++ ": lexer error around " ++ T.unpack t
showError (ParseError    l t) = show l ++ ": parse error around " ++ showToken t
showError  UnexpectedEOF      =           "Unexpected end of file"

data ParserState = ParserState {
       psPrecTable   :: Map Text Word
     , psTokenStream :: [Located Token]
     }

instance StateM Parser ParserState where
  get = Parser   get
  set = Parser . set

instance ExceptionM Parser ParseError where
  raise = Parser . raise

instance RunExceptionM Parser ParseError where
  try m = Parser (try (unParser m))

runNextToken :: (Located Token -> Parser a) -> Parser a
runNextToken action =
  do state <- get
     case psTokenStream state of
       []         -> raise UnexpectedEOF
       (x : rest) ->
         do set (state{ psTokenStream = rest })
            action x

lexWithLayout :: Origin -> Position -> Text -> [Located Token]
lexWithLayout src pos txt = lexer src (Just pos) txt

parseModule :: Origin -> Text -> Either ParseError Module
parseModule src txt =
  let parserM   = unParser top_module
      excM      = runStateT initialState (parserM :: StateT ParserState (ExceptionT ParseError Id) Module)
      idM       = runExceptionT (excM :: ExceptionT ParseError Id (Module, ParserState))
      resWState = runId idM
  in fmap fst resWState
 where
  tokenStream  = lexWithLayout src initialPosition txt
  initialState = ParserState Map.empty tokenStream

parseError :: Located Token -> Parser a
parseError t =
  case t of
    Located _ EOFTok       -> raise UnexpectedEOF
    Located p (ErrorTok t) -> raise (LexError p t)
    Located p t            -> raise (ParseError p t)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

























infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.


module ParseLib where

import qualified Tokenizer as T
import qualified Utility as U


-- *** Types *** --

-- Import error
type Error = String

type ParseInput = (U.Span, [T.Token])
data ParseOutput a = Match (a, ParseInput) | NoMatch -- New type for readability
type ParseFun a = ParseInput -> (ParseOutput a, Maybe Error)
data ParseMonad a = PM ( ParseFun a )
unpack (PM a) = a

-- *** Monad and Combinators *** --
instance Monad ParseMonad where
--	Sequential combinator of two parser functions
--	(>>=) :: ParseMonad a -> (a -> ParseMonad b) -> ParseMonad b
	(>>=) (PM parseA) makePMB = PM $ \ input -> case (parseA input) of
		(NoMatch, Nothing)	-> error "PARSE ERROR: No syntax match and no errors!"
		(NoMatch, Just e)	-> (NoMatch, Just e)
		(Match (a, input2), e) 	-> unpack (makePMB a) input2
			
--	return :: a -> ParseMonad a
	return a = PM $ \ input -> (Match (a, input), Nothing)

-- Lazy OR combinator of two parser functions
(\/) :: ParseMonad a -> ParseMonad a -> ParseMonad a
(\/) parseA parseB = PM $ \ input -> case (unpack parseA input) of
	(Match aResult, aError)		-> (Match aResult, aError)
	(NoMatch, _) -> case (unpack parseB input) of
		(Match bResult, bError) -> (Match bResult, bError)
		(NoMatch, bError)	-> (NoMatch, bError) 		-- TODO: Implement error selection.


opt :: ParseMonad a -> ParseMonad (Maybe a)
opt monad = 
		return Nothing
	\/ do
		x <- monad
		return (Just x)

-- Operator for repetition
kleene :: ParseMonad a -> ParseMonad [a]
kleene monad = do
	return [] \/ kleenePlus monad

kleenePlus :: ParseMonad a -> ParseMonad [a]
kleenePlus monad = do
	x <- monad
	do
		xs <- kleenePlus monad
		return (x:xs)
		\/
		return [x]

-- Operator for repitition with delimiters
kleeneDelimited :: ParseMonad a -> ParseMonad b -> ParseMonad [a]
kleeneDelimited mA mB = do
	return [] \/ kleenePlusDelimited mA mB

kleenePlusDelimited :: ParseMonad a -> ParseMonad b -> ParseMonad [a]
kleenePlusDelimited mA mB = do
	x <- mA
	do
			mB
			xs <- kleenePlusDelimited mA mB
			return (x:xs)
		\/ do
			return [x]
		

-- *** Parser Functions *** --

-- TODO: This function is not finished
applyParser :: ParseMonad a -> [T.Token] -> Either Error (a, ParseInput)
applyParser m list = case unpack m (U.Span 0 0, list) of
	(NoMatch, Nothing)	-> error "PARSE ERROR: No syntax match and no errors!"
	(NoMatch, Just e)	-> Left e
	(Match (output,rest),e)	-> Right (output,rest)
	
equalsTokenEnum :: T.TokenEnum -> ParseMonad (T.TokenEnum)
equalsTokenEnum token = parseTokenEnum ((==) token)

-- Applies a predicate from token enums to truth values to e.g. eat simple tokens
parseTokenEnum :: (T.TokenEnum -> Bool) -> ParseMonad (T.TokenEnum)
parseTokenEnum f = PM $ \ input -> case input of
	(_, []) 				-> (NoMatch, Just "PARSE ERROR: End of stream")
	(parseSpan, (tokenEnum, tokenSpan):ts)	-> case (f tokenEnum) of
		True	-> (Match (tokenEnum, (newSpan, ts)), Nothing)
		False 	-> (NoMatch, Just "PARSE ERROR: Unexpected token")		-- TODO: Add token information to error.
	where newSpan = U.Span 0 0	-- TODO: Correctly calculate new span
	
-- Applies a mapping from lexer tokens to a result type, such as a piece of AST
parseToken :: (T.Token -> Maybe a) -> ParseMonad (a)
parseToken f = PM $ \ input -> case input of
	(_, [])			-> (NoMatch, Just "PARSE ERROR: End of stream")
	(parseSpan, token:xs)	-> case (f token) of
		Nothing		-> (NoMatch, Just "PARSE ERROR: Unexpected token")	-- TODO: Add information on token to error.
		Just result	-> (Match (result, (newSpan, xs)), Nothing)
	where newSpan = U.Span 0 0	-- TODO: Correctly calculate new span
	
-- Applies a given operator mapping from lexer to AST
-- This allows for very simple syntax.
parseOp :: (T.OperatorEnum -> Maybe (U.Span -> a) ) -> ParseMonad (a)
parseOp dict = parseToken $ \ token -> case token of
	(T.Op opLex, spn) -> case dict (opLex) of
		Just opAST	-> Just (opAST spn)
		Nothing		-> Nothing
	_		  -> Nothing
		
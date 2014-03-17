-- Import token
-- Import span
-- Import error
type Token = Int
type Span = (Int, Int)
type Error = String

type ParseInput = (Span, [Token])
data ParseOutput a = Match (a, ParseInput) | NoMatch
type ParseFun a = ParseInput -> (ParseOutput a, Maybe Error)

data ParseMonad a = PM ( ParseFun a )
unpack (PM a) = a

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
(\/) parseA parseB = PM $ \ input -> case unpack parseA input of
	(Match aResult, aError)		-> (Match aResult, aError)
	(NoMatch, _) -> case (unpack parseB input) of
		(Match bResult, bError) -> (Match bResult, bError)
		(NoMatch, bError)	-> (NoMatch, bError) 		-- TODO: Implement error selection.

									-- TODO: Possibly implement two more combinators
--kleene :: Parsfun -> Parsfun					-- de "*" in de grammatica
--opt :: Parsfun -> Parsfun						-- representeerd de [ optionele argumenten ] in de grammar

parseOne :: Token -> ParseMonad Token
parseOne token = parseToken ((==) token)

parseToken :: (Token -> Bool) -> ParseMonad Token
parseToken f = PM $ \ (_, token:ts) -> case (f token) of		-- TODO: Empty token list not matched
	True 	-> (Match (token, ((0,0),ts) ), Nothing)		-- TODO: Implement token span
	False 	-> (NoMatch, Just "PARSE ERROR: Unexpected token")
	
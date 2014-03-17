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

-- Sort of like an either monad
instance Monad ParseMonad where
--	(>>=) :: ParseMonad a -> (a -> ParseMonad b) -> ParseMonad b
	(>>=) (PM parseA) makePMB = PM $ \ input -> case (parseA input) of
		(NoMatch, Nothing)	-> error "PARSE ERROR: No syntax match and no errors!"
		(NoMatch, Just e)	-> (NoMatch, Just e)
		(Match (a, input2), e) 	-> unpack (makePMB a) input2
		
	-- (>>=) parseA makeParseB = \ input ->
	-- 	let (a, input2) = parseA input in 
	-- 	(makeParseB a) input2
	
--	return :: a -> ParseMonad a
	return a = PM $ \ input -> (Match (a, input), Nothing)


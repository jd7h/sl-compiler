module Parser where

import qualified Tokenizer as T
import qualified Utility as U
import qualified AST

import ParseLib

import Debug.Trace

type SourceTree a = a U.Span

-- type ParseInput = (U.Span, [T.Token])
-- data ParseOutput a = Match (a, ParseInput) | NoMatch
-- type ParseFun a = ParseInput -> (ParseOutput a, Maybe Error)
-- data ParseMonad a = PM ( ParseFun a )

-- parseType
-- parseComment
-- parseSep
-- parseKey
-- parseLiteral
-- parseOperator

-- TODO: Implement
parseProgram :: ParseMonad (SourceTree AST.Program)
parseProgram = undefined

-- TODO: Implement
parseDeclaration :: ParseMonad (SourceTree AST.Declaration)
parseDeclaration = undefined

-- TODO: Implement
parseVarDecl :: ParseMonad (SourceTree AST.Declaration)
parseVarDecl = undefined

-- TODO: Implement
parseFunDecl :: ParseMonad (SourceTree AST.Declaration)
parseFunDecl = undefined

parseType :: ParseMonad (SourceTree AST.Type)
parseType = undefined

-- TODO: Implement
parseStatement :: ParseMonad (SourceTree AST.Statement)
parseStatement = undefined

-- *** Expressions *** --

-- Function calls and variable accessing
parseExpression :: ParseMonad (SourceTree AST.Expression)
parseExpression = 
	do
		ident <- parseIdentifier
		parseFunOrField ident	
	\/
		parseExp1

parseFunOrField :: SourceTree AST.Identifier -> ParseMonad (SourceTree AST.Expression)
parseFunOrField ident = 
	do
		fields <- (kleene $ parseField)
		return $ AST.Var ident fields (U.Span 0 0)
	\/ do
		equalsTokenEnum (T.Sep T.LPar)
		e <- opt $ parseActArgs
		equalsTokenEnum (T.Sep T.RPar)
		let args = case e of
			Nothing -> []
			Just x -> x
		return $ AST.FunCall ident args (U.Span 0 0)

parseActArgs :: ParseMonad [SourceTree AST.Expression]
parseActArgs = kleeneDelimited parseExpression (equalsTokenEnum (T.Sep T.Comma))

-- Boolean operators
parseExp1 :: ParseMonad (SourceTree AST.Expression)
parseExp1 =
	do
		e2 <- parseExp2
		op <- parseBoolOp
		e1 <- parseExp1
		return $ AST.BinOp e2 op e1 (U.Span 0 0)
	\/ do
		op <- parseNotOp
		e1 <- parseExp1
		return $ AST.UnOp op e1 (U.Span 0 0)
	\/
		parseExp2

-- Relation operators
parseExp2 :: ParseMonad (SourceTree AST.Expression)
parseExp2 =
	do
		e3 <- parseExp3
		op <- parseRelOp
		e2 <- parseExp2
		return $ AST.BinOp e3 op e2 (U.Span 0 0)
	\/
		parseExp3

-- List operators
parseExp3 :: ParseMonad (SourceTree AST.Expression)
parseExp3 =
	do
		e4 <- parseExp4
		op <- parseConsOp
		e3 <- parseExp3
		return $ AST.BinOp e4 op e3 (U.Span 0 0)
	\/
		parseExp4

-- Summation operators
parseExp4 :: ParseMonad (SourceTree AST.Expression)
parseExp4 =
	do
		e5 <- parseExp5
		op <- parseSumOp
		e4 <- parseExp4
		return $ AST.BinOp e5 op e4 (U.Span 0 0)
	\/
	 	parseExp5 
		

-- Multiplication operators
parseExp5 :: ParseMonad (SourceTree AST.Expression)
parseExp5 =
	do
		e6 <- parseExp6 
		op <- parseMultOp
		e5 <- parseExp5
		return $ AST.BinOp e6 op e5 (U.Span 0 0)
	\/
		parseExp6

-- Minus operator
parseExp6 :: ParseMonad (SourceTree AST.Expression)
parseExp6 = 
	do
		op <- parseNegOp
		e6 <- parseExp6
		return $ AST.UnOp op e6 (U.Span 0 0)
	\/
		parseExp7


parseExp7 :: ParseMonad (SourceTree AST.Expression)		
parseExp7 = 
	do	
		b <- parseBoolean
		return $ AST.ConstBool b (U.Span 0 0)
	\/ do
		i <- parseInteger
		return $ AST.ConstInt i (U.Span 0 0)
	\/ do
		equalsTokenEnum (T.Sep T.LBr)
		equalsTokenEnum (T.Sep T.RBr)
		return $ AST.Nil (U.Span 0 0)
	\/ do
		equalsTokenEnum (T.Sep T.LPar)
		e <- parseExpression
		do
				equalsTokenEnum (T.Sep T.Comma)
				e2 <- parseExpression
				equalsTokenEnum (T.Sep T.RPar)
				return $ AST.Pair e e2 (U.Span 0 0) 
			\/ do
				equalsTokenEnum (T.Sep T.RPar)
				return e

-- *** Operator token parsing *** --
parseBoolOp :: ParseMonad (SourceTree AST.Op2)
parseBoolOp = parseOp $ \ operator -> case operator of
		T.And -> Just AST.And
		T.Or  -> Just AST.Or
		_     -> Nothing
		
parseNotOp :: ParseMonad (SourceTree AST.Op1)
parseNotOp = parseOp $ \ operator -> case operator of
		T.Not -> Just AST.Not
		_     -> Nothing

parseRelOp :: ParseMonad (SourceTree AST.Op2)
parseRelOp = parseOp $ \ operator -> case operator of
		T.Eq  -> Just AST.Eq 
		T.Lt  -> Just AST.Lt
		T.Gt  -> Just AST.Gt
		T.Le  -> Just AST.Le
		T.Ge  -> Just AST.Ge
		T.Neq -> Just AST.Neq
		_     -> Nothing

parseConsOp :: ParseMonad (SourceTree AST.Op2)
parseConsOp = parseOp $ \ operator -> case operator of
		T.Cons -> Just AST.Cons
		_      -> Nothing

parseSumOp :: ParseMonad (SourceTree AST.Op2)
parseSumOp = parseOp $ \ operator -> case operator of
		T.Plus  -> Just AST.Plus
		T.Min 	-> Just AST.Min
		_	-> Nothing

parseMultOp :: ParseMonad (SourceTree AST.Op2)
parseMultOp = parseOp $ \ operator -> case operator of
		T.Mult	-> Just AST.Mult
		T.Div	-> Just AST.Div
		T.Mod	-> Just AST.Mod
		_	-> Nothing
		
parseNegOp :: ParseMonad (SourceTree AST.Op1)
parseNegOp = parseOp $ \ operator -> case operator of
		T.Min	-> Just AST.Neg
		_	-> Nothing

parseField :: ParseMonad (SourceTree AST.Field)
parseField = parseToken $ \ token -> case token of
		(T.Field T.Head, spn)	-> 	Just (AST.Head spn)
		(T.Field T.Tail, spn)	-> 	Just (AST.Tail spn)
		(T.Field T.First, spn)	-> 	Just (AST.First spn)
		(T.Field T.Second, spn)	-> 	Just (AST.Second spn)		
		_			-> 	Nothing

-- *** Constants and identifier parsing *** --
parseBoolean :: ParseMonad (SourceTree AST.Boolean)
parseBoolean 	= parseToken $ \ token -> case token of
		(T.Boolean n, spn)	-> Just (AST.Boolean n spn)
		_			-> Nothing
		
parseInteger :: ParseMonad (SourceTree AST.Integer)
parseInteger 	= parseToken $ \ token -> case token of
		(T.Number n, spn)	-> Just (AST.Integer n spn)
		_			-> Nothing

parseIdentifier :: ParseMonad (SourceTree AST.Identifier)
parseIdentifier = parseToken $ \ token -> case token of
		(T.Id name, spn)	-> Just (AST.Identifier name Nothing spn)
		_ 			-> Nothing
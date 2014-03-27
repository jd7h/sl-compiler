module Parser where

import qualified Tokenizer as T
import qualified Utility as U
import qualified AST

import ParseLib

type SourceTree a = a U.Span

parseLanguage :: [T.Token] -> Either Error (SourceTree AST.Program)
parseLanguage tokens = applyParser tokens parseProgram

parseProgram :: ParseMonad (SourceTree AST.Program)
parseProgram =
	do
		decls <- kleenePlus $ parseDeclaration
		return $ AST.Program decls (U.Span 0 0)

parseDeclaration :: ParseMonad (SourceTree AST.Declaration)
parseDeclaration = parseVarDecl \/ parseFunDecl

parseVarDecl :: ParseMonad (SourceTree AST.Declaration)
parseVarDecl = 
	do
		t <- parseType
		ident <- parseIdentifier
		equalsTokenEnum (T.Op T.As)
		e <- parseExpression
		equalsTokenEnum (T.Sep T.Pcomma)
		return $ AST.VarDecl t ident e (U.Span 0 0)

parseFunDecl :: ParseMonad (SourceTree AST.Declaration)
parseFunDecl = 
	do
		t <- parseRetType
		ident <- parseIdentifier
		equalsTokenEnum (T.Sep T.LPar)
		e <- opt $ parseFunArgs
		equalsTokenEnum (T.Sep T.RPar)
		equalsTokenEnum (T.Sep T.LAcc)
		vars <- kleene $ parseVarDecl
		stmnts <- kleenePlus $ parseStatement
		equalsTokenEnum (T.Sep T.RAcc)
		let fargs = case e of
			Nothing -> []
			Just x 	-> x
		return $ AST.FunDecl t ident fargs vars stmnts (U.Span 0 0)
	
parseFunArgs :: ParseMonad [(SourceTree AST.Type, SourceTree AST.Identifier)]
parseFunArgs = kleeneDelimited parseFunArg (equalsTokenEnum (T.Sep T.Comma))

parseFunArg = do
		t <- parseType
		ident <- parseIdentifier
		return $ (t, ident)

parseRetType :: ParseMonad (SourceTree AST.Type)
parseRetType = 	parseVoid \/ parseType

parseStatement :: ParseMonad (SourceTree AST.Statement)
parseStatement =
	do 	-- Block
		equalsTokenEnum (T.Sep T.LAcc)
		stmnts <- kleene $ parseStatement
		equalsTokenEnum (T.Sep T.RAcc)
		return $ AST.Block stmnts (U.Span 0 0)
	\/ do 	-- IfElse
		equalsTokenEnum (T.Key T.If)
		equalsTokenEnum (T.Sep T.LPar)
		e <- parseExpression
		equalsTokenEnum (T.Sep T.RPar)
		s <- parseStatement
		equalsTokenEnum (T.Key T.Else)
		s2 <- parseStatement
		return $ AST.IfElse e s s2 (U.Span 0 0)
	\/ do 	-- If
		equalsTokenEnum (T.Key T.If)
		equalsTokenEnum (T.Sep T.LPar)
		e <- parseExpression
		equalsTokenEnum (T.Sep T.RPar)
		s <- parseStatement
		return $ AST.If e s (U.Span 0 0)
	\/ do 	-- While
		equalsTokenEnum (T.Key T.While)
		equalsTokenEnum (T.Sep T.LPar)		
		e <- parseExpression
		equalsTokenEnum (T.Sep T.RPar)
		s <- parseStatement
		return $ AST.While e s (U.Span 0 0)
	\/ do 	-- Assignment
		ident <- parseIdentifier
		fields <- (kleene $ parseField)
		equalsTokenEnum (T.Op T.As)
		e <- parseExpression
		equalsTokenEnum (T.Sep T.Pcomma)
		return $ AST.Assignment ident fields e (U.Span 0 0)
	\/ do 	-- Return
		equalsTokenEnum (T.Key T.Return)
		e <- opt $ parseExpression
		equalsTokenEnum (T.Sep T.Pcomma)
		return $ AST.Return e (U.Span 0 0)
	\/ do 	-- Expression
		e <- parseExpression
		equalsTokenEnum (T.Sep T.Pcomma)
		return $ AST.Expression e (U.Span 0 0)
		
		
-- *** Expressions *** --

-- Function calls and variable accessing
parseExpression :: ParseMonad (SourceTree AST.Expression)
parseExpression = parseExp1

-- Boolean operators
parseExp1 :: ParseMonad (SourceTree AST.Expression)
parseExp1 =
	do
		op <- parseNotOp
		e1 <- parseExp1
		return $ AST.UnOp op e1 (U.Span 0 0)
	\/ do
		e2 <- parseExp2
		do
				op <- parseBoolOp
				e1 <- parseExp1
				return $ AST.BinOp e2 op e1 (U.Span 0 0)
			\/	
				return e2

-- Relation operators
parseExp2 :: ParseMonad (SourceTree AST.Expression)
parseExp2 =
	do
		e3 <- parseExp3
		do
				op <- parseRelOp
				e2 <- parseExp2
				return $ AST.BinOp e3 op e2 (U.Span 0 0)
			\/
				return e3

-- List operators
parseExp3 :: ParseMonad (SourceTree AST.Expression)
parseExp3 =
	do
		e4 <- parseExp4
		do
				op <- parseConsOp
				e3 <- parseExp3
				return $ AST.BinOp e4 op e3 (U.Span 0 0)
			\/
				return e4

-- Summation operators
parseExp4 :: ParseMonad (SourceTree AST.Expression)
parseExp4 =
	do
		e5 <- parseExp5
		do
				op <- parseSumOp
				e4 <- parseExp4
				return $ AST.BinOp e5 op e4 (U.Span 0 0)
			\/
			 	return e5
		

-- Multiplication operators
parseExp5 :: ParseMonad (SourceTree AST.Expression)
parseExp5 =
	do
		e6 <- parseExp6 
		do
				op <- parseMultOp
				e5 <- parseExp5
				return $ AST.BinOp e6 op e5 (U.Span 0 0)
			\/
				return e6

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
	do	-- Boolean value
		b <- parseBoolean
		return $ AST.ConstBool b (U.Span 0 0)
	\/ do 	-- Integer value
		i <- parseInteger
		return $ AST.ConstInt i (U.Span 0 0)
	\/ do 	-- Nil value
		equalsTokenEnum (T.Sep T.LBr)
		equalsTokenEnum (T.Sep T.RBr)
		return $ AST.Nil (U.Span 0 0)
	\/ do 	-- Parentheses or tuple value
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
	\/ do 	-- Function call or identifier
		ident <- parseIdentifier
		parseFunOrField ident	
		

parseFunOrField :: SourceTree AST.Identifier -> ParseMonad (SourceTree AST.Expression)
parseFunOrField ident = 
	do
		equalsTokenEnum (T.Sep T.LPar)
		e <- opt $ parseActArgs
		equalsTokenEnum (T.Sep T.RPar)
		let args = case e of
			Nothing -> []
			Just x -> x
		return $ AST.FunCall ident args (U.Span 0 0)
	\/ do
		fields <- (kleene $ parseField)
		return $ AST.Var ident fields (U.Span 0 0)

parseActArgs :: ParseMonad [SourceTree AST.Expression]
parseActArgs = kleeneDelimited parseExpression (equalsTokenEnum (T.Sep T.Comma))
		

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
		(T.Field T.Head, spn)	-> Just (AST.Head spn)
		(T.Field T.Tail, spn)	-> Just (AST.Tail spn)
		(T.Field T.First, spn)	-> Just (AST.First spn)
		(T.Field T.Second, spn)	-> Just (AST.Second spn)		
		_			-> Nothing


-- *** Type Parsing *** --
parseVoid :: ParseMonad (SourceTree AST.Type)
parseVoid = parseToken $ \ token -> case token of
		(T.Type T.Void, spn)	-> Just (AST.Void spn)	
		_			-> Nothing

parseBasicType :: ParseMonad (SourceTree AST.Type)
parseBasicType = parseToken $ \ token -> case token of
		(T.Type T.Int, spn)	-> Just (AST.Int spn)
		(T.Type T.Bool, spn)	-> Just (AST.Bool spn)
		_			-> Nothing
		
parseType :: ParseMonad (SourceTree AST.Type)
parseType = 	
		parseBasicType
	\/ do 	-- Tuple
		equalsTokenEnum (T.Sep T.LPar)
		t1 <- parseType
		equalsTokenEnum (T.Sep T.Comma)
		t2 <- parseType
		equalsTokenEnum (T.Sep T.RPar)
		return $ AST.Tuple t1 t2 (U.Span 0 0)
	\/ do 	-- List
		equalsTokenEnum (T.Sep T.LBr)
		t <- parseType
		equalsTokenEnum (T.Sep T.RBr)	
		return $ AST.List t (U.Span 0 0)
	\/ do 	-- Type Identifier
		ident <- parseIdentifier
		return $ AST.TypeId ident (U.Span 0 0)


-- *** Constants and identifier parsing *** --
parseBoolean :: ParseMonad (SourceTree AST.Boolean)
parseBoolean = parseToken $ \ token -> case token of
		(T.Boolean n, spn)	-> Just (AST.Boolean n spn)
		_			-> Nothing
		
parseInteger :: ParseMonad (SourceTree AST.Integer)
parseInteger = parseToken $ \ token -> case token of
		(T.Number n, spn)	-> Just (AST.Integer n spn)
		_			-> Nothing

parseIdentifier :: ParseMonad (SourceTree AST.Identifier)
parseIdentifier = parseToken $ \ token -> case token of
		(T.Id name, spn)	-> Just (AST.Identifier name Nothing spn)
		_ 			-> Nothing
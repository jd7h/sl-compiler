module Scoping where

import qualified AST

import Control.Monad.State

type Context a b = ([(a, b)], b)
type ScopingContext = Context String Int

-- Registers a new identifier in the scoping context
register :: AST.Identifier a -> State ScopingContext (AST.Identifier a)
register (AST.Identifier name Nothing a) = do
	(idents, freshNum) <- get
	put ((name, freshNum):idents, freshNum+1)
	return $ AST.Identifier name (Just freshNum) a
register (AST.Identifier _ (Just _) _) = error "Identifier already declared"

-- Matches a previously defined identifier in the scoping context
match :: AST.Identifier a -> State ScopingContext (AST.Identifier a)
match (AST.Identifier name Nothing a) = do
	(idents, _) <- get
	case (lookup name idents) of
		Just num	-> return $ AST.Identifier name (Just num) a
		Nothing		-> error $ "Undeclared identifier: " ++ name
match (AST.Identifier _ (Just _) _) = error "Identifier already matched"

class Scoper a where
	traverse :: a -> State ScopingContext a

-- Executes a scoping analysis and assigns identifiers unique numbers
-- Should consider three types of scopes: global, argument and local
-- Program --
instance Scoper (AST.Program a) where
	traverse (AST.Program decls a) = do
		newDecls <- mapM traverse decls
		return $ AST.Program newDecls a

-- Declarations --
instance Scoper (AST.Declaration a) where
	traverse (AST.VarDecl t ident expr a) = do
		newIdent <- register ident	-- Variable name (global)
		newExpr <- traverse expr
		return $ AST.VarDecl t newIdent newExpr a
		
	traverse (AST.FunDecl t ident args decls stms a) = do
		newIdent <- register ident	-- Function name (global)
		(scope, _) <- get		-- Keep global state
		newArgs <- mapM register idents	-- Traverse local scope
		newDecls <- mapM traverse decls	
		newStms <- mapM traverse stms
		(_, freshNum) <- get		-- Restore global state
		put (scope, freshNum)
		return $ AST.FunDecl t newIdent (zip types newArgs) newDecls newStms a
		where (types, idents) = unzip args

-- Statements --
instance Scoper (AST.Statement a) where
	traverse (AST.Block stms a) = do
		(scope, _) <- get		-- Keep current scope
		newStms <- mapM traverse stms
		(_, freshNum) <- get		-- Restore scope
		put (scope, freshNum)
		return $ AST.Block newStms a

	traverse (AST.IfElse expr stm1 stm2 a) = do
		-- No new scoping is necessary, due to block statement
		newExpr <- traverse expr
		newStm1 <- traverse stm1
		newStm2 <- traverse stm2
		return $ AST.IfElse newExpr stm1 stm2 a

	traverse (AST.If expr stm a) = do
		-- No new scoping is necessary, due to block statement
		newExpr <- traverse expr
		newStm <- traverse stm
		return $ AST.If newExpr stm a
	
	traverse (AST.While expr stm a) = do
		-- No new scoping is necessary, due to block statement
		newExpr <- traverse expr
		newStm <- traverse stm
		return $ AST.While newExpr newStm a

	traverse (AST.Assignment ident fields expr a) = do
		-- identifier should have been defined previously
		newIdent <- match ident
		newExpr <- traverse expr
		return $ AST.Assignment newIdent fields newExpr a

	traverse (AST.Return Nothing a) = 
		return $ AST.Return Nothing a
	
	traverse (AST.Return (Just expr) a) = do
		newExpr <- traverse expr
		return $ AST.Return (Just newExpr) a

	traverse (AST.Expression expr a) = do
		newExpr <- traverse expr
		return $ (AST.Expression newExpr a)

-- -- Expressions --
instance Scoper (AST.Expression a) where
	traverse (AST.BinOp expr1 op expr2 a) = do
		newExpr1 <- traverse expr1
		newExpr2 <- traverse expr2
		return $ AST.BinOp newExpr1 op newExpr2 a

	traverse (AST.UnOp op expr a) = do
		newExpr <- traverse expr
		return $ AST.UnOp op newExpr a
	
	traverse (AST.FunCall ident exprs a) = do
		-- identifier should have been defined previously
		newIdent <- match ident
		newExprs <- mapM traverse exprs
		return $ AST.FunCall newIdent newExprs a
	
	traverse (AST.Pair expr1 expr2 a) = do
		newExpr1 <- traverse expr1
		newExpr2 <- traverse expr2
		return $ AST.Pair newExpr1 newExpr2 a
	
	traverse (AST.Var ident fields a) = do
		-- identifier should have been defined previously
		newIdent <- match ident
		return $ AST.Var newIdent fields a

-- For leaf nodes without identifier structure
-- -- traverse constant = return $ constant
-- -- traverse (AST.ConstInt (AST.Integer int) a) = 
-- -- traverse (AST.ConstBool (AST.Integer int) a) = 
-- -- traverse (AST.Nil a) = 
	
-- -- TODO: Implement multi passes: first register all global scope identifiers, then recursively go into all local scopes
-- -- TODO: Variable types not handled
-- -- TODO: Fields not handled
-- -- TODO: Tree spans are not handled
-- -- TODO: Error messages are too simple
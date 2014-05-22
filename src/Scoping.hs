import qualified AST

type Context a b = ([(a, b)], b)
type ScopingContext = Context Int String

-- Updates the scope context and provides a new identifier
register :: AST.Identifier a -> State ScopingContext (AST.Identifier a)
register AST.Identifier name Nothing a = do
	(idents, freshNum) <- get
	put ((name, freshNum):idents, freshNum+1)
	return $ Identifier name (Just freshNum) a
register AST.Identifier _ (Just _) _ = error "Identifier already named"	

-- Should consider three types of scopes: global, argument and local
traverse :: (AST.Program a) -> State ScopingContext (AST a)
traverse (AST.Program decls a) = do
	newDecls <- mapM traverse decls
	return $ AST.Program newDecls a

traverse (AST.VarDecl t ident expr a) = do
	newIdent <- register ident	-- Variable name (global)
	newExpr <- traverse expr	-- TODO: Store old state?
	return $ AST.VarDecl t newIdent newExpr a

traverse (AST.FunDecl t ident args decls stms a) = do
	newIdent <- register ident	-- Function name (global)
	(scope, _) <- get		-- Keep global state
	newArgs <- mapM register idents	-- Traverse local scope
	newDecls <- mapM traverse decls	
	newStms <- mapM traverse stms
	(_, freshNum) <- get		-- Restore global state
	set (scope, freshNum)
	return $ AST.FunDecl t newIdent (zip types newArgs) newDecls newStms a
	where (types, idents) = unzip args

----------------------------------------------
-- register :: String -> State ScopingContext Int
-- register name = do
-- 	(idents, freshNum) <- get
-- 	put ((name, freshNum):idents, freshNum+1)
-- 	return freshNum
-- 
-- traverse :: (AST a) -> State ScopingContext (AST a)

-- 	
-- traverse (AST.VarDecl t ident expr _) = do
-- 	newExpr <- traverse expr
-- 	case ident of
-- 		(AST.Identifier name Nothing _) 	-> 
-- 			freshNum <- register name
-- 			return (AST.VarDecl t (Identifier name (Just freshNum)) newExpr _)
-- 			
-- 		(identifier _ (Just _) _)	->
-- 			error "Unexpected identifier number in tree"
-- 	
-- traverse (AST.FunDecl t ident args _) = do
-- 	newArgs <- mapM traverse args
-- 	
-- 	case ident of
-- 		(Identifier name Nothing _)
module AST where

-- Below largely follows overall structure
-- I've used datatypes rather than standard types to allow for flexibility in adjustments
data Program a		= Program [Declaration a] a
			deriving (Show, Eq, Read)

data Declaration a	= VarDecl (Type a) (Identifier a) (Expression a) a
			| FunDecl (Type a) (Identifier a) [(Type a, Identifier a)] [(Declaration a)] [(Statement a)] a
			deriving (Show, Eq, Read)

data Type a		= Void a
			| Int a
			| Bool a
			| List (Type a)
			| Tuple (Type a) (Type a) a
			| TypeId (Identifier a) a
			deriving (Show, Eq, Read)

data Statement a	= Expression (Expression a) a				-- To support function calls
			| Assignment (Identifier a) [Field a] (Expression a) a
			| If (Expression a) [Statement a] [Statement a] a
			| While (Expression a) [Statement a] a
			| Return (Maybe (Expression a)) a
			deriving (Show, Eq, Read)

data Expression	a 	= Var (Identifier a) [Field a] a
			| BinOp (Expression a) (Op2 a) (Expression a) a		-- Enables tree simplification
			| UnOp (Op1 a) (Expression a) a
			| ConstInt (AST.Integer a) a
			| ConstBool (Boolean a) a
			| FunCall (Identifier a) [Expression a] a
			| Pair (Expression a) (Expression a) a
			| Nil a
			deriving (Show, Eq, Read)
					
data Field a		= Head a | Tail a | First a | Second a
			deriving (Show, Eq, Read)
					
data Op2 a		= Plus a | Min a | Mult a | Div a | Mod a
			| Eq a | Lt a | Gt a | Le a | Ge a | Neq a
			| And a | Or a
			| Cons a
			deriving (Show, Eq, Ord, Read)

data Op1 a		= Neg a | Not a
			deriving (Show, Eq, Ord, Read)

data Identifier	a	= Identifier String (Maybe Int) a
			deriving (Show, Eq, Read)

data Integer a 		= Integer Int a
			deriving (Show, Eq, Read)
			
data Boolean a 		= Boolean Bool a
			deriving (Show, Eq, Read)
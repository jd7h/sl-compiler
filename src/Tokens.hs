module Tokens where

type Tokenlist = Maybe [Token]

data Token = 
	  Number Int 
	| Id String
	| Boolean Bool
	| Op Operator
	| Key Keyword
	| T Type
	| Sep Separator
	deriving (Show, Eq, Read)

data Operator = 
	  Plus
	| Minus
	| Times
	| Div
	| Not
	| GrEq
	| SmEq
	| Gr
	| Sm
	| Eq
	| As
	| And
	| Or
	| Concat
	deriving (Show,Eq, Read)

data Keyword =
	  If
	| Then
	| Else
	| While
	| Return
	deriving (Show, Eq, Read)

data Type = 
	  INT
	| VOID
	| BOOL	
	deriving (Show, Eq, Read)

data Separator =
	  LBr
	| RBr
	| LAcc
	| RAcc
	| LPar
	| RPar
	| Comma
	| Pcomma
	deriving (Show, Eq)
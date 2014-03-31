module Printer_draft where

import AST
import Utility
-- Pretty Printer --

-- Takes a program tree and outputs it with indentation and syntax highlighting
	
data Language =
	  Type
	| Variable
	| Field
	| Constant
	| Keyword
	| Function

data OpenClose a = Open a | Close a
type Markup a = Either Char (OpenClose a)
type MarkupString a = [Markup a]

data OutputMeta = OutputMeta  {
	 indentation :: Int
	,parentheses :: Bool
	}
	
withParentheses :: OutputMeta -> OutputMeta
withParentheses om = om { parentheses = True }

withoutParentheses :: OutputMeta -> OutputMeta
withoutParentheses om = om { parentheses = False }

markup :: (a, String) -> MarkupString a
markup (cons,str) = open cons ++ fromString str ++ close cons

fromString :: String -> MarkupString a
fromString = fmap Left

open :: a -> MarkupString a
open a = return $ Right $ Open a

close :: a -> MarkupString a
close a = return $ Right $ Close a

delimitedMap :: (a -> [b]) -> [b] -> [a] -> [b]
delimitedMap _ _ [] = []
delimitedMap f _ [x] = f x
delimitedMap f s (x:xs) = f x ++ s ++ delimitedMap f s xs

enclose :: OutputMeta -> MarkupString a -> MarkupString a
enclose om str
	| parentheses om	= fromString "(" ++ str ++ fromString ")"
	| otherwise		= str

-- *** Output class definition and instantiation *** --
class Output b where
	output :: OutputMeta -> b a -> MarkupString Language

instance Output Expression where
											         
	output om (Var ident fields _)		= markup (Variable, getIdentifierName ident) ++ (foldr (++) [] (map (output om) fields))
	output om (BinOp e1 op e2 _)		= enclose om $ output (withParentheses om) e1 ++ output om op ++ output (withParentheses om) e2
	output om (UnOp op e _)			= enclose om $ output om op ++ output (withParentheses om) e
	output om (ConstInt (Integer n _) _)	= markup (Constant, show n)
	output om (ConstBool (Boolean b _) _)	= markup (Constant, show b)
	output om (FunCall ident exprs _)	= markup (Function, getIdentifierName ident) ++ fromString "(" ++ delimitedMap (output $ withoutParentheses om) (fromString ", ") exprs ++ fromString ")"
	output om (Pair e1 e2 _)		= fromString "(" ++ output (withoutParentheses om) e1 ++ fromString ", " ++ output (withoutParentheses om) e2 ++ fromString ")" 
	output om (Nil _)			= fromString "[]"

instance Output Field where
	output om (Head _)	= markup (Field, ".hd")
	output om (Tail _)	= markup (Field, ".tl")
	output om (First _)	= markup (Field, ".fst")
	output om (Second _)	= markup (Field, ".snd")
	
instance Output Op2 where
	output om (Plus _)	= fromString " + "
	output om (Min _)	= fromString " - "
	output om (Mult _)	= fromString " * "
	output om (Div _)	= fromString " / "
	output om (Mod _)	= fromString " % "
	output om (Eq _)	= fromString " == "
	output om (Lt _)	= fromString " < "
	output om (Gt _)	= fromString " > "
	output om (Le _)	= fromString " <= "
	output om (Ge _)	= fromString " >= "
	output om (Neq _)	= fromString " != "
	output om (And _)	= fromString " && "
	output om (Or _)	= fromString " || "
	output om (Cons _)	= fromString " : "

instance Output Op1 where
	output om (Neg _)	= fromString "-"
	output om (Not _)	= fromString "!"
		
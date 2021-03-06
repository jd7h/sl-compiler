module Output where

import AST

-- *** Required types and helper functions *** -- 
data Language =
	  Type
	| Variable
	| Field
	| Constant
	| Keyword
	| Function

data StyleSpan a = Open a | Close a
type Markup a = Either Char (StyleSpan a)
type MarkupString a = [Markup a]

data OutputMeta = OutputMeta  {
	  indentation :: Int
	, parentheses :: Bool
}

defaultMeta :: OutputMeta
defaultMeta = OutputMeta {
	  indentation = 0
	, parentheses = False
}

indent :: OutputMeta -> OutputMeta
indent om = om {indentation = 1 + indentation om}	

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

newline :: MarkupString a
newline = fromString "\r\n"

tabs :: OutputMeta -> MarkupString a
tabs om = fromString $ take (indentation om) (repeat '\t')

body :: OutputMeta -> Statement a -> MarkupString Language
body om statement = case statement of
	(Block xs _)	-> output om statement
	otherwise	-> output (indent om) statement
	
-- *** Class Definition *** --
class Output b where
	output :: OutputMeta -> b a -> MarkupString Language
	
outputList :: Output b => OutputMeta -> [b a] -> MarkupString Language
outputList _ []		= fromString ""
outputList om (x:xs)	= output om x ++ outputList om xs

outputArg :: Output b => OutputMeta -> (b a, Identifier c) -> [Markup Language]
outputArg om (t, ident) = output om t ++ fromString " " ++ markup (Variable, getIdentifierName ident)

-- *** Class Instantiation *** --

instance Output Program where
	output om (Program decls _) = outputList om decls

instance Output Declaration where
	output om (VarDecl t ident e _)				= tabs om ++ output om t ++ fromString " " ++ markup (Variable, getIdentifierName ident) ++ fromString " = " ++ output (withoutParentheses om) e ++ fromString ";" ++ newline
	output om (FunDecl t ident args varDecls stmts _)	= tabs om ++ output om t ++ fromString " " ++ markup (Function, getIdentifierName ident) ++ enclose (withParentheses om) (delimitedMap (outputArg om) (fromString ", ") args) ++ newline ++ fromString "{" ++ newline ++ outputList (indent om) varDecls ++ outputList (indent om) stmts ++ tabs om ++ fromString "}" ++ newline ++ newline
	
instance Output Type where
	output om (Void _)		= markup (Type, "Void")
	output om (Int _)		= markup (Type, "Int")
	output om (Bool _)		= markup (Type, "Bool")
	output om (TypeId ident _)	= markup (Type, getIdentifierName ident)
	output om (List t _)		= fromString "[" ++ output om t ++ fromString "]"
	output om (Tuple t1 t2 _)	= fromString "("++ output om t1 ++ fromString ", " ++ output om t2 ++ fromString")"

instance Output Statement where
	output om (Expression e _)		= tabs om ++ output (withoutParentheses om) e ++ fromString ";" ++ newline
	output om (Block [] _)			= tabs om ++ fromString "{}" ++ newline
	output om (Block stmts _)		= tabs om ++ fromString "{\n" ++ outputList (indent om) stmts ++ tabs om ++ fromString "}" ++ newline
	output om (Assignment ident fields e _) = tabs om ++ markup (Variable, getIdentifierName ident) ++ outputList om fields ++ fromString " = " ++ output (withoutParentheses om) e ++ fromString ";" ++ newline
	output om (IfElse e stmt1 stmt2 _) 	= tabs om ++ markup (Keyword, "if") ++ enclose (withParentheses om) (output om e) ++ newline ++ body om stmt1 ++ tabs om ++ markup (Keyword, "else") ++ newline ++ body om stmt2
	output om (If e stmt _)			= tabs om ++ markup (Keyword, "if") ++ enclose (withParentheses om) (output om e) ++ newline ++ body om stmt
	output om (While e stmt _)		= tabs om ++ markup (Keyword, "while") ++ enclose (withParentheses om) (output om e) ++ newline ++ body om stmt
	output om (Return (Just e) _)		= tabs om ++ markup (Keyword, "return") ++ fromString " " ++ output om e ++ fromString ";" ++ newline
	output om (Return Nothing _)		= tabs om ++ markup (Keyword, "return") ++ fromString ";" ++ newline

instance Output Expression where											         
	output om (Var ident fields _)		= markup (Variable, getIdentifierName ident) ++ outputList om fields
	output om (BinOp e1 op e2 _)		= enclose om $ output (withParentheses om) e1 ++ output om op ++ output (withParentheses om) e2
	output om (UnOp op e _)			= enclose om $ output om op ++ output (withParentheses om) e
	output om (ConstInt (Integer n _) _)	= markup (Constant, show n)
	output om (ConstBool (Boolean b _) _)	= markup (Constant, show b)
	output om (FunCall ident exprs _)	= markup (Function, getIdentifierName ident) ++ enclose (withParentheses om) (delimitedMap (output $ withoutParentheses om) (fromString ", ") exprs)
	output om (Pair e1 e2 _)		= enclose (withParentheses om) (output (withoutParentheses om) e1 ++ fromString ", " ++ output (withoutParentheses om) e2 )
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
		
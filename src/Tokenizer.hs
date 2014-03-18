module Tokenizer where

import qualified Utility as U

import Data.List as List
import Data.Maybe as Maybe
import qualified Data.Char as C

type Reader = (String,Int)
type Token = (TokenEnum, U.Span)
type LexResult = Maybe [Token]
type LexFun = Reader -> (Reader, LexResult)

data TokenEnum =
	-- Literals
	  Number Int 
	| Id String
	| Boolean Bool
	-- Other tokens	
	| Type TypeEnum
	| Op OperatorEnum
	| Key KeywordEnum
	| Sep SeparatorEnum
	deriving (Show, Eq, Read)

data TypeEnum = 
	  Int
	| Void
	| Bool	
	deriving (Show, Eq, Read)

data OperatorEnum = 
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

data KeywordEnum =
	  If
	| Then
	| Else
	| While
	| Return
	deriving (Show, Eq, Read)

data SeparatorEnum =
	  LBr
	| RBr
	| LAcc
	| RAcc
	| LPar
	| RPar
	| Comma
	| Pcomma
	deriving (Show, Eq, Read)

reservedSymbols :: [(String,TokenEnum)]
reservedSymbols =
	[	--operators
		("*",	Op Times),
		("/",	Op Div),
		("+",	Op Plus),
		("-",	Op Minus),
		("!",	Op Not),
		(">=",	Op GrEq),
		("<=",	Op SmEq),
		(">",	Op Gr),
		("<",	Op Sm),
		("==",	Op Eq),
		("=",	Op As),
		("&&",	Op And),
		("||",	Op Or),
		(":",	Op Concat),
		--separators
		("[",	Sep LBr),
		("]",	Sep RBr),
		("{",	Sep LAcc),
		("}",	Sep RAcc),
		("(",	Sep LPar),
		(")",	Sep RPar),
		(",",	Sep Comma),
		(";",	Sep Pcomma)	
	]

reservedWords :: [(String,TokenEnum)]
reservedWords = 
	[	("if", 		Key If),
		("then", 	Key Then),
		("else", 	Key Else),
		("while",	Key While),
		("Int",		Type Int),
		("Void",	Type Void),
		("Bool",	Type Bool),
		("False",	Boolean False),
		("True",	Boolean True),
		("return",	Key Return)
	]

-- Lexes the whole program string
lexStr :: Reader -> LexResult
lexStr ([], _) = Nothing
lexStr (input, start) = if isJust result then Just (fromJust result ++ (fromMaybe [] (lexStr (rinput,rindex)))) else Nothing
	where ((rinput, rindex), result) = lexOneToken (input, start)

-- Lexes one token
-- TODO: At the moment chooses FIRST match, must go to LONGEST match
lexOneToken :: LexFun
lexOneToken = \(input,start) -> (lexWhitespace `andthen` lexComment `andthen` lexInteger `andthen` lexSymbol `andthen` lexKeyword) (input,start)

-- Combinator for lexer functions
andthen :: LexFun -> LexFun -> LexFun
andthen f g = \x -> 
	case (f x) of 
		(_, Nothing) 	-> g x
		output		-> output

lexComment :: LexFun
lexComment (input,start)
	| isPrefixOf "//" input	= lexLineComment (drop 2 input)
	| isPrefixOf "/*" input	= lexMultiComment (drop 2 input)
	| otherwise		= ((input, start), Nothing)
	where
		lexLineComment input = 	let	(comment, (x:rest)) = break (\x -> x == '\n' || x == '\r') input
					in	((rest, start+(length comment)), Just [])
		lexMultiComment input = if "*/" `isInfixOf` input 
					then 
						let (comment,rest) = splitAtEndComment [] input
						in ((rest, start+length comment), Just [])
					else 	(("", start+length input), Nothing)

splitAtEndComment :: String -> String -> (String,String)
splitAtEndComment acc []		= (acc,[])
splitAtEndComment acc ('*':'/':xs)	= (acc,xs)
splitAtEndComment acc (x:xs)		= splitAtEndComment (acc ++ [x]) xs

-- Lexes (and discards) whitespace
lexWhitespace :: LexFun
lexWhitespace (input,index) =
	case input of 
		('\r':xs)	-> ((xs,index+1), empty)
		('\n':xs)	-> ((xs,index+1), empty)
		('\t':xs)	-> ((xs,index+1), empty)
		(' ':xs)	-> ((xs,index+1), empty)
		_		-> ((input,index), Nothing)
	where empty = Just []

lexInteger :: LexFun
lexInteger (input, start)
	| C.isDigit (head input)	= let
					(integer, rest) = splitAtInt [] input
					size = length integer
					end = start + size
					in ((rest, end), Just [ (Number (read integer :: Int), U.Span start end) ])
	| otherwise			= ((input, start), Nothing)
	where
	splitAtInt integer [] = (integer, "")
	splitAtInt integer (x:xs) = if C.isDigit x then splitAtInt (integer++[x]) xs else (integer, (x:xs))

-- Lexes special symbols such as operators
lexSymbol :: LexFun
lexSymbol (input, start)
	| C.isAlphaNum (head input)	= ((input, start), Nothing)
	| otherwise			= let 
					(symbol, _) = span isWeirdSymbol input
					match = longestMatch symbol
					size = if isJust match then length (fst (fromJust match)) else 0
					rest = drop (size) input
					end = start + size
					token = if isJust match then Just [(snd (fromJust match), U.Span start end)] else Nothing
					in ((rest, end), token)
	where
		longestMatch []			= Nothing
		longestMatch symbols@(x:xs) 	= if isJust (lookup symbols reservedSymbols) then Just (symbols, fromJust (lookup symbols reservedSymbols)) else longestMatch xs
		isWeirdSymbol s = C.isMark s || C.isSymbol s || C.isPunctuation s

-- Lexes keywords and variable names
lexKeyword :: LexFun
lexKeyword ([], start) 	= (([], start), Nothing)
lexKeyword (input, start)
	| C.isAlpha (head input) =	let 
					(identifier, rest) = span C.isAlphaNum input
					end = start + (length identifier)
					in 
					((rest, end), Just [(assignToken identifier, U.Span start end )])
	| otherwise		 =	((input, start), Nothing)
	where assignToken str 	 = 	if isJust (lookup str reservedWords) 
					then fromJust (lookup str reservedWords) else Id str

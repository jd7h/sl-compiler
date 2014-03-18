module Utility where

type Index = Int
data Span = Span Index Index
	deriving(Show,Eq,Read)

-- Finds the end of a string for a certain predicate
findEnd :: (Char -> Bool) -> String -> Maybe Index
findEnd _ [] 		= Nothing
findEnd f (c:cs)
	| not (f c) 	= Nothing
	| otherwise	= case (findEnd f cs) of
		Nothing -> Just 0
		Just n 	-> Just (n+1)

-- Given string, index and length, cuts out specified substring.	
subStr :: String -> Index -> Int -> String
subStr [] _ _ 		= []
subStr _ _ 0		= []
subStr (x:xs) 0 n	= (x : subStr xs 0 (n-1))
subStr (x:xs) i n	= subStr xs (i-1) n
-- {# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #}

module TokenizerSpec (spec, main) where

import Data.Maybe

import Test.Hspec
import Test.QuickCheck

import Tokenizer
import Utility

main :: IO ()
main = hspec spec

fromRight (Right x) = x

doLexStr source = fromJust (lexStr (source, 0))
	
spec :: Spec
spec = do	
	describe "SL tokenizer" $ do
		
		-- *** Whole programs *** --
		it "lexes the factorial program" $
			doLexStr "Void facR ( Int n ) { if ( n - 2 ) return 1; else return n * facR ( n - 1 ) ; }" `shouldBe`
				[
				 (Type Void,Span 0 4),
				 (Id "facR",Span 5 9),
				 (Sep LPar,Span 10 11),
				 (Type Int,Span 12 15),
				 (Id "n",Span 16 17),
				 (Sep RPar,Span 18 19),
				 (Sep LAcc,Span 20 21),
				 (Key If,Span 22 24),
				 (Sep LPar,Span 25 26),
				 (Id "n",Span 27 28),
				 (Op Min,Span 29 30),
				 (Number 2,Span 31 32),
				 (Sep RPar,Span 33 34),
				 (Key Return,Span 35 41),
				 (Number 1,Span 42 43),
				 (Sep Pcomma,Span 43 44),
				 (Key Else,Span 45 49),
				 (Key Return,Span 50 56),
				 (Id "n",Span 57 58),
				 (Op Mult,Span 59 60),
				 (Id "facR",Span 61 65),
				 (Sep LPar,Span 66 67),
				 (Id "n",Span 68 69),
				 (Op Min,Span 70 71),
				 (Number 1,Span 72 73),
				 (Sep RPar,Span 74 75),
				 (Sep Pcomma,Span 76 77),
				 (Sep RAcc,Span 78 79)
				]
		
		-- *** EXPRESSIONS *** --
		it "lexes parantheses and fields" $
			doLexStr "(UPPERCASE,lowercase).fst.snd.hd.tl" `shouldBe`
				[
				 (Sep LPar,Span 0 1),
				 (Id "UPPERCASE",Span 1 10),
				 (Sep Comma,Span 10 11),
				 (Id "lowercase",Span 11 20),
				 (Sep RPar,Span 20 21),
				 (Field First,Span 21 25),
				 (Field Second,Span 25 29),
				 (Field Head,Span 29 32),
				 (Field Tail,Span 32 35)
				]
		it "lexes relation and boolean operators" $
			doLexStr "x+y+4 >= 12 && !(3+z!=13)" `shouldBe`
				[
				 (Id "x",Span 0 1),
				 (Op Plus,Span 1 2),
				 (Id "y",Span 2 3),
				 (Op Plus,Span 3 4),
				 (Number 4,Span 4 5),
				 (Op Ge,Span 6 8),
				 (Number 12,Span 9 11),
				 (Op And,Span 12 14),
				 (Op Not,Span 15 16),
				 (Sep LPar,Span 16 17),
				 (Number 3,Span 17 18),
				 (Op Plus,Span 18 19),
				 (Id "z",Span 19 20),
				 (Op Neq,Span 20 22),
				 (Number 13,Span 22 24),
				 (Sep RPar,Span 24 25)
				]
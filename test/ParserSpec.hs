-- {# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, RankNTypes #}

module ParserSpec (spec, main) where

import Data.Maybe
import Data.Either

import Test.Hspec
import Test.QuickCheck

import qualified Tokenizer as T
import Utility
import AST
import ParseLib
import Parser

main :: IO ()
main = hspec spec

fromRight (Right x) = x

doParseExpression source = fst $ fromRight $ applyParser parseExpression (fromJust (T.lexStr (source, 0)))
	
spec :: Spec
spec = do	
	-- Sample test
	describe "null" $ do
		it "recognises the empty list" $
			null [] `shouldBe` True

	-- *** EXPRESSIONS *** --
	describe "expression parser" $ do
		it "parses sum (+) expressions" $
			doParseExpression "4+3" `shouldBe` 
				BinOp 
				(ConstInt(Integer 4 (Span 0 1)) (Span 0 0))
				(Plus (Span 1 2)) 
				(ConstInt(Integer 3 (Span 2 3)) (Span 0 0)) 
				(Span 0 0)
		
		it "parses negated sum expressions" $
			doParseExpression "-4+3" `shouldBe` 
				BinOp 
				(UnOp 
					(Neg (Span 0 1))
					(ConstInt (Integer 4 (Span 1 2)) (Span 0 0))
					(Span 0 0)
				)
				(Plus (Span 2 3)) 
				(ConstInt (Integer 3 (Span 3 4)) (Span 0 0)) (Span 0 0)
				
		it "parses reverse negated sum expression" $
			doParseExpression "4+-3" `shouldBe` 
				BinOp
				(ConstInt (Integer 4 (Span 0 1)) (Span 0 0)) 
				(Min (Span 1 2))
				(UnOp 
					(Neg (Span 2 3)) 
					(ConstInt (Integer 3 (Span 3 4)) 
					(Span 0 0)
				) 
				(Span 0 0))
				(Span 0 0)
			

		it "parses sum (-) expressions" $
			doParseExpression "4-3" `shouldBe`
				BinOp 
				(ConstInt (Integer 4 (Span 0 1)) (Span 0 0)) 
				(Min (Span 1 2)) 
				(ConstInt (Integer 3 (Span 2 3)) (Span 0 0))
				(Span 0 0)
		
		it "parses combined sum expressions" $
			doParseExpression "4-3+2" `shouldBe`
				BinOp 
				(ConstInt (Integer 4 (Span 0 1)) (Span 0 0)) 
				(Min (Span 1 2)) 
				(BinOp 
					(ConstInt (Integer 3 (Span 2 3)) (Span 0 0)) 
					(Plus (Span 3 4))
					(ConstInt (Integer 2 (Span 4 5)) (Span 0 0))
					(Span 0 0)
				) 
				(Span 0 0)
				
		it "parses combined sum expressions" $
			doParseExpression "4+3-2" `shouldBe`
				BinOp
				(ConstInt (Integer 4 (Span 0 1)) (Span 0 0))
				(Plus (Span 1 2))
				(BinOp 
					(ConstInt (Integer 3 (Span 2 3)) (Span 0 0)) 
					(Min (Span 3 4))
					(ConstInt (Integer 2 (Span 4 5)) (Span 0 0)) 
					(Span 0 0)
				) 
				(Span 0 0)
				
		it "parses complicated numerical expressions" $
			 doParseExpression "((4+3) / 2) - -1" `shouldBe`
				 BinOp 
				 (BinOp 
				 	(BinOp 
						(ConstInt (Integer 4 (Span 2 3)) (Span 0 0))
						(Plus (Span 3 4))
						(ConstInt (Integer 3 (Span 4 5)) (Span 0 0))
						(Span 0 0)
					) 
					(Div (Span 7 8))
					(ConstInt (Integer 2 (Span 9 10)) (Span 0 0))
					(Span 0 0)
				 )
				 (Min (Span 12 13)) 
				 (UnOp (Neg (Span 14 15)) (ConstInt (Integer 1 (Span 15 16)) (Span 0 0)) (Span 0 0)) 
				 (Span 0 0)
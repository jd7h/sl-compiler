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
fromRight (Left e) = error e


doParseExpression source = fromRight $ applyParser (fromJust (T.lexStr (source, 0))) parseExpression 
doParseStatement source = fromRight $ applyParser (fromJust (T.lexStr (source, 0))) parseStatement
doParseProgram source = fromRight $ applyParser (fromJust (T.lexStr (source, 0))) parseProgram

spec :: Spec
spec = do	
	describe "Program parsing" $do
		it "parses the recursive factorial program" $
			doParseProgram "Void facR ( Int n ) { if ( n - 2 ) return 1; else return n * facR ( n - 1 ) ; }" `shouldBe`
				Program 
				[
				 FunDecl 
				 (Void (Span 0 4)) 
				 (Identifier "facR" Nothing (Span 5 9)) 
				 [ (Int (Span 12 15), Identifier "n" Nothing (Span 16 17))] 
				 []
				 [
				  IfElse 
				  (
				  	BinOp 
					(Var (Identifier "n" Nothing (Span 27 28)) [] (Span 0 0)) 
					(Min (Span 29 30)) 
					(ConstInt (Integer 2 (Span 31 32)) (Span 0 0))
					(Span 0 0)
				  ) 
				  (
				  	Return 
					(Just (ConstInt (Integer 1 (Span 42 43)) (Span 0 0))) 
					(Span 0 0)
				  ) 
				  (
				  	Return 
					(Just 
					(
						BinOp 
						(Var (Identifier "n" Nothing (Span 57 58)) [] (Span 0 0))
						(Mult (Span 59 60)) 
						(
							FunCall 
							(Identifier "facR" Nothing (Span 61 65)) 
							[
							 BinOp
							 (Var (Identifier "n" Nothing (Span 68 69)) [] (Span 0 0)) 
							 (Min (Span 70 71)) 
							 (ConstInt (Integer 1 (Span 72 73)) (Span 0 0)) 
							 (Span 0 0)
							] 
							(Span 0 0)
						) 
						(Span 0 0)
					))
					(Span 0 0)
				) 
				(Span 0 0)
			       ] 
			       (Span 0 0)
			      ]
			      (Span 0 0)

	-- *** STATEMENTS *** --	
	describe "statement parser" $ do		
		it "parses empty return statements" $
			doParseStatement "return;" `shouldBe`
				Return Nothing (Span 0 0)
		
		it "parses complex return statements" $
			doParseStatement "return (x%y);" `shouldBe`
				Return 
				(
				Just 
				(
					BinOp 
					(Var (Identifier "x" Nothing (Span 8 9)) [] (Span 0 0)) 
					(Mod (Span 9 10)) 
					(Var (Identifier "y" Nothing (Span 10 11)) [] (Span 0 0)) 
					(Span 0 0))
				) 
				(Span 0 0)
		
		it "parses block statement" $
			doParseStatement "{ { { { { { { True; } } } } } } }" `shouldBe`
				Block [
				 Block [
				  Block [
				   Block [
				    Block [
				     Block [
				      Block [
				       Expression (ConstBool (Boolean True (Span 14 18)) (Span 0 0)) (Span 0 0)
				       ] (Span 0 0)
				      ] (Span 0 0)
				     ] (Span 0 0)
				    ] (Span 0 0)
				   ] (Span 0 0)
				  ] (Span 0 0)
				 ] (Span 0 0)
		
		it "parses if statements" $
			doParseStatement "if(True) {x = 1337;}" `shouldBe`
				If 
				(ConstBool (Boolean True (Span 3 7)) (Span 0 0)) 
				(
					Block 
					[
					 Assignment 
					 (Identifier "x" Nothing (Span 10 11)) 
					 [] 
					 (ConstInt (Integer 1337 (Span 14 18)) (Span 0 0)) 
					 (Span 0 0)
					]
					(Span 0 0)
				) 
				(Span 0 0)
			
		it "parses ifElse statements" $
			doParseStatement "if(True) {x = 1337;} else {y = foo;}" `shouldBe`
				IfElse 
				(ConstBool (Boolean True (Span 3 7)) (Span 0 0)) 
				(
					Block 
					[
					 Assignment 
					 (Identifier "x" Nothing (Span 10 11)) 
					 [] 
					 (ConstInt (Integer 1337 (Span 14 18)) (Span 0 0)) 
					 (Span 0 0)
					]
					(Span 0 0)
				) 
				(
					Block 
					[
					 Assignment 
					 (Identifier "y" Nothing (Span 27 28)) 
					 [] 
					 (Var (Identifier "foo" Nothing (Span 31 34)) [] (Span 0 0)) 
					 (Span 0 0)
					]
					(Span 0 0)
				) 
				(Span 0 0)
						
		it "parses assignment statements" $
			doParseStatement "x = 1;" `shouldBe` 			
				Assignment
				(Identifier "x" Nothing (Span 0 1))
				[]
				(ConstInt (Integer 1 (Span 4 5)) (Span 0 0))
				(Span 0 0)
				
		it "parses assignment with expression statements" $
			doParseStatement "x = 3+4;" `shouldBe` 			
				Assignment
				(Identifier "x" Nothing (Span 0 1)) [] 
				(BinOp 
					(ConstInt (Integer 3 (Span 4 5)) (Span 0 0))
					(Plus (Span 5 6)) 
					(ConstInt (Integer 4 (Span 6 7)) (Span 0 0)) 
					(Span 0 0)
				) 
				(Span 0 0)
		
		it "parses multiplication as assignment statement" $
			doParseStatement "x = x+x;" `shouldBe` 			
				Assignment
				(Identifier "x" Nothing (Span 0 1))
				[] 
				(BinOp 
					(Var (Identifier "x" Nothing (Span 4 5)) [] (Span 0 0)) 
					(Plus (Span 5 6)) 
					(Var (Identifier "x" Nothing (Span 6 7)) [] (Span 0 0)) 
					(Span 0 0)
				)
				(Span 0 0)


	-- *** EXPRESSIONS *** --
	describe "expression parser" $ do
		it "parses function calls" $ do
			doParseExpression "foo();" `shouldBe`
				FunCall (Identifier "foo" Nothing (Span 0 3)) [] (Span 0 0)

		it "parses argumented function calls" $ do
			doParseExpression "foo(bar, baz);" `shouldBe`
				FunCall 
				(Identifier "foo" Nothing (Span 0 3)) 
				[
				 Var (Identifier "bar" Nothing (Span 4 7)) [] (Span 0 0),
				 Var (Identifier "baz" Nothing (Span 9 12)) [] (Span 0 0)
				] 
				(Span 0 0)
		
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
				(Plus (Span 1 2))
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
				 
		it "parses expressions with fields" $
			doParseExpression "camel_case123.fst.tl.snd.hd = (UPPERCASE,lowercase).fst" `shouldBe`
				Var 
				(Identifier "camel_case123" Nothing (Span 0 13)) 
				[
					First (Span 13 17),
					Tail (Span 17 20),
					Second (Span 20 24),
					Head (Span 24 27)
				] 
				(Span 0 0)	
				
		it "parses expressions with identifiers" $
			doParseExpression "x+x" `shouldBe`
				BinOp 
				(Var (Identifier "x" Nothing (Span 0 1)) [] (Span 0 0)) 
				(Plus (Span 1 2)) 
				(Var (Identifier "x" Nothing (Span 2 3)) [] (Span 0 0)) 
				(Span 0 0)
				
		it "parses boolean expressions" $
			doParseExpression "x+y+4 >= 12 && !(3+z!=13)" `shouldBe`
				BinOp 
				(
					BinOp 
					(
						BinOp 
						(Var (Identifier "x" Nothing (Span 0 1)) [] (Span 0 0))
						(Plus (Span 1 2))
						(
							BinOp 
							(Var (Identifier "y" Nothing (Span 2 3)) [] (Span 0 0))
							(Plus (Span 3 4))
							(ConstInt (Integer 4 (Span 4 5)) (Span 0 0)) 
							(Span 0 0)
						) 
						(Span 0 0)
					) 
					(Ge (Span 6 8))
					(ConstInt (Integer 12 (Span 9 11)) (Span 0 0)) 
					(Span 0 0)
				) 
				(And (Span 12 14))
				(
					UnOp 
					(Not (Span 15 16)) 
					(
						BinOp 
						(
							BinOp 
							(ConstInt (Integer 3 (Span 17 18)) (Span 0 0))
							(Plus (Span 18 19)) 
							(Var (Identifier "z" Nothing (Span 19 20)) [] (Span 0 0)) 
							(Span 0 0)
						) 
						(Neq (Span 20 22)) 
						(ConstInt (Integer 13 (Span 22 24)) (Span 0 0)) 
						(Span 0 0)
					) 
					(Span 0 0)
				) 
				(Span 0 0)

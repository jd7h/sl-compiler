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
	-- Sample test
	describe "null" $ do
		it "recognises the empty list" $
			null [] `shouldBe` True

	-- *** EXPRESSIONS *** --
	describe "SL tokenizer" $ do
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
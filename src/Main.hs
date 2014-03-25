import System.Environment
import System.Exit

import Control.Monad

import qualified Options
import qualified Tokenizer
import qualified Parser

-- type CompFun = Options -> String -> String -> [Token] -> IO (...)

main = do
	
	return ()
	
-- 	-- Argument parsing
-- 	(options, [file]) <- getArgs >>= Options.parseArguments
-- 	-- TODO: Clearly define console interface functions rather than using putStrLn. And snail. Definitely snail.
-- 	when (Options.snail options) $ putStrLn ("~~ ADD SNAIL HERE ~~")
-- 	when (Options.verbose options) $ putStrLn ("* Processing " ++ file ++ " *")
-- 
-- 	-- Read source file
-- 	source <- readFile file
-- 	when (Options.verbose options) $ putStrLn ("* Done reading file *")
-- 
-- 	-- Lex source
-- 	(tokenResult, tokenErrors) <- tokenize options file source (Just [])
-- 	
-- 	putStrLn(tokenResult)
-- 
-- 	exitSuccess
-- 		
-- 	
-- -- tokenize :: CompFun
-- tokenize options file source _ = do
-- 	let tokenResult = Tokenizer.lexStr (source, 0)
-- 	return $ case tokenResult of
-- 		Nothing -> ([], [])			-- TODO: Do error interpretation
-- 		Just x -> (x, [])
		

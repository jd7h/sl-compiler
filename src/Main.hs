import System.Environment
import System.Exit

import Control.Monad

import qualified Options
import qualified Tokenizer
import qualified Parser
import qualified AST
import qualified Utility as U

-- type CompFun = Options -> String -> String -> [Token] -> IO (...)

main = do
	
	-- Argument parsing
	(options, [file]) <- getArgs >>= Options.parseArguments
	-- TODO: Clearly define console interface functions rather than using putStrLn. And snail. Definitely snail.
	when (Options.snail options) $ putStrLn ("~~ ADD SNAIL HERE ~~")
	when (Options.verbose options) $ putStrLn ("* Processing " ++ file ++ " *")

	-- Read source file
	source <- readFile file
	when (Options.verbose options) $ putStrLn ("* Done reading file *")

	-- Lex source
	(tokenResult, tokenErrors) <- tokenize options file source (Just [])
	
	-- Parse tokens
	-- (parseResult, parseErrors) <- parse options

	exitSuccess


tokenize _ _ [] _ 		= return ([], "Tokenizer error: empty input")
tokenize options file source _ 	= do
	let tokenResult = Tokenizer.lexStr (source, 0)
	return $ case tokenResult of
		Nothing -> ([], "Tokenizer error: failed to generate tokens")
		Just x -> (filterComment x, [])
		-- TODO: No errors are generated in the tokenizer.

filterComment :: [Tokenizer.Token] -> [Tokenizer.Token]
filterComment tokens = filter (not . isComment) tokens
	where isComment (tokenE, _) = case tokenE of
		Tokenizer.Comment _ 	-> True
		_			-> False
		
parse _ _ _ [] = return (AST.Program [] (U.Span 0 0), "Parse error: nothing to parse")
parse options file source tokens =
	do
		let parseResult = Parser.parseLanguage tokens
		return $ case parseResult of
			Left e -> (AST.Program [] (U.Span 0 0), e)
			Right tree -> (tree, [])

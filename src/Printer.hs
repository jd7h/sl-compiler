module Printer where

import System.Console.ANSI

import AST
import Output

printTree :: Program a -> IO ()
printTree program = coloredPrinter $ output defaultMeta program

-- *** Colored Printer *** --
coloredPrinter :: MarkupString Language -> IO ()
coloredPrinter = printer putChar applyColor

applyColor :: StyleSpan Language -> IO ()
applyColor (Open s) = setSGR [SetColor Foreground Vivid (syntaxColor s)]
applyColor (Close s) = setSGR []

syntaxColor :: Language -> Color
syntaxColor Type = Green
syntaxColor Variable = Blue
syntaxColor Field = Cyan
syntaxColor Constant = Red
syntaxColor Keyword = Magenta
syntaxColor Function = Yellow

-- *** Main printer function *** --
printer :: Monad m => (Char -> m a) -> (StyleSpan Language -> m a) -> MarkupString Language -> m a
printer f g [] = f '\n'
printer f g (Left x : xs) = f x >> printer f g xs
printer f g (Right x :xs) = g x >> printer f g xs
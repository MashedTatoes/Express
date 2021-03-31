module Main where
import Core.Interpreter
import Core.Lexer
import Core.Parser
import Control.Monad
main::IO ()
main =
    do 
        putStr "> "
        expression <- getLine 
        unless (expression == "exit") $ do
            
            print $ interactive expression
            main



interactive :: [Char] -> Token
interactive expression = interpret $ parse $ tokenize expression
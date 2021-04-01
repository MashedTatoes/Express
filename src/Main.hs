module Main where
import Express.Interpreter ( interpret )
import Express.Lexer ( Token, tokenize )
import Express.Parser ( parse )
import Control.Monad ( unless )
import System.IO
main::IO ()
main =
    do 
        
        expression <- prompt "> " 
        unless (expression == "exit") $ do
            
            print $ interactive expression
            main

prompt :: String -> IO String 
prompt text = do
    putStr  text
    hFlush stdout
    getLine

interactive :: [Char] -> Token
interactive expression = interpret $ parse $ tokenize expression
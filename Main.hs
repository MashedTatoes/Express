module Main where
import Core (interpret,tokenize,parse,Token)
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
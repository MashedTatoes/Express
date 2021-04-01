module Express.Lexer where
import Text.Read ( readMaybe )
import Data.Char ()
import Data.Maybe ( isJust )
data Token =
    InvalidSyntax 
    | OpAdd
    | OpMinus
    | OpMul
    | BracketOpen
    | BracketClose
    | NumInt Int
    deriving(Show,Eq,Ord)

collectToken :: [Char] -> [Char]
collectToken [] = []
collectToken (' ':xs) = ""
collectToken (x:xs) = x:collectToken xs


tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize xs
    | token == "+" = OpAdd : tokenize remainder
    | token == " " = tokenize remainder
    | token == "-" = OpMinus : tokenize remainder
    | token == "*" = OpMul : tokenize remainder
    | token == ")" = BracketOpen : tokenize remainder
    | token == "(" = BracketClose : tokenize remainder
    | isJust (readMaybe token :: Maybe Integer)  =  NumInt (read token) : tokenize remainder
    where 
        token = collectToken xs
        (_,remainder) = splitAt (1 +  length token ) xs 
module Core  where 

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


data ParseTree =
    EmptyTree 
    | Node Token 
    | Operation Token ParseTree ParseTree
    deriving(Show)

class Eval a where 
    eval :: a -> a -> a -> a
instance Eval Token where
    eval OpAdd (NumInt x) (NumInt y) = NumInt $ x + y
    eval OpMinus (NumInt x) (NumInt y) = NumInt $ x - y
    eval OpMul (NumInt x) (NumInt y) = NumInt $ x * y
    
    eval _ _ _ = InvalidSyntax

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

findTokenPositions :: Int -> Token -> [Token] -> [Int]
findTokenPositions _ _ [] = []
findTokenPositions pos token xs
    | x == token = pos:findTokenPositions next token (tail xs)
    | x /= token = findTokenPositions next token (tail xs)
    where 
        x = head xs
        next = pos + 1

parse :: [Token] -> ParseTree
parse [] = EmptyTree
parse [x] = Node x
parse xs =
    let operationPoints = findTokenPositions 0 (minimum xs) xs
        (left,right) = splitAt (head operationPoints) xs
        op = head right
    in
        Operation op (parse left) (parse $ tail right)
interpret :: ParseTree -> Token
interpret EmptyTree = NumInt 0
interpret (Node x) = x
interpret (Operation op left right) =  eval op (interpret left) (interpret right)


    



    



import Text.Read
import Data.Char

data Token =
    InvalidSyntax 
    | NumInt Int
    | OpAdd
    | OpMinus
    | BracketOpen
    | BracketClose
    deriving(Show,Eq)

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
    eval _ _ _ = InvalidSyntax

lexer :: [Char] -> [Token]
lexer [] = []
lexer (x:xs)
    | x == '+' = OpAdd : lexer xs
    | x == ' ' = lexer xs
    | x == '-' = OpMinus : lexer xs
    | x == '(' = BracketOpen : lexer xs
    | x == ')' = BracketClose : lexer xs
    | isDigit x =  NumInt (digitToInt x) : lexer xs

parse :: [Token] -> ParseTree
parse [] = EmptyTree
parse [x] = Node x
parse ((NumInt x):xs) = Operation (head xs) (parse [NumInt x]) (parse $ tail xs )
parse (x:xs) 
    | x `elem` [OpAdd,OpMinus] = Operation x (parse [head xs]) (parse $ tail xs)


interpret :: ParseTree -> Token
interpret EmptyTree = NumInt 0
interpret (Node x) = x
interpret (Operation op left right) =  eval op (interpret left) (interpret right)
    



    



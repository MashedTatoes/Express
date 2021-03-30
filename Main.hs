import Text.Read
import Data.Char

data Token =
    InvalidToken 
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
    eval _ _ _ = InvalidToken

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
parse (x:y:z:xs)
    | x `elem` [OpAdd,OpMinus] = case (x,y,z) of 
        (_,NumInt n1,NumInt n2) -> Operation x (parse [NumInt n1]) (parse (z:xs))
        (_, NumInt n1, _) -> Operation x (parse [NumInt n1]) (parse (z:xs))


interpret :: ParseTree -> Token
interpret EmptyTree = NumInt 0
interpret (Node x) = x
interpret (Operation op left right) =  eval op (interpret left) (interpret right)
    



    



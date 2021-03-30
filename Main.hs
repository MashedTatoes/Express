import Text.Read
import Data.Char
import Data.Maybe

data Token =
    InvalidSyntax 
    | NumInt Int
    | OpAdd
    | OpMinus
    | OpMul
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
    eval OpMul (NumInt x) (NumInt y) = NumInt $ x * y
    
    eval _ _ _ = InvalidSyntax

lexer :: [Char] -> [Token]
lexer [] = []
lexer xs
    | filterToken == "+" = OpAdd : lexer remainder
    | filterToken == " " = lexer remainder
    | filterToken == "-" = OpMinus : lexer remainder
    | filterToken == "*" = OpMul : lexer remainder
    | filterToken == ")" = BracketOpen : lexer remainder
    | filterToken == "(" = BracketClose : lexer remainder
    | isJust (readMaybe filterToken :: Maybe Integer)  =  NumInt (read filterToken) : lexer remainder
    where 
        (token,remainder) = splitAt (1 + length ( takeWhile(/= ' ') xs )) xs 
        filterToken = filter (/= ' ') token
parse :: [Token] -> ParseTree
parse [] = EmptyTree
parse [x] = Node x
parse ((NumInt x):xs) = Operation (head xs) (parse [NumInt x]) (parse $ tail xs )
parse (x:xs) 
    | x `elem` [OpAdd,OpMinus,OpMul] = Operation x (parse [head xs]) (parse $ tail xs)


interpret :: ParseTree -> Token
interpret EmptyTree = NumInt 0
interpret (Node x) = x
interpret (Operation op left right) =  eval op (interpret left) (interpret right)
    



    



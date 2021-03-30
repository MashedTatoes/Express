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

collectToken :: [Char] -> [Char]
collectToken [] = []
collectToken (' ':xs) = ""
collectToken (x:xs) = x:collectToken xs


lexer :: [Char] -> [Token]
lexer [] = []
lexer xs
    | token == "+" = OpAdd : lexer remainder
    | token == " " = lexer remainder
    | token == "-" = OpMinus : lexer remainder
    | token == "*" = OpMul : lexer remainder
    | token == ")" = BracketOpen : lexer remainder
    | token == "(" = BracketClose : lexer remainder
    | isJust (readMaybe token :: Maybe Integer)  =  NumInt (read token) : lexer remainder
    where 
        token = collectToken xs
        (_,remainder) = splitAt (1 +  length token ) xs 
        
parse :: [Token] -> ParseTree
parse [] = EmptyTree
parse [x] = Node x
parse ((NumInt x):xs) = Operation (head xs) (parse [NumInt x]) (parse $ tail xs )



interpret :: ParseTree -> Token
interpret EmptyTree = NumInt 0
interpret (Node x) = x
interpret (Operation op left right) =  eval op (interpret left) (interpret right)


    



    



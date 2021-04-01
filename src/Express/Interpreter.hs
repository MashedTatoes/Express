module Express.Interpreter where
import Express.Lexer( Token(OpAdd, OpMinus, OpMul, InvalidSyntax, NumInt) )
import Express.Parser ( ParseTree(..) )

class Eval a where 
    eval :: a -> a -> a -> a
    
instance Eval Token where
    eval OpAdd (NumInt x) (NumInt y) = NumInt $ x + y
    eval OpMinus (NumInt x) (NumInt y) = NumInt $ x - y
    eval OpMul (NumInt x) (NumInt y) = NumInt $ x * y
    
    eval _ _ _ = InvalidSyntax

interpret :: ParseTree -> Token
interpret EmptyTree = NumInt 0
interpret (Node x) = x
interpret (Operation op left right) =  eval op (interpret left) (interpret right)
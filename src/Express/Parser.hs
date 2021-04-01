module Express.Parser where
import Express.Lexer ( Token )


data ParseTree =
    EmptyTree 
    | Node Token 
    | Operation Token ParseTree ParseTree
    deriving(Show)

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
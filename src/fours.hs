import           Control.Applicative hiding (many, (<|>))
import           Control.Monad
import           Data.List
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

opss = "*/+-"
ops = map (:[]) opss

interleave:: [String] -> [String] -> [String]
interleave [x] _ = [x]
interleave xs [] = xs
interleave xss@(x:xx:xs)(y:ys)
        | x == "(" || xx == ")" = x : interleave (xx:xs) (y:ys)
        | otherwise = x : y : interleave (xx:xs) ys

parenthesize:: (Show a) => [a] -> [a] -> [[String]]
parenthesize [] [] = []
parenthesize [] [x] = [[show x]]
parenthesize [x] [] = [[show x]]
parenthesize [] (x:xs) = parenthesize [x] xs
parenthesize xs [] = []
parenthesize xss@(x:xs) yss@(y:ys) = ((++) <$> parens xss <*> parens yss) ++ next
    where parens zs
            | length zs == 1 = parenthesize [] zs
            | otherwise = map addParens . parenthesize [] $ zs
          next = parenthesize ((x:xs) ++ [y]) ys

addParens s = ["("] ++ s ++ [")"]

sequenceN = replicateM

--List of numbers to choose from -> number of numbers -> all eqs
allEqs:: [Int] -> Int -> [String]
allEqs xs n = map concat (interleave <$> parens <*> nOps)
      where parens = concatMap (parenthesize []) . sequenceN n $ xs
            nOps = sequenceN (n-1) ops

--List of specific numbers -> all Eqs
specAllEqs:: [Int] -> [String]
specAllEqs xs = map concat (interleave <$> parens <*> nOps)
      where parens = concatMap (parenthesize []) . nub . permutations $ xs
            nOps = sequenceN (length xs - 1) ops

specEqs:: [Int] -> [String]
specEqs xs = map concat (interleave <$> parenthesize [] xs <*> nOps)
        where nOps = sequenceN (length xs - 1) ops

num::Parser Float
num = do
    spaces
    x <- oneOf ['1'..'9']
    xs <- many . oneOf $ ['0'..'9']
    spaces
    return $ read (x:xs)

tChar:: Char -> Parser String
tChar c = do
           spaces
           x <- char c
           spaces
           return [x]

factor::Parser Float
factor = (do
            tChar '('
            val <- expr
            tChar ')'
            return val) <|> num

component = do x <- factor
               (do tChar '*'
                   y <- component
                   return $ x * y) <|> (do tChar '/'
                                           y <- component
                                           return $ x / y) <|> return x
expr = do x <- component
          (do tChar '+'
              y <- expr
              return $ x + y) <|> (do tChar '-'
                                      y <- expr
                                      return $ x - y) <|> return x

evalList [] = []
evalList (x:xs) = case eval x of
                    (Right n) -> (x,n):evalList xs
                    (Left _)  -> evalList xs



eval = parse expr []




solve:: [Int] -> Float -> [String]
solve xs x = map (\(a,b) -> a ++ " = " ++ show b) . filter (\(a,b) -> b == x) . evalList . specAllEqs $ xs

main = putStr "hello world"

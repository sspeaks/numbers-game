module Fours (prettyAnswer) where
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe

data Op = Multiply | Divide | Add | Subtract
data Expr = Val Float | Tree Op Expr Expr

evalExpr (Val x) = Just x
evalExpr (Tree o l r) = case o of
    Multiply -> liftM2 (*) left right
    Add -> liftM2 (+) left right
    Subtract -> liftM2 (-) left right
    Divide -> case right of
        (Just 0) -> Nothing
        _ -> liftM2 (/) left right
  where left = evalExpr l
        right = evalExpr r

instance Show Expr where
    show (Val x) = show x
    show t@(Tree o l r) = case o of
        Multiply -> "(" ++ show l ++ "*" ++ show r ++ ")"
        Add -> "(" ++ show l ++ "+" ++ show r ++ ")"
        Subtract -> "(" ++ show l ++ "-" ++ show r ++ ")"
        Divide -> "(" ++ show l ++ "/" ++ show r ++ ")"

splits xs = init . tail $ zip (inits xs) (tails xs)

combos l r = [id] <*> (Tree <$> [Add, Subtract, Multiply, Divide] <*> [l] <*> [r])

trees:: [Float] -> [Expr]
trees [x] = [id] <*> [Val x]
trees xs = do
    (f, s) <- splits xs
    l <- trees f
    r <- trees s
    combos l r

results :: [Float] -> IO ()
results xs = putStrLn . unlines $ zipWith (\x y  -> show x ++ "=" ++ show y) validTrees results
    where validTrees = mfilter (isJust . evalExpr) $ trees xs
          results =  mapMaybe evalExpr $ trees xs

isAnswer :: Float -> Maybe Float -> Bool
isAnswer _ Nothing = False
isAnswer y (Just x) = x == y
       
answer :: [Float] -> Float -> [Expr]
answer xs x = mfilter (\v -> isAnswer x (evalExpr v)) $ trees xs

prettyAnswer :: [Float] -> Float -> [String]
prettyAnswer xs x = map (\exp -> show exp ++ "=" ++ show x) $ res
    where res = answer xs x
    
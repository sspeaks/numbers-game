import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Math.Gamma

data Op = Multiply | Divide | Add | Subtract
data Expr = Val Float | Fac Expr | Sqrt Expr | Tqrt Expr | Tree Op Expr Expr

evalExpr (Val x) = Just x
evalExpr (Fac x) = gamma <$> ((+1) <$> (evalExpr x))
evalExpr (Sqrt x) = sqrt <$> (evalExpr x)
evalExpr (Tqrt x) = (** (1/3)) <$> (evalExpr x) 
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
    show (Fac x) = show x ++ "!"
    show (Sqrt x) = show x ++ "SR"
    show (Tqrt x) = show x ++ "CR"
    show t@(Tree o l r) = case o of
        Multiply -> "(" ++ show l ++ "*" ++ show r ++ ")"
        Add -> "(" ++ show l ++ "+" ++ show r ++ ")"
        Subtract -> "(" ++ show l ++ "-" ++ show r ++ ")"
        Divide -> "(" ++ show l ++ "/" ++ show r ++ ")"

splits xs = init . tail $ zip (inits xs) (tails xs)

combos l r = [id, Fac, Sqrt, Tqrt] <*> (Tree <$> [Add, Subtract, Multiply, Divide] <*> [l] <*> [r])

trees:: [Float] -> [Expr]
trees [x] = [id, Fac, Sqrt, Tqrt] <*> [Val x]
trees xs = do
    (f, s) <- splits xs
    l <- trees f
    r <- trees s
    combos l r

results :: [Float] -> IO ()
results xs = putStrLn . unlines $ zipWith (\x y  -> show x ++ "=" ++ show y) validTrees results
    where validTrees = mfilter (\t -> evalExpr t /= Nothing) $ trees xs
          results =  mapMaybe evalExpr $ trees xs

isAnswer :: Float -> Maybe Float -> Bool
isAnswer _ Nothing = False
isAnswer y (Just x) = x == y
       
answer :: [Float] -> Float -> [Expr]
answer xs x = mfilter (\v -> isAnswer x (evalExpr v)) $ trees xs

prettyAnswer :: [Float] -> Float -> String
prettyAnswer xs x = unlines . map (\exp -> show exp ++ "=" ++ show x) $ res
    where res = answer xs x
    
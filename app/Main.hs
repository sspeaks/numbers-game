module Main where

import           Fours
import           System.Environment
import           System.Exit
import           Data.List
import           Control.Monad

main :: IO ()
main = do
    ls <- fmap (map read) getArgs :: IO [Float]
    if length ls < 2
        then die "You must provide atleast 3 numbers"
        else putStrLn . unlines $ prettyAnswer (init ls) (last ls)


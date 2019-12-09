module Main where

import           Control.Monad
import           Data.List
import           Fours
import           System.Environment
import           System.Exit

main :: IO ()
main = do
    ls <- fmap (map read) getArgs :: IO [Float]
    if length ls < 2
        then die "You must provide atleast 3 numbers"
        else putStrLn . unlines $ prettyAnswer (init ls) (last ls)


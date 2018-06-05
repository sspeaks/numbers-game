module Main where

import 			 Lib
import           System.Environment
import			 System.Exit
import 			 Data.List
import 			 Control.Monad

main :: IO ()
main = do
	ls <- (liftM (map read) $ getArgs)::IO [Int]
	if (length ls) < 2
		then die "You must provide atleast 3 numbers"
		else putStrLn . unlines $ solve (init ls) (fromIntegral $ last ls)
				  
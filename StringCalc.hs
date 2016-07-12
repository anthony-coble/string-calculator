module StringCalc 
( calc
) where

import Data.List.Split (splitOn)


calc :: String -> Integer
calc "" = 0
calc ('/':'/':delimiter:'\n':ns) = sumString delimiter ns
calc s = sumString ','
       $ map (mapdelimiter) s
  where mapdelimiter x | x `elem` ",\n" = ','
                       | otherwise = x

sumString :: Char -> String -> Integer
sumString delimiter = foldl (+) 0
                    . noNegatives
                    . map (read)
                    . splitOn [delimiter] 

-- | This feels so wrong, why am I introducing 
-- an exception to pure code?
noNegatives :: [Integer] -> [Integer]
noNegatives nums = case (filter (<0) nums) of
                     [] -> nums
                     as -> error $ "Negatives not allowed: " ++ (show as)

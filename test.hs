import Data.Char
lucasnum :: Int -> Integer
lucasnum n |n>=0 =  resValueUnsigned
           |otherwise =  if odd n then (- resValueUnsigned) else resValueUnsigned
           
           where posLucas xs n =  if n == 0 then 2 else if n == 1 then head xs else posLucas ((sum xs):[(head xs)]) (n-1)
                 resValueUnsigned = posLucas [1,2] (abs n)
                 
testfun_1 str c = (map (((==) (toLower c)).toLower) str)

cardGame :: Int -> Int
cardGame 0 = 0
cardGame 1 = 1
cardGame n |n `mod` 4 == 0 && n `div` 4 > 1 = 1 + cardGame (newAmountOfCards (n-1))
           |n `mod` 4 == 0 = n `div` 2 + cardGame (newAmountOfCards (n `div` 2))
           |n `mod` 2 == 0 = n `div` 2 + cardGame (newAmountOfCards (n `div` 2))
           |otherwise = 1 + cardGame (newAmountOfCards (n-1))
           
           where newAmountOfCards n1|  n1`mod` 4 == 0  && n1 `div` 4 > 1 =  n1 - 1
                                    | n1 `mod` 4 == 0 = n1 `div` 2
                                    |  n1 `mod` 2 == 0 =  n1 `div` 2
                                    | otherwise =  n1 - 1
                 
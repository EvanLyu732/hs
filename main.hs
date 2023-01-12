capital :: String->String
capital "" = "Empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

densityTell :: (RealFloat a) => a -> String
densityTell density 
  | density < 1.2 = "wow"
  | density <= 2.2 = "wow 2"
  | otherwise = ":)"

-- head' :: [a] -> a
-- head' [] = error "no head for empty files"
-- head' (x: _) = x
--
-- case expression of pattern -> result
--                    pattern -> result
--                    pattern -> result
head' :: [a] -> a
head' xs = case xs of [] -> error "no head for empty files"
                      (x: _) -> x

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x]= x
maximum' (x: xs) 
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- replicate
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1)x

-- take
-- take' :: (Num i, Ord i) => i -> [a] -> [a]
-- take' n 
--      | n <= 0  = []
-- take' _ [] = []
-- take' n (x: xs) = x : take' (n-1)xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x: xs) = reverse' xs ++ [x]

-- repeat' :: a -> [a]
-- repeat' x = x: repeat' x
--
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' []_ = []
zip' (x: xs)(y: ys) = (x, y): zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x: xs) =
      let smallerBound = quicksort [a | a<-xs, a<=x]
          biggerBound = quicksort [a | a<-xs, a>x]
      in smallerBound ++ [x] ++ biggerBound
 
main =  do 
  print (capital "Dracula")
  print (capital "Yes")
  print (densityTell 0)
  print (let square x = x * x in (square 5, square 3, square 2))
  print (maximum' [1, 2, 3, 4 ,5])
  print (replicate' 5 5)
  print (reverse' [1, 2, 3])
  print (zip' [1,2,3] ['a', 'b', 'c'])
  print (quicksort [6, 10, 8, 2, 5])

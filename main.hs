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

applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)

-- mind blow
zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x: xs) (y: ys) = f x y :zipWith' f xs ys

flip' :: (a->b->c) -> (b->a->c)
flip' f = g
    where g x y = f y x

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x: xs) = f x: map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
      | p x = x :filter p xs
      | otherwise = filter p xs

largestDiv :: (Integral a) => a
largestDiv = head (filter p [10000, 9999..])
    where p x = x `mod` 2369 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain(n `div` 2)
  | odd n = n:chain(n*3 + 1)

-- lambda
numLongChains :: Int
numLongChains = length(filter (\xs -> length xs > 10) (map chain [1..100]))
 
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
  print (applyTwice (+3) 10)
  print (zipWith' max [4,2,3] [5,6,9])
  print (flip' zip [1,2,3,4,5] "Hello")
  print (map' (+3) [1,2,3,4,5])
  print (filter' (>3) [6,2,6,2,3,7,9])
  print (largestDiv)
  print (sum (takeWhile (<10000) [n^2 | n<-[1..], odd(n^2)]))
  print (chain 10)
  print (numLongChains)
  print (map (\(a,b) -> a+b) [(1,2), (3,4), (5,6)])


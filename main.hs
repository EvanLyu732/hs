capital :: String->String
capital "" = "Empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

densityTell :: (RealFloat a) => a -> String
densityTell density 
  | density < 1.2 = "wow"
  | density <= 2.2 = "wow 2" | otherwise = ":)"

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

-- fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum2' :: (Num a) => [a] -> a
sum2' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map2' :: (a->b) -> [a] -> [b]
map2' f fs = foldr (\x acc -> f x: acc) [] fs

-- 
maximum2' :: (Ord a) => [a] -> a
maximum2'  = foldr1 (\x acc -> if x > acc then x else acc)

reverse2' :: [a] -> [a]
reverse2' = foldl (\acc x -> x : acc) []

product2' :: (Num a) => [a] -> a
product2' = foldl1 (*)

-- filter2' :: (a -> Bool) -> [a] -> [a]
-- filter2' p = foldr (\acc x -> if p x then x : acc else acc) []

head2' :: [a] -> a
head2' = foldr1 (\x _ -> x)

last2' :: [a] -> a
last2' = foldl1 (\_ x -> x)

-- dollar 
-- res = sum (filter (> 10) (map (*2) [2..10]))
res = sum $ filter (> 10) $ map (*2) [2..10]

-- (.) :: (b->c) -> (a->b) -> a -> c
-- f.g = \x -> f (g x)
--
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum2 :: Integer
oddSquareSum2 = 
  let oddSquareSum = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (< 10000) oddSquareSum
  in sum belowLimit
 
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
  print (sum' [3,5,2,1])
  print (sum2' [3,5,2,1])
  print (map2' (+3) [3,5,2,1])
  print (maximum2' [3,5,2,1])
  print (reverse2' [3,5,2,1])
  print (product2' [3,5,2,1])
  print (head2' [3,5,2,1])
  print (last2' [3,5,2,1])
  -- scanl
  print (scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,5,8,9,6,7])
  print (scanl (flip (:)) [] [3,2,1])

  print (scanr1 (\acc x -> if x > acc then x else acc) [3,4,5,5,8,9,6,7])
  print (res)

  print (map (\x -> negate (abs x)) [-2,3,5,8,-9])
  print (map (negate . abs) [-2,3,5,8,-9])

  print (map (negate . sum . tail) [[1..5], [3..6], [1..7]])

  print (replicate 100 . product . map (* 3) . zipWith max [1,2,3,4,5] $ [4, 5, 6, 7, 8])
  print (oddSquareSum)
  print (oddSquareSum2)

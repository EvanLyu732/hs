capital :: String->String
capital "" = "Empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

densityTell :: (RealFloat a) => a -> String
densityTell density 
  | density < 1.2 = "wow"
  | density <= 2.2 = "wow 2"
  | otherwise = ":)"

main =  do 
  print (capital "Dracula")
  print (capital "Yes")
  print (densityTell 0)
  print (let square x = x * x in (square 5, square 3, square 2))

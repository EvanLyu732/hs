module Geometry 
(
  Country(..)
) where


data Country = WaKanDa Int  | Altanlantis Int 

data Person = Person String String String Int Float String String deriving(Show)

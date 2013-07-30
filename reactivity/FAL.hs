module Fal where

import SOE hiding (Region, Event)
import qualified SOE as G (Region, Event)
import Animation (picToGraphic)
import Shape
import Picture
import Memo

infixr 1 =>>, ->>
infixr 1 `untilB`, `switch`, `stepAccum`, `step`
infixl 0 .|.
infixr 4 <*, >*
infixr 3 &&*
infixr 2 ||*

type Time = Float
type UserAction = G.Event

newtype Behaviour1 a = Behaviour1 ([(UserAction, Time)]  -> Time -> a)

-- Example function
inList :: [Int] -> Int -> Bool

result2 :: Bool
result2 = manyInList xs ys

manyInList               :: [Int] -> [Int] -> Bool
manyInList [] ys         = map (\_ -> False) ys
manyInList _ []          = []
manyInList (x:xs) (y:ys) = if x < y 
                           then manyInList xs (y:ys)
                           else (x == y) : manyInList (x:xs) ys




dadd7 x = 2*x + 7

dadd13 x = 2*x + 13

dsub1 x = 2*x - 1

dadd x y = 2*x + y

add7All [] = []
add7All (x:xs) = (x+7):(add7All xs)

add13All [] = []
add13All (x:xs) = (x+13):(add13All xs)

absLst [] = []
absLst (x:xs) = (abs x):(absLst xs)


doToAll f [] = []
doToAll f (x:xs) = (f x):(doToAll f xs)

addThreeDiv x y z = (x + y) `div` z


singletons lst = map help lst
  where help x = [x]

positives lst = filter (>0) lst

uppers lst = filter (`elem` ['A'..'Z']) lst

select :: (a -> Bool) -> [a] -> [a]
select p [] = []
select p (x:xs) = if p x 
                  then x:(select p xs)
                  else select p xs

evens lst = filter isEven lst
  where isEven x = (x `mod` 2) == 0

f = \x -> x + 3

g x = x + 3

multPairs :: [Int] -> [Int] -> [Int]
multPairs (x:xs) (y:ys) = (x*y):(multPairs xs ys)
multPairs [] [] = []

concatPairs :: [String] -> [String] -> [STring]
concatPairs (x:xs) (y:ys) = (x++y):(concatPairs xs ys)
concatPairs [] [] = []



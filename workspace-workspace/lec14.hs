find :: (a ->  Bool) -> [a] -> a
find p lst = let results = (filter p lst)
             in if null results
                then error "Find could not find. AAAAAAAAAAAAA"
                else head results

concatenate :: [String] -> String
concatenate [] = ""
concatenate (s:ss) = s ++ (concatenate ss)

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

katamari :: (b -> a -> b) -> b-> [a] -> b
katamari f b [] = b
--katamari f b (x:xs) = f x (katamari f b xs)
katamari f b (x:xs) = katamari f (f b x) xs

myElem y lst = foldr (\acc x -> if x == y then True else acc) False lst
{-
myElem y lst = find (== y) lst
myElem y [] = False
myElem y (x:xs) = if x == y then True else myElem y xs
-}
count :: Eq a => a -> [a] -> Int
count y lst = foldl (\acc x -> if y == x then 1 + acc else acc) 0 lst

myMaximum [] = error "No maximum of an empty list."
myMaximum lst = foldl (\acc x -> if x > acc then x else acc) (head lst) lst


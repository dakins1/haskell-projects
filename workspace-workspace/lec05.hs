import Data.List
import Debug.Trace
intersect :: Eq a => [a] -> [a] -> [a]
intersect as bs = [x | x <- as, x `elem` bs] 

crossproduct :: [a] -> [b] -> [(a,b)]
crossproduct as bs =[(x,y) | x <- as, y <- bs] 

union :: Eq a => [a] -> [a] -> [a]
union as bs = nub $ as ++ bs

--subset as bs = and [x `elem` bs | x <- as]
subset as bs = null [x | x <- as, x `notElem` bs]

--powerset lst = [ bs | bs `subset` lst]

myLength [] = 0
myLength (x:xs) =1+ (myLength xs)

numNonZero [] = 0
numNonZero (0:xs) = numNonZero xs 
--numNonZero (x:xs) = if x == 0 else numNonZero xs else 1 + numNonZero xs 

--myMaximum :: Ord a => [a] -> a
myMaximum [] = error "AAAH"
myMaximum [x] = x
myMaximum (x:xs) = 
    let maxxs = myMaximum xs
    in if x > maxxs then x else maxxs
--do not do this
{-myMaximum (x:y:xs) = 
    if x > y 
    then myMaximum (x:xs) 
    else myMaximum (y:xs)-}

secondToLast [] = error "Oops"
secondToLast [x] = error "Oops"
secondToLast [x,y] = x
secondToLast (x:xs) = secondToLast xs


--secondToLast [7,3,1,5,9] == 5
--
nodups :: Ord a => [a] -> [a] 
nodups lst = aux (sort lst)
   where aux [] = []
         aux (x:xs) = let uxs = aux xs
                      in if null xs || x /= head (uxs)
                         then x:uxs
                         else uxs

occurancesOfHead :: Eq a => [a] -> Integer
occurancesOfHead [] = error "Failed to yote"
occurancesOfHead [x] = 1
occurancesOfHead (x:xs) = 1 + aux xs
    where aux [] = 0
          aux (y:ys) = if x == y 
                       then 1 + aux ys
                       else aux ys

count :: Eq a => a -> [a] -> Integer
count y [] = 0
count y (x:xs) = if y == x 
                 then 1 + count y xs
                 else count y xs

rev :: [a] -> [a]
rev lst = aux lst []
  where aux [] acc = acc
        aux (x:xs) acc = aux xs (x:acc)

addToEnd y [] = y:[]
addToEnd y (x:xs) = x:(addToEnd y xs)

secondLargest [] = error "AAAH"
secondLargest [x] = error "AAAH"
secondLargest [x,y] = min x y
secondLargest (x:xs) = 
  let lxs = maximum xs
      slxs = secondLargest xs
  in if x >= lxs
     then lxs
     else if x >= slxs
          then x
          else slxs

bSL (x:y:xs) | x > y = aux x y xs
             | otherwise  = aux y x xs
          where aux l sl [] = sl
                aux l sl (x:xs) 
                      | x > l  = aux x l xs
                      | x > sl = aux l x xs
                      | otherwise = aux l sl xs
bsl _ = error "List too short" 

hoursToSchedule :: [(Int, Int)] -> Int
hoursToSchedule tsks = let (h,m) = sumTimes tsks
                       in if m > 0 then h+1 else h
  where sumTimes :: [(Int, Int)] -> (Int, Int)
        sumTimes [] = (0,0)
        sumTimes ((h,m):xs) = 
            let (hrest, mrest) = sumTimes xs
                htotal = if mrest+m > 60 then hrest+h+1 else hrest+h
                mtotal = if mrest+m > 60 then mrest+m-60 else mrest+m
            in (htotal, mtotal)
fib :: Integer -> Integer
fib n = aux 2 1 1
    where aux k fkm fkmm 
              | k == n = fkm + fkmm
              | otherwise = let fk = fkm + fkmm
                            in aux (k+1) fk fkm

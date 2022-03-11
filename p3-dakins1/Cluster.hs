module Cluster where
import ClusterData (tenPoints)
import Data.List
import Data.Maybe
import Debug.Trace

--Points are an x coordinate, a y coordinate, and an integer label.
type Point = (Double, Double, Int)
type Center = Point
--A cluster is a center and a list of points.
type Cluster = (Center, [Point])

-- All undefined values and functions should be completed. Your code will compile and test 
-- -- (with the -- test flag) even if some functions are left undefined.
--
-- --                                       Milestone
--

--Given a list of elements and an integer k, return k evenly spaced elements of the list.
--As a first attempt, simply take the first k elements. Fix this after you finish the rest of the
--project.
getKElems :: Int -> [a] -> [a]
getKElems k lst = let aux :: Int -> Int -> [a] -> [a]
                      aux k step (x:xs) = if (step == k) then []
                                          else x:(aux k (step+1) xs)
                    in aux k 0 lst
{-
getKElems k lst = let aux :: Int -> Int -> [a] -> [a]
                      aux 0 step lst = []
                      aux k step [] = []
                      aux k step (x:xs) = --if (step == (length lst)) then [] 
                                          if ((step `mod` k) == ((length lst) `mod` k))
                                               then x:(aux k (step+1) xs)
                                               else aux k (step+1) xs
                   in aux k 1 lst

getKGroups :: Int -> [a] -> [[a]]
getKGroups k lst = let aux :: Int -> Int -> [a] -> [a] -> [[a]]
                       aux k step build (x:xs) = if (step == (length lst)) then [[]] 
                                                 else if ((length build) /= k) 
                                                 then aux k (step+1) x:build xs
                                                 else aux k (step+1) [] xs
                                                     
                    in 
-}
--((length (x:xs)) `mod` k)) 
--for future, change k==step to k==((length lst) `mod` k) or something similar?
--Example: getKElems 3 [1..6] = [1,3,6]
--Return the Euclidean distance between two points. You may work in squared distances if you
--prefer.
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x
trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

eucDist :: Point -> Point -> Double
eucDist p1 p2 = let x1 = fst3 p1
                    y1 = snd3 p1
                    x2 = fst3 p2
                    y2 = snd3 p2
                    a = (x2 - x1)^2
                    b = (y2 - y1)^2
                 in sqrt (a + b)
    
--Example: eucDist (0,0,10) (1,1,10) < eucDist (0,0,10) (0,2,10)

--Return the Manhattan distance between two points: the distance between the points based on
--strictly horizontal and vertical movement, as if on a city grid.
manhatDist :: Point -> Point -> Double
manhatDist p1 p2 = let x1 = fst3 p1
                       y1 = snd3 p1
                       x2 = fst3 p2
                       y2 = snd3 p2
                    in (abs (x2 - x1)) + (abs (y2 - y1))
--Example: manhatDist (0,0,10) (1,1,10) == manhatDist (0,0,10) (0,2, 10)

--Return the Chebyshev distance between two points: the maximum between the x-distance and the
--y-distance, as if diagonal movement was free, like for a King in chess.
chebyDist :: Point -> Point -> Double
chebyDist p1 p2 = let a = abs (fst3 p2 - fst3 p1)
                      b = abs (snd3 p2 - snd3 p1)
                  in max a b
--Example: chebyDist (0,0,10) (0,5,10) == chebyDist (0,0,10) (5,5,10)

--Return the traffic-aware Manhattan distance: count horizontal distances twice as much as vertical.
trafficDist :: Point -> Point -> Double
trafficDist p1 p2 = let x1 = fst3 p1
                        y1 = snd3 p1
                        x2 = fst3 p2
                        y2 = snd3 p2
                    in (2* abs (x2 - x1)) + (abs (y2 - y1))

--Example: trafficDist (0,0,10) (0,10,10) == trafficDist (0,0,10) (5,0,10)

--Return the township-aware Manhattan distance. The label of the point is taken to be the township
--the point is in.  A penalty factor of 2 is applied to the distance between points in different
--townships.
townshipDist :: Point -> Point -> Double
townshipDist p1 p2 = if (trd3 p1) /= (trd3 p2)
                     then 2*(manhatDist p1 p2)
                     else manhatDist p1 p2
--Example: townshipDist (0,0,10) (1,1,20) == 2*townshipDist (0,0,10) (1,1,10) 

--Given a list of doubles, compute their average. You may need fromIntegral.
average :: [Double] -> Double
average lst = (sum lst)/(fromIntegral (length lst))
--Example:  average [0,5,10] = 5.0

--Given a ranking function and a list of elements, return the element with the minimum rank.
minimize :: Ord a => (a -> Double) -> [a] -> a
minimize rank lst = snd (minimum [(rank x, x) | x<-lst])
--Note: watch out for ord a part
--Example: minimize (fromIntegral . length) ["aaaa", "b"] = "b"

--Given a bucketing function, a list of buckets, and a list of items to be placed in buckets, 
--and returns the list of bucket, items-in-buckets pairs.
--Go take your old buildCorpus function, copy it, and turn it into a HOF (higher-order function).
-- Bucket is a HOF, because it takes a function as input. You can write it using recursion, other
-- HOFs, or list comprehensions as you choose.
bucket :: Eq b => (a -> b) -> [b] -> [a] -> [(b,[a])]
bucket func [] lst = []
bucket func (b:bs) lst = (b, [x | x<-lst, (func x)==b]):(bucket func bs lst)

--badBucket func bucks lst = [ (x,[y|y<-lst,(func y)==x]) | x<-bucks]
--Example:  bucket length [1..3] ["Hi","my","job","is","fun","!"]
--[(1,["!"]),(2,["Hi","my","is"]),(3,["job","fun"])]
--

--Full project!

--Given a metric, a list of centers, and a point, return the center closest to the point.
--Hint: you've already written a higher-order function to do this.
--type Cluster = (Center, [Point])
assignPoint :: (Point -> Center -> Double) -> [Center] -> Point -> Center
assignPoint metric cLst p = minimize (\c -> metric p c) cLst

--Examples: assignPoint eucDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (0.0,0.0,-1)
--          assignPoint trafficDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (5.0,7.0,-1)

--Given a metric, a list of centers, and a list of point, return the clusters, where each point is
--assigned to the nearest center.
--Hint: you've already written a higher-order function to do this.
--A cluster is a center and a list of points.
--type Point = (Double, Double, Int)
--type Center = Point
--type Cluster = (Center, [Point])
assignPoints :: (Point -> Center -> Double) -> [Center] -> [Point] -> [Cluster]
assignPoints func [] pLst = []
assignPoints metric cLst pLst = bucket (\p -> assignPoint metric cLst p) cLst pLst
--Examples
--testClusters = assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints 
--
--[(c, length ps) | (c,ps) <- testClusters]
--[((1.0,1.0,-1),1),((2.0,2.0,-1),9)]
--
--testClusters
--[((1.0,1.0,-1),[(1.0,7.0,700)]),
-- ((2.0,2.0,-1),[(7.0,1.0,100),(7.0,3.0,200),(8.0,1.0,300),(8.0,2.0,400),(7.5,3.5,500),
--                (2.0,7.0,600),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]

--Given a metric and a cluster, return the mean of the points in the cluster.
--The existing center is NOT a point, and should be ignored.
--The label should be the label of the closest point to the new center. 
--Since you can't take the mean of an empty list of points, return Nothing in that case.
findMean :: (Point -> Center -> Double) -> Cluster -> Maybe Center
findMean metric (c,[]) = Nothing
findMean metric (c,lst) = let xTotal = sum [ fst3 x | x<-lst]
                              yTotal = sum [ snd3 x | x<-lst]  
                              lngth = length lst
                              newX = (xTotal / (fromIntegral lngth))
                              newY = (yTotal / (fromIntegral lngth))
                              tmpLbl = trd3 $ minimize (\p -> metric p c) lst
                              closest = minimize (\p -> metric p (newX, newY, tmpLbl)) lst
                           in Just (newX, newY, trd3 closest)
--NOTE: Not sure if the label for the metric calc in closest is correct
--Example: findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)]) = Just (4.0,4.0,1)

--Given a metric and a list of clusters, return all the valid centers. If any cluster is empty,
--simply remove it.
moveCenters :: (Point -> Center -> Double) -> [Cluster] -> [Center]
moveCenters metric lst = [fromMaybe (-1,-1,-1) x | x<-maybes, x/=Nothing]
                    where maybes = [findMean metric x | x<-lst]

--Given a metric, k, and a list of clusters, first move the centers, and then reassign the points
--to the new centers.
--Note that clusters can become empty and disappear. For full credit, replace missing clusters as
--described on the website.
improveClusters :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
improveClusters metric k cLst = let newCenters = moveCenters metric cLst
                                    aux :: [Cluster] -> [Point]
                                    aux [] = []
                                    aux (c:cs) = (snd c)++(aux cs)
                                    allPoints = aux cLst
                                 in if (length newCenters) < k
                                    then let bigC = snd $ maximum [(length (snd x), x) | x<-cLst]
                                             newNewCenters = (splitCenter bigC):newCenters
                                          in assignPoints metric newNewCenters allPoints   
                                    else assignPoints metric newCenters allPoints   

splitCenter :: Cluster -> Center
splitCenter (c, (x:xs)) = if c /= x
                          then x
                          else splitCenter (c,xs)

--iterationLimit should be used by kMeans to limit how many times improveClusters can be called.
--Remember variables are not mutable.
iterationLimit = 100
--Given a metric, k, and a list of points, create the initial clusters and repeatedly 
--improve them, until they stop changing.

getClusters :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
getClusters metric k pLst= let centers = getKElems k pLst
                            in assignPoints metric centers pLst 

clusterize :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
clusterize metric 0 cLst = cLst
clusterize metric i cLst = let new = (improveClusters metric 0 cLst)
                            in if (checkChange new cLst) then cLst
                               else clusterize metric (i-1) new

checkChange :: [Cluster] -> [Cluster] -> Bool
checkChange old new = let oldCs = [fst x | x<-old]
                          newCs = [fst x | x<-new]
                          aux :: [Center] -> [Center] -> Bool
                          aux [] [] = True
                          aux (x:xs) (y:ys) = if (x /= y) then False
                                              else aux xs ys
                      in aux oldCs newCs

kMeans :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
kMeans metric k pLst = let start = getClusters metric k pLst
                        in clusterize metric iterationLimit start

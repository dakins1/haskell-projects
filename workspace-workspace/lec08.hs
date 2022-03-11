import Debug.Trace
{-
[7,3,1,9,10,5]

pivot = 7
smallers = [3,1,5]
largers = [9,10]
smallSorted = [1,3,5]
largSOrted [9,10]
[1,3,5,9,10]
-}

isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = (x <= head xs) && (isSorted xs)

quicksort lst = 
          if isSorted lst then lst
          else let pivot = head lst
                   smallers = [x | x <- tail lst , x < pivot ] 
                   largers = [x | x <- tail lst, x >= pivot ]
                   smallSorted = quicksort smallers
                   largSorted = quicksort largers
               in traceShow (pivot, smallers, largers) $ smallSorted ++ [pivot] ++ largSorted

hanoi 0 start end place = [] 
hanoi n start end place =
  let clearStart = hanoi (n-1) start place end 
      middleStep = "Move " ++ (show n) ++ " from " ++ start ++ " to " ++ end
      finishMoving = hanoi (n-1) place end start
  in clearStart ++ [middleStep] ++ finishMoving

main = putStr $ unlines $ hanoi 7 "A" "C" "B"

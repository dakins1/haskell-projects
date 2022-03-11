middle x y z = if (x>y) == (z>x) 
               then x
               else if (y-x) == (z-y)
                    then y
                    else z

isEmpty [] = True
isEmpty (x:xs) = False

absLst xs = [if x <= 0 then x*(-1) else x | x <- xs ]


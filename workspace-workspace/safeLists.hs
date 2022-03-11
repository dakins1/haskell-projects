mHead :: [a] -> Maybe a
mHead [] = Nothing
mHead (x:xs) = Just x

mTail :: [a] -> Maybe [a]
mTail [] = Nothing
mTail (x:xs) = Just xs

thirdElem :: [a] -> Maybe a
thirdElem lst =
  case mTail lst of
    Nothing -> Nothing
    Just tail -> case mTail tail of
                   Nothing -> Nothing
                   Just tail2 -> mHead tail2

thirdElemb lst = 
    do lst' <- mTail lst
       lst'' <- mTail lst'
       mHead lst''


twiceFifth :: [Int] -> Maybe Int
twiceFifth lst = mHead =<< mTail =<< mTail =<< mTail =<< mTail lst
twiceFith lst = 
    do tail <- mTail lst
       ttail <- mTail tail
       tttail <- mTail ttail
       ttttail <- mTail tttail
       h <- mHead ttttail
       return (2*h)

(=<) :: (a -> Maybe b) -> Maybe a -> Maybe b
(=<) f mv = 
    do v <- mv
       f v

twiceKth :: Int -> [Int] -> Maybe Int
twiceKth 0 lst =
    do h <- mHead lst
       return (2*h)
twiceKth k lst =
    do xs <- mTail lst
       twiceKth (k-1) xs

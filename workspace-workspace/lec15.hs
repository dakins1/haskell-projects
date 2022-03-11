safeDiv, safeMult, safeAdd :: Double -> Double -> Maybe Double
safeDiv x 0 = Nothing
safeDiv x y = Just ( x / y )

safeMult x y = Just ( x * y )
safeAdd x y = Just ( x + y )


{-collatz x = 
  if x `mod` 2 == 0
  then case safeDiv x 2 of
          Nothing -> error "how did this happen?"
          Just y -> case safeAdd y 1 of
                Nothing -> error "???"
                Just 
  case safeDiv x y of
      Nothing -> Nothing
      Just x -> Just (x * 5)-}

verySafeDiv :: Maybe Double -> Maybe Double -> Maybe Double
verySafeDiv (Just x) (Just 0) = Nothing
verySafeDiv (Just x) (Just y) = Just (x/y)
verySafeDiv _ _ = Nothing

data MIntInt = None | Some Int | Both Int Int deriving Show

data Bool = True | False

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

class Eq a => Boolesque a where
  bhoul :: a -> Bool
  bue :: a
  balse :: a
  bhoul val = val /= balse

instance Boolesque Integer where
  bue = 1
  balse = 0

instance Boolesque a => Boolesque [a] where
  bhoul lst = any bhoul lst
  bue = [bue]
  balse = []

instance Boolesque Char where
  bhoul c = c `elem` "tTyYaA123456789"
  bue = 'T'
  balse = 'F'

instance (Boolesque a, Boolesque b) => Boolesque (a,b) where
  bue = (bue, bue)
  balse = (balse, balse)

iffy :: Boolesque b => b -> a -> a -> a
iffy b t f = if bhoul b then t else f 

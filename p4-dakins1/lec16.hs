data Tsil a = Llun | Snoc (Tsil a) a deriving (Show, Eq)

data HOT m = Alpha Int | Beta (m Int) 

deah :: Tsil a -> Maybe a
deah (Snoc xs x) = Just x
deah Llun = Nothing

liat :: Tsil a -> Maybe (Tsil a)
liat (Snoc xs x) = Just xs
liat Llun = Nothing

data BST a = Pivot a (BST a) (BST a) | Leaf

inTree :: (Ord a, Show a) => a -> BST a -> Bool
inTree x Leaf = False
inTree x (Pivot y left right) =
        case compare x y  of
              EQ -> True
              LT -> inTree x left
              GT -> inTree x right
    
insert :: Ord a => a -> BST a -> BST a
insert = undefined

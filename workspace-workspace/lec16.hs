import Data.List (sort)
data Tsil a = Llun | Snoc (Tsil a) a deriving (Show, Eq)

data HOT m = Alpha Int | Beta (m Int) 

deah :: Tsil a -> Maybe a
deah (Snoc xs x) = Just x
deah Llun = Nothing

liat :: Tsil a -> Maybe (Tsil a)
liat (Snoc xs x) = Just xs
liat Llun = Nothing

data BST = Pivot Int (BST ) (BST ) | Leaf deriving Show

logequal :: BST -> BST -> Bool
logequal treeA treeB = (fromTree treeA) == (fromTree treeB)
{-logequal Leaf Leaf = True
logequal Leaf (Pivot y ly ry) = False
logequal (Pivot x l r) tree = (x `inTree` tree) && (l `logequal` tree) && (r `logequal` tree)-}


insert :: Int -> BST -> BST
insert x Leaf = Pivot x Leaf Leaf
insert x tree@(Pivot p left right) =
        case compare x p  of
              EQ -> tree
              LT -> Pivot p (insert x left) right
              GT -> Pivot p left (insert x right)

inTree :: Int -> BST -> Bool
inTree x Leaf = False
inTree x (Pivot y left right) =
        case compare x y  of
              EQ -> True
              LT -> inTree x left
              GT -> inTree x right
 
toTree ::  [Int] -> BST 
toTree lst = foldr insert Leaf lst

fromTree :: BST -> [Int]
fromTree Leaf = []
fromTree (Pivot x l r) = (fromTree l) ++ [x] ++ (fromTree r)


instance Eq BST where
  treeA == treeB = (fromTree treeA) == (fromTree treeB)
    where fromTree Leaf = []
          fromTree (Pivot x l r) = (fromTree l) ++ [x] ++ (fromTree r)

data Cluster = Cluster String [String]

instance Eq Cluster where
  (Cluster ca pa) == (Cluster cb pb) = ca == cb && (sort pa) == (sort pb)

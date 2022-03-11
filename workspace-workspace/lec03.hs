aNum = 5
aFloat = 7.2
z = aNum + aFloat
aString = "Hello"
aList = [7,3,5]

anotherList :: [String]
anotherList = ["Hello", "World"]

aThirdList = [[5,9],[],[7]]
aTuple = ("Jacob", 19)
anotherTuple = (7,3,12)
aLOT = [(a,b) | a <- [1..], b <- [1..]]
extraFunList = reverse [1..]
naturals = [1..]

end = []
lst4 = aNum:end

removeUpper str = [x | x <- str, not (x `elem` ['A'..'Z'])]
addThree a b c = a + b + c

addPair :: (Integer, Integer) -> Integer
addPair (a,b) = a + b

assocs :: [(String, [Integer])]
assocs = [("Jane", [73,52,81]), ("William", [82, 21, 100]), ("Davide", [100, 100, 0])]

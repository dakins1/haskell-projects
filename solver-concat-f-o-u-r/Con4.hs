module Con4 where
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char

data Player = Player1 | Player2 deriving (Eq, Show)
data Slot = Empty | Occ Player deriving (Eq, Show)
data Outcome = Draw | Win Player deriving (Eq, Show)
type Move = Int
type Column = [Slot]
type Board = [Column]
type GameState = (Board, Player) --where player represents whose move it currently is

showGame :: GameState -> IO ()
showGame (brd, player) = putStr (show player ++ "'s turn\n" ++ showBoard brd)

showBoard :: Board -> String
showBoard brd = let properOrder = transpose [ reverse x | x <- brd ]
                in concat [ "|" ++ concat (map prettyShowSlot x) ++ "\n" | x <- properOrder ]

prettyShowSlot :: Slot -> String 
prettyShowSlot (Empty) = "_|"
prettyShowSlot (Occ x) = prettyShowPlayer x

prettyShowPlayer :: Player -> String
prettyShowPlayer (Player1) = "x|"
prettyShowPlayer (Player2) = "o|"

emptyBoard :: Int -> Int -> Board
emptyBoard y x = replicate x (replicate y Empty)

--All valid moves for a gamestate.
moves :: GameState -> [Move]
moves game@(brd, plr) = [x | x <- [0..(length brd) - 1], (makeMove x game) /= Nothing]

--Checks if a move is valid
legalMove :: GameState -> Move -> Bool
legalMove (b,cp) c = case getI c b of
                        Nothing -> False
                        Just col -> (hasEmptySlot col)

hasEmptySlot :: Column -> Bool
hasEmptySlot col = Empty `elem` col

--A safer form of indexing.
-- | Safe version of '!!'
getI :: Int -> [a] -> Maybe a
getI _ [] = Nothing
getI 0 (a:_) = Just a
getI i (_:as) = getI (i-1) as

-- Standard Library Function in Data.List.Tools
setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

makeMove :: Move -> GameState -> Maybe GameState
makeMove position (board,player) = let column = getI position board
                                    in case column of Nothing -> Nothing
                                                      Just c -> let moveHeight = getMoveHeight c
                                                                 in if last c /= Empty then Nothing
                                                                    else Just ((setAt board position (setAt c moveHeight (Occ player)), (otherPlayer player)))

-- a take until would work faster than a filter.
getMoveHeight :: [Slot] -> Int
getMoveHeight col = length (filter (/=Empty) col)

getColumn :: Move -> [[Slot]] -> [Slot]
getColumn x [] = error ("Index out of bounds: " ++ show x)
getColumn 0 (slot:board) = slot
getColumn x (slot:board) = getColumn (x-1) board


otherPlayer :: Player -> Player
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

--Check Winner

checkWinner :: GameState -> Maybe Outcome
checkWinner (bored, _) = let vertWin = checkVerticalWinner bored 4
                             horizWin = checkHorizontal bored 4
                             leftDiagWin = checkLeftDiag bored 4
                             rightDiagWin = checkRightDiag bored 4
                             lstWinners = [vertWin, horizWin, leftDiagWin, rightDiagWin]

                             isWinner = onlyJust lstWinners
                    in if not (null isWinner) then Just $ head isWinner
                       else if isDraw bored then Just Draw
                       else Nothing

-- checkColumnWinner :: Column -> Int -> Maybe Outcome
-- checkColumnWinner col req = let
--     aux :: Column -> (Int, Player) -> Maybe Outcome
--     aux [] (count,player) = if (count == req) then (Just (Win player)) else Nothing
--     aux (s:ss) (count,player) = if (count == req) 
--                                 then (Just (Win player))
--                                 else case s of
--                                        Occ p -> if (p == player) then aux ss (count + 1, p)
--                                                 else aux ss (0, otherPlayer p)
--                                        Empty -> Nothing
--     in aux col (0, Player1)


checkColumnWinner :: Column -> Int -> Maybe Outcome
checkColumnWinner (x:xs) req = let (plr, score) = aux xs x 1 1 x Empty 
                               in if score >= req 
                                      then case plr of Occ x -> Just $ Win x
                                                       Empty -> Nothing
                                      else Nothing
                               where aux :: Eq a => [a] -> a -> Int -> Int -> a -> a-> (a, Int)
                                     aux [] _ _ allTimeLongest allTimeLongestElement _ = (allTimeLongestElement, allTimeLongest)
                                     aux (x:xs) ele currentLongest allTimeLongest allTimeLongestElement ignore = 
                                      if x == ignore then aux xs ele 1 allTimeLongest allTimeLongestElement ignore 
                                      else if x == ele 
                                      then aux xs x (currentLongest + 1) newAllTimeLongest newAllTimeLongestElement ignore
                                      else aux xs x 1 allTimeLongest newAllTimeLongestElement ignore
                                      where newAllTimeLongest = if currentLongest + 1 > allTimeLongest then currentLongest + 1 else allTimeLongest
                                            newAllTimeLongestElement = if currentLongest + 1 > allTimeLongest then ele else allTimeLongestElement
isDraw :: Board -> Bool
isDraw bored = not (or [any ( == Empty) c | c <- bored])

checkVerticalWinner :: Board -> Int -> Maybe Outcome
checkVerticalWinner [] req = Nothing
checkVerticalWinner (b:bs) req = let win = checkColumnWinner b req
                                  in if (win /= Nothing) then win
                                     else checkVerticalWinner bs req

checkHorizontal :: Board -> Int -> Maybe Outcome
checkHorizontal board req = checkVerticalWinner (transpose board) req

checkLeftDiag :: Board -> Int -> Maybe Outcome
checkLeftDiag board req = checkHorizontal (rearrange shiftWithEmpty board) req

checkRightDiag :: Board -> Int -> Maybe Outcome
checkRightDiag board req = checkHorizontal (rearrange revShift board) req

rearrange :: ([a] -> Int -> [a]) -> [[a]] -> [[a]]
                     --aux :: [[a]] -> Int -> [[a]]
rearrange sp lst = let aux [] count = []
                       aux (c:cs) count = (sp c count):(aux cs (count+1))
                    in aux lst 0

shiftWithEmpty :: [Slot] -> Int -> [Slot]
shiftWithEmpty lst 0 = lst
shiftWithEmpty lst k = (replicate k Empty) ++ lst

revShift :: [Slot] -> Int -> [Slot]
revShift lst k = shiftWithEmpty (reverse lst) k

rotate :: [a] -> Int -> [a]
rotate [] k = []
rotate lst 0 = lst
rotate lst k = restOfLst ++ fstK
               where howManyOver = k `mod` (length lst)
                     fstK = take howManyOver lst
                     restOfLst = drop howManyOver lst


--Test boards for checkWinner and findWinner

onlyJust :: [Maybe a] -> [a]
onlyJust lst = [x | Just x <- lst]

--Test boards for checkWinner


testCol = [[Occ Player1, Occ Player1, Occ Player2, Occ Player1, Occ Player1, Occ Player1]]

testWhor = [[Occ Player1],[Occ Player1],[Occ Player1],[Occ Player1]]

testDiag = [[Occ Player1,Empty,Empty,Empty],[Empty,Occ Player1,Empty,Empty],[Empty,Empty,Occ Player1,Empty],[Empty,Empty,Empty,Occ Player1]]

testEdgeCase = [[Empty, Empty, Occ Player1], [Occ Player1, Empty, Empty], [Empty, Occ Player1, Empty]]

testGame = (testDiag, Player1) --a test gamestate


predictionBoard = [[Occ Player1, Empty, Empty, Empty], [Occ Player1, Occ Player1, Occ Player2, Empty], [Occ Player2, Occ Player1, Occ Player2, Empty], [Occ Player2, Occ Player1, Occ Player2, Empty]]

drawBoard = ([[Occ Player1], [Empty]], Player1)

bigDrawBoard = ([[Occ Player2, Occ Player2, Empty, Empty], [Occ Player2, Occ Player2, Empty, Empty], [Occ Player2, Occ Player2, Empty, Empty]], Player2)

predictionBoard2 = [[Occ Player1, Occ Player1, Empty, Empty], [Occ Player2, Occ Player2, Empty, Empty], [Occ Player2, Occ Player1, Occ Player1, Empty], [Occ Player1, Occ Player2, Occ Player2, Occ Player1], [Occ Player1, Empty, Empty, Empty], [Occ Player2, Occ Player2, Empty, Empty], [Occ Player1, Empty, Empty, Empty]]

predictionBoard2Won = [[Occ Player1, Occ Player1, Empty, Empty], [Occ Player2, Occ Player2, Empty, Empty], [Occ Player2, Occ Player1, Occ Player1, Empty], [Occ Player1, Occ Player2, Occ Player2, Occ Player1], [Occ Player1, Occ Player2, Empty, Empty], [Occ Player2, Occ Player2, Empty, Empty], [Occ Player1, Empty, Empty, Empty]]

predGame3 = ([[Occ Player1, Occ Player1, Empty, Empty], [Occ Player2, Occ Player2, Empty, Empty]], Player1)

testGame2 = (map (reverse) $ transpose [[Empty, Occ Player1, Empty, Empty], 
              [Occ Player2, Occ Player2, Occ Player2, Occ Player2],
              [Occ Player2, Occ Player1, Occ Player1, Occ Player1],
              [Occ Player1, Occ Player1, Occ Player2, Occ Player2]], Player1)

testPrediction = (predictionBoard, Player1)

testPrediction2 = (predictionBoard2, Player2)

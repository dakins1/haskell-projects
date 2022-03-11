module Solver where
import Con4
import Data.List
import Data.Maybe

validStates :: GameState -> [GameState]
validStates state = catMaybes [ makeMove x state | x <- moves state ]

findWinner :: GameState -> Outcome
findWinner state@(brd, plr) = 
    let valids = validStates state
        outcomes = map findWinner valids
        check = checkWinner state
    in case check of 
        Just o -> o
        Nothing -> if any ( == (Win plr)) outcomes
                   then (Win plr)
                   else if all ( == (Win (otherPlayer (plr)))) outcomes
                        then (Win (otherPlayer plr) )
                        else Draw

findWinnerDepth :: GameState -> Int -> Maybe Outcome
findWinnerDepth _ 0 = Nothing
findWinnerDepth state@(brd, plr) depth = 
    let valids = validStates state
        outcomes = map (\x -> findWinnerDepth x (depth - 1)) valids 
        check = checkWinner state
    in if check /= Nothing
        then check
        else if any ( == Just (Win plr)) outcomes
                then Just (Win plr)
                else if all ( == Just (Win (otherPlayer (plr)))) outcomes
                        then Just (Win (otherPlayer plr) )
                        else Just Draw

bestMove :: GameState -> Maybe Move
bestMove state@(brd, plr) = 
    let check = checkWinner state
    in if check /= Nothing then Nothing --if someone has already won the game, then nothing, else all that
        else let validMoves = moves state
                 madeStates = [ (makeMove x state, x) | x <- validMoves ]
                 safeMakeStates = [(s,m) | (Just s, m) <- madeStates ]
                 found = [ (findWinner s, m) | (s, m) <- safeMakeStates ]
                 wins = [ (w, m) | (w, m) <- found, w == Just (Win plr)]
                 draws = [ (w, m) | (w, m) <- found, w == Just (Draw) ]
                 ls = [ (w, m) | (w, m) <- found, w == Just (Win (otherPlayer plr)) ]
             in if not (null wins) then Just (snd (head wins))
                else if not (null draws) then Just (snd (head draws))
                    else Just (snd (head ls))

contigSeq :: Eq a => [a] -> [[a]]
contigSeq [] = []
contigSeq  (x:xs) =
    let aux :: Eq a => [a] -> [a] -> [[a]]
        aux [] lst  = [lst]
        aux (h:t) (y:ys) =
            if h == y then (aux t (h:y:ys))
            else (y:ys):(aux t [h])
    in aux xs [x]

rateSeq :: [[Slot]] -> Int
rateSeq [] = 0
rateSeq (x@(y:ys):xs) =
    case y of Occ Player1 -> (length x)^2 + rateSeq xs
              Occ Player2 -> -((length x)^2 + rateSeq xs)
              Empty   -> rateSeq xs

rateGame :: GameState -> Int
rateGame (brd, _) = 
    let ratedCols = map (rateSeq . contigSeq) brd
        ratedRows = map (rateSeq . contigSeq) (transpose brd)
        ratedLeftDiag = map (rateSeq . contigSeq) (rearrange shiftWithEmpty brd)
        ratedRightDiag = map (rateSeq . contigSeq) (rearrange revShift brd)
    in (sum ratedCols) + (sum ratedRows) + (sum ratedLeftDiag) + (sum ratedRightDiag)

rateMove :: Move -> GameState -> Maybe Int
rateMove mv state@(brd, plr) = 
    let madeState = makeMove mv state
    in case madeState of Nothing -> Nothing
                         Just ms -> Just (max (-100000) (min 100000 (rateGame ms)))

threadWinner :: [Maybe Outcome] -> Maybe Outcome
threadWinner [] = Nothing
threadWinner (o:os) = if o /= Nothing then o else threadWinner os

boundedDepth :: Int -> GameState -> Int
boundedDepth depth state = aux depth state
    where aux :: Int -> GameState -> Int
          aux 0 state = rateGame state
          aux depth state = 
              let valids = validStates state
                  outcomes = map (aux (depth - 1)) valids
              in case checkWinner state of 
                    Just (Win Player1) -> 100000
                    Just (Win Player2) -> -100000
                    Just Draw -> 0
                    Nothing -> if (fst state) == Player1 then max outcomes --best rating for the current player
                               else min outcomes 

goodMove :: Int -> GameState -> Maybe Move
goodMove depth state@(brd, plr) = 
    let mvs = moves state
        results = [ (makeMove x state, x) | x <- mvs ]
        rated = [ (boundedDepth (depth - 1) x, y) | (Just x, y) <- results ]
    in if null rated then Nothing else Just $ aux rated (head rated)
    where aux :: [(Int, Move)] -> (Int, Move) -> Move
          aux [] current = snd current
          aux (x@(rating, mv):xs) current@(cRating, _) = case plr of Player1 -> if rating >= cRating then aux xs x else aux xs current
                                                                     Player2 -> if rating <= cRating then aux xs x else aux xs current

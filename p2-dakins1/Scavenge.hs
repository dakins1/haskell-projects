module Scavenge where
import Dictionaries
import Data.List (sort)
import Debug.Trace
import Data.Char

--                                          Type Aliases
-- These type aliases help to abstract our code. 
-- 
type Hand = [Char]
type Move = String
type Play = [Move]
type Dictionary = [String] 

-- You don't need to use buildDict currently. However, if you change the type of Dictionary
-- to be more efficient than a straight list, you must change buildDict accordingly.

buildDict :: [String] -> Dictionary
buildDict dct = dct

-- All undefined values and functions should be completed. Your code will compile and test 
-- (with the --test flag) even if some functions are left undefined.
--
--                                       Milestone
--

-- Score takes a move and returns the sum of the letter values, according to the Scrabble scoring
-- system: https://scrabble.hasbro.com/en-us/faq
-- A helper function may be useful.
scores :: [(Integer, [Char])]
scores = [(1,['A','E','I','O','U','L','N','S','T','R']),
          (2,['D','G']),
          (3,['B','C','M','P']),
          (4,['F','H','V','W','Y']),
          (5,['K']),
          (8,['J','X']),
          (10,['Q','Z'])]
points :: [Int]
points = [1,2,3,4,5,8,10]

letters :: [Char]
letters = ['A','B','C','D','E','F','G','H','I','J','K','L','M',
           'N','O','P','Q','R','S','T','U','V','W','X','Y','Z']

scoreLookup :: Char -> Integer
scoreLookup letter = let results =  [fst x|x<-scores, (toUpper letter) `elem` (snd x)]
                      in if results==[] then error "Cannot score empty list"
                         else head results
score :: Move -> Integer
score word = sum [ scoreLookup x | x<-word]
-- score "QA" == 11
-- score "JF" == 12

-- scorePlay takes a play and returns the total score of all words.
scorePlay :: Play -> Integer
scorePlay play = sum [score x | x<-play]
-- scorePlay ["KB", "QA"] == 19 

-- remove takes an element and a list, and returns the list with one copy of that element removed.
-- You should not assume the list is sorted. If there are multiple copies of the element in the list,
-- only remove one of them. If the element doesn't occur, you should throw an error.

remove :: Eq a => a -> [a] -> [a]   
remove take [] = error "Cannot remove an element that is not in the string!"
remove take (x:xs) = if (take == x) 
                     then xs
                     else x:(remove take xs)
-- remove 7 [7,3,1,7,5] = [3,1,7,5] 
-- The order here doesn't matter, if you remove the second 7 it is okay.

-- updateHand should take a hand (a list of characters), and a move (a string), and return the hand
-- that remains after that move is played.
updateHand :: Hand -> Move -> Hand
updateHand hand [] = hand
updateHand hand [x] = remove x hand
updateHand hand (x:xs) = let newHand = remove x hand
                             in updateHand newHand xs
                       
-- updateHand "HELLO" "LO" = "HEL"

-- canMake takes a hand and a move, and tells you if that move can be made with that hand. Be sure to
-- consider the possibility that a letter may occur more times in the move than it does in the hand.

canMake :: Hand -> Move -> Bool

canMake hand move = let --the logic of this reminds me of a pushdown automata
                        aux [] [] = True
                        aux [] mv = False
                        aux hnd [] = True
                        aux (h:hs) (m:ms) = 
                            if (h == m) then aux hs ms
                            else aux hs (m:ms)
                      in aux (sort hand) (sort move)

-- "DNAHTSET" `canMake` "HAND" = True 
-- "DNAHTSET" `canMake` "HAAND" = False
-- For full credit, this must run in near-linear time (n log n is sufficient)

-- isValidMove tests if a move is valid with respect to a dictionary and hand: 
-- the move must be a word in the dictionary and a move that can be made with the hand.
isValidMove :: Dictionary -> Hand -> Move -> Bool
isValidMove dic hand move = (hand `canMake` move) && (move `elem` dic)
-- isValidMove tinyDict "MKEKIOCHAUX" "MAKE" = TRUE
-- isValidMove tinyDict "MKEKIOCHAUX" "MAXKE" = FALSE
-- isValidMove tinyDict "MKEKIOCHAUX" "FAKE" = FALSE

-- isValidPlay checks if a play is valid. Each move in the play must be a word in the dictionary, and
-- must be playable using whatever remains of the hand after all previous moves have been made.
isValidPlay :: Dictionary -> Hand -> Play -> Bool
isValidPlay dic [] [] = True 
isValidPlay dic [x] [] = True 
isValidPlay dic [] [x] = False
isValidPlay dic (x:xs) [] = True 
isValidPlay dic [] (x:xs) = False
isValidPlay dic hand (x:xs) = if (isValidMove dic hand x)
                              then isValidPlay dic (hand `updateHand` x) xs
                              else False
-- isValidPlay tinyDict "TMAKE" ["TAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["MAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["TAKE","MAKE"] = False
    
-- validMoves: takes a dictionary and a hand, and returns all words that can be
-- created by letters in the hand. Order does not matter.
validMoves :: Dictionary -> Hand -> [Move]
validMoves dic hand = [x|x<-dic, isValidMove dic hand x]
-- validMoves shortDict "PEMDOVZIJM" = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]

-- maximumPlay takes a list of plays and returns the highest scoring play.
maximumPlay :: [Play] -> Play
maximumPlay lst = if (length lst == 0) then error "cannot do maxPlay on empty list" 
                  else
                      let maxPlay = maximum [scorePlay x|x<-lst]
                      in head [x|x<-lst, (scorePlay x) == maxPlay]

--maximumPlay [["HI","THERE"], ["HI"], ["QUASAR"]] = ["QUASAR"]


--                                  End of Milestone!

--                                  Final Project 

-- --- Brute Force Algorithms
-- You are going to search for the best play, instead of just choosing the best move one at a time.
-- To do so, you will consider every possible play, by considering every possible combination of
-- words. You will implement two variants. 
 
-- powerset: return the powerset of the input, i.e. the list of all sub-lists.
-- You may assume the list has no duplicates. 
-- The output should not have duplicates, up to sorting.
set = [1,2]
set3 = [1,2,3]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset [x] = [[x],[]]
powerset (x:xs) = let rest = powerset xs
                  in  rest++[x:y | y<-rest]

-- powerset [1,2] = [[],[1],[1,2],[2]]
-- It is acceptable to have [2,1] INSTEAD of [1,2], but not in addition.
-- length (powerset "abcde") = 32

-- The Naive Brute Force (naiveBrutePlay) takes every combination of moves in
-- the dictionary: .the powerset of the dictionary. It will only work with very small
-- dictionaries, like tenWords.  You will then choose the best valid play out of that list.
testDic = ["FEE", "FI", "FOE", "FUM", "FIZ"]
testHand = "FEEIOEUMIZ"
-- <bestPlayFunction> testDic testHand = ["FIZ"]

naiveBrutePlay :: Dictionary -> Hand -> Play
naiveBrutePlay dic hand = let plays = powerset dic
                              validPlays = [play|play<-plays, (isValidPlay dic hand play)]
                           in maximumPlay validPlays

-- The Smart Brute Force approach realizes that we can restrict the size of the dictionary
-- before we compute the powerset. There are probably MANY moves in the dictionary that we can't
-- create at all! So, first find all the moves that can be made with this hand. Then, take the
-- powerset of that restricted list to create a list of all plays made up of those moves. Then
-- find the best valid play from that list.
smartBrutePlay :: Dictionary -> Hand -> Play
smartBrutePlay dic hand = let moves = validMoves dic hand
                              plays = powerset moves
                              validPlays = [play|play<-plays, (isValidPlay dic hand play)]
                           in maximumPlay validPlays

-- --- Greedy Algorithm

-- maximumMove: takes a list of moves, and returns the one with the highest
-- score. Return an error for the empty list.
maximumMove:: [Move] -> Move
maximumMove [] = error "No such thing as maximumMove on empty list"
maximumMove lst = let maxScore = maximum [score x | x<- lst]
                   in head [x | x<-lst, (score x) == maxScore]

--maximumMove ["OR", "OK"]  = "OK"


-- greedyPlay: choose the best move you can at any given point in time, then check to see what
-- other moves you can make.
greedyPlay :: Dictionary -> Hand -> Play
greedyPlay dic hand = if (validMoves dic hand) == []
                      then []
                      else          
                          maxMove:(greedyPlay dic leftOvers)
                          where maxMove = maximumMove (validMoves dic hand)
                                leftOvers = updateHand hand maxMove
                          

-- greedyPlay shortDict "CLOSEFLOOR" = ["FORCE", "SO"] 


-- --- Recursive Game-Tree Algorithm

-- Finally we will try a recursive strategy to find the best play. Even the smart brute force
-- fails for large hands: I can make a lot of moves, but not a lot of moves simultaniously. Thus
-- the list of all possible plays is large, but the list of all valid plays is still likely
-- small. 
-- For this algorithm, start with the list of valid moves. Then, for each move find the
-- best play for the remaining hand. Select the hand that leads to the best overall score, counting both
-- the score of the move and the score of the best remaining play.
recBestPlay :: Dictionary -> Hand -> Play 
recBestPlay dic [] = []
recBestPlay dic hand = let
                            valMoves = validMoves dic hand
                            recurser :: Hand -> Play-> Dictionary -> Play
                            recurser [] play dic = play
                            recurser hand play dict = 
                                let moves = validMoves dict hand
                                    possibPlays = [recurser (updateHand hand x) (x:play) dict | x<-moves]
                                    maxTup = maximum [ (scorePlay x,x) | x<-possibPlays]
                                    newPlay = (snd maxTup)
                                    in if moves == [] then play else newPlay
                        in recurser hand [] valMoves 

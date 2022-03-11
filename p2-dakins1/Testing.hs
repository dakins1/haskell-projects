module Testing where
import Data.Time
import Debug.Trace
import Control.DeepSeq
import Data.Maybe
import System.CPUTime
import Control.Monad
import Control.Exception
import Data.List
import Coloring
import Data.Either
import Scavenge
import Dictionaries

type TestCase = IO (Either String String)

assertTrue :: Bool -> TestCase
assertTrue b =  do
    ret <- (try $ evaluate b :: IO (Either SomeException Bool))
    return $ case ret of
        Left ex -> Left $ "Failed with exception: " ++ (head $ lines $ show ex)
        Right False -> Left "Failed, but didn't crash!"
        Right True -> Right "*" 

assertEqual :: Eq a => a -> a -> (a -> String) -> TestCase
assertEqual a c s =  do
    ret <- (try $ evaluate (a == c) :: IO (Either SomeException Bool))
    return $ case ret of
        Left ex -> Left $ "Failed with exception: " ++ (head $ lines $ show ex)
        Right False -> do Left $ intercalate "\n" ["Failed, but didn't crash!", "\t\tShould have been:" ++ (s c), "\t\tBut got:" ++ (s a)]
        Right True -> do Right "*" 

assertError :: a -> String -> TestCase
assertError exp errMsg = do
    ret <- (try $ evaluate exp )
    return $ case ret of
        Left ex -> let exStr = show (ex :: SomeException)
                   in if "Prelude" `isInfixOf` exStr || "Non-exhaustive patterns" `isInfixOf` exStr
                      then Left $ "Failed, returning the built-in exception: " ++ (head $ lines exStr)
                      else Right $ "\n\tProbably passed, error message: " ++ (head $ lines exStr )
        Right b -> Left $ "Failed, reason: " ++ errMsg

printResult :: Bool -> (Either String String)-> IO Bool
printResult _ (Left err) = putRed ("\n\t* "++ err) >> return False
printResult False (Right pass) = putGreen ("\n\t"++pass) >> return True
printResult True (Right pass) = putGreen pass >> return True

printTests :: Bool -> String -> [TestCase] -> [TestCase] -> IO Bool
printTests quiet label tests bonus = do
    if quiet then do
            results <- sequence tests
            let pass = all isRight results
            unless pass $ 
                do putStr (label++": ")
                   foldM_ printResult True $ filter isLeft results
            opts <- sequence bonus
            let passO = all isRight opts
            unless passO $ 
                do putStrLn (label++" non-critical:")
                   foldM_ printResult True $ filter isLeft opts
            return pass
        else do
            putStr (label++": ")
            results <- sequence tests
            foldM_ printResult True results
            putStrLn ""
            unless (null bonus) $
              do putStr "\tNon-critical tests (for full credit, no Prelude errors):"
                 opts <- sequence bonus
                 foldM_ printResult True opts 
                 putStrLn ""
            return $ all isRight results

getRunTime :: Eq a => a -> IO NominalDiffTime
getRunTime expr = do
    start <- getCurrentTime
    let res = expr == expr
    unless res $ putStrLn "The world has ended. Please alert Dr. Fogarty." 
    stop <- getCurrentTime
    let diff = diffUTCTime stop start
    when (diff < 0) $ putStr "The world has ended. Please alert Dr. Fogarty." 
    return $ diff

assertTiming :: Eq a => a -> NominalDiffTime -> (String, String) -> IO (Either String String)
assertTiming expr timeout (fast, slow) = do
    runtime <- getRunTime expr
    if runtime > timeout
    then return $ Left $ unwords ["Failed, you are probably runing in",slow,"time instead of",fast,"time:", show runtime]
    else return $ Right $ unwords ["Passed! Your solution is probably running in",fast,"time", show runtime]


findBlowup :: (Eq a, Eq b) => (Int -> a) -> (a -> b) -> Int -> (Int -> Int) -> (Int -> Int) -> NominalDiffTime -> IO Double
findBlowup gen f n harden step threshold = aux n
    where aux :: Int -> IO Double
          aux n = do
            let caseN = gen n
            let caseSN = gen (step n)
            when (caseN /= caseN || caseSN /= caseSN) $ putStrLn "The world has ended. Please alert Dr. Fogarty." 
            runtimeN <- getRunTime (f caseN)
            if (runtimeN < threshold)
            then aux $ if (runtimeN * 20 < threshold) 
                       then (harden (harden n)) 
                       else (harden n)
            else do
              --putStrLn (show n)
              runtimeSN <- getRunTime (f caseSN)
              return ((realToFrac runtimeSN) / (realToFrac runtimeN)) 

assertLinear :: (Eq a, Eq b) => (Int -> a) -> (a -> b) -> IO (Either String String)
assertLinear gen f = 
    do blowup <- findBlowup gen f 2 (2*) (2*) (0.2)
       if (blowup > 2.5)
       then return $ Left $ unwords 
               ["Failed! Your solution is probably roughly quadratic. Doubling input size increased the runtime by a factor of ", take 3 $ show blowup]
       else return $ Right $ unwords 
               ["Passed! Your solution is probably linear or n log(n). Doubling input size increased the runtime by a factor of", take 3 $ show blowup]


assertPoly :: (Eq a, Eq b) => (Int -> a) -> (a -> b) -> Int -> IO (Either String String)
assertPoly gen f start = 
    do blowup <- findBlowup gen f start (1+) (1+) (0.2)
       if (blowup > 1.5)
       then return $ Left $ unwords 
                ["Failed! Your solution is probably exponential. Increasing input by 1 increased the runtime by a factor of ", take 3 $ show blowup]
       else return $ Right $ unwords 
                ["Passed! Your solution is probably polynomial. Increasing input by 1 increased the runtime by a factor of", take 3 $ show blowup]

assertNonPoly :: (Eq a, Eq b) => (Int -> a) -> (a -> b) -> Int -> IO (Either String String)
assertNonPoly gen f start = 
    do blowup <- findBlowup gen f start (1+) (1+) (0.2)
       if (blowup > 1.5)
       then return $ Right $ unwords 
                ["Passed! Your solution is probably exhaustive. Increasing input by 1 increased the runtime by a factor of ", take 3 $ show blowup]
       else return $ Left $ unwords 
                ["Failed! Your solution is probably not exhaustive. Increasing input by 1 increased the runtime by a factor of", take 3 $ show blowup]

testScore :: Bool -> IO Bool
testScore q = printTests q "Testing score" 
          [ assertEqual (score "KB") 8 show
          , assertEqual (score "") 0 show
          , assertEqual (score "QA") 11 show
          , assertEqual (score "JF") 12 show
          ][assertError (score "K9X") "You should throw an error when for a non-letter character."]

testScorePlay :: Bool -> IO Bool
testScorePlay q = 
    printTests q "Testing scorePlay"
        [ assertEqual (scorePlay ["KB", "QA"]) 19 show
        , assertEqual (scorePlay []) 0 show
        ] []

             
testRemove :: Bool ->  IO Bool 
testRemove q =
    let lst3 = ["one", "two", "three"]
    in printTests q "Testing remove"
          [ assertEqual (remove 'a' ['a'..'b']) ['b'] show
          , assertEqual (remove "three" lst3) (init lst3) show
          , assertEqual (sort $ remove 1 [5,3,1,7,3,1,9,10,1]) [1,1,3,3,5,7,9,10] show
          , assertEqual (sort $ remove 'a' "abcra") ("abcr") show
          ][assertError (length $ remove 11 [1..10]) "You should throw an error when the element isn't in the list!"
          , assertError (length $ remove 11 []) "You should throw an error when the element isn't in the list!"
          ]

testCanMake :: Bool -> IO Bool
testCanMake q = 
    let hand1 = "TESTHAND"
        hand2 = "DNAHTSET"
        hand3 = "AAABBBCCC"
    in printTests q "Testing canMake"
                [ assertTrue (hand1 `canMake` "" && hand1 `canMake` "TEST")
                , assertTrue (hand2 `canMake` "TEST" && hand2 `canMake` "HAND")
                , assertTrue (hand3 `canMake` "ABCABCABC")
                , assertTrue (not $ hand1 `canMake` "HAAND" || "" `canMake` "EMPTY")
                ][]


testIsValidMove :: Bool -> IO Bool
testIsValidMove q =
    let tf = isValidMove tinyDict
    in printTests q "Testing isValidMove"
      [ assertTrue (tf "MKEKIOCHAOX" "MAKE")
      , assertTrue (tf "MKEKIOCHAOX" "I")
      , assertTrue (not $ tf "MKKIOCHAOX" "MAKE")
      , assertTrue (not $ tf "MKKIOCHAOX" "MAKI")
      ] []

testUpdateHand :: Bool -> IO Bool
testUpdateHand q = 
    let hand1 = "ABCDEFG"
        hand2 = "AAABBBCCC"
    in printTests q "Testing updateHand"
      [ assertEqual (sort $ updateHand hand1 "ADG") ("BCEF") show
      , assertEqual (sort $ updateHand hand2 "CCBBAA") ("ABC") show
      , assertTrue (not $ null $ sort $ updateHand hand2 "ABC")
      ][assertError (sort $ updateHand hand1 "FAIL") "You should throw an error when the hand cannot make the move!"
      ]

testIsValidPlay :: Bool -> IO Bool 
testIsValidPlay q =
   let hand = "THEQUICKBROWN"
       play1 = ["THE", "QUICK", "BROWN"]
       play2 = ["THE", "THE", "QUICK", "QUICK", "BROWN", "BROWN"]
       play3 = ["THEQUICKBROWN"]
   in printTests q "Testing isValidPlay"
     [ assertTrue (isValidPlay standardDict hand play1)
     , assertTrue (not $ isValidPlay standardDict hand play2)
     , assertTrue (not $ isValidPlay standardDict hand play3)
     ][]

testValidMoves :: Bool -> IO Bool
testValidMoves q = 
    let hand1 = "PEMDOVZIJM"
        hand2 = "EIFYOZWFKC"
        hand3 = "AAJNURWLGG"
        moves1 = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]
        moves2 = ["FEW","I","IF","KEY","OF","OFF","OFFICE","OK","WE","WIFE"]
        moves3 = ["A","GUN","LAW","RUN","WAR"]
    in printTests q "Testing validMoves"
      [ assertEqual (sort $ validMoves shortDict hand1) moves1 show
      , assertEqual (sort $ validMoves shortDict hand2) moves2 show
      , assertEqual (sort $ validMoves shortDict hand3) moves3 show
      , assertEqual (sort $ validMoves shortDict "") [] show
      ][]
        
testMaximumMove :: Bool -> IO Bool
testMaximumMove q =
    let moves1 = ["THIS", "IS", "A", "TEST", "ZZZ", "HOPEFULLY", "THE", "MAX", "WORD", "WILL", "NOT", "BE", "ZZZ", "THE", "LONGEST"]
        moves2 = ["ZZZ", "THIS", "IS", "A", "TEST", "HOPEFULLY", "THE", "MAX", "WORD", "WILL", "NOT", "BE", "THE", "LONGEST"]
        moves3 = ["THIS", "IS", "A", "TEST", "HOPEFULLY", "THE", "MAX", "WORD", "WILL", "NOT", "BE", "THE", "LONGEST", "ZZZ"]
    in printTests q "Testing maximumMove"
        [ assertEqual (maximumMove moves1) ("ZZZ") show
        , assertEqual (maximumMove moves2) ("ZZZ") show
        , assertEqual (maximumMove moves3) ("ZZZ") show
        ][assertError (maximumMove []) "You should throw an error on an empty list!"
        ]

testGreedyPlay :: Bool -> IO Bool
testGreedyPlay q = printTests q "Testing greedyPlay"
        [ assertEqual (greedyPlay tinyDict "THEIR") ["THEIR"] show
        , assertEqual (sort $ greedyPlay shortDict "CLOSEFLOOR") ["FORCE", "SO"] show
        , assertEqual (sort $ greedyPlay customDict "FIVEO") ["FIVE"] show
        , assertEqual (sort $ greedyPlay customDict "FIVEFIVEFIVEFIVEFIVEFIVEO") (replicate 6 "FIVE") show
        ][]
        where customDict = ["FIVE","IVE","OF"]

testPowerset :: Bool -> IO Bool
testPowerset q =
    printTests q "Testing powerset"
              [ assertEqual (sms $ powerset lst1) (res1) show
              , assertEqual (sms $ powerset lst2) (res2) show
              , assertEqual (sms $ powerset lst3) (res3) show
              , assertEqual (sms $ map sort $ powerset lst4) (res4) show
              , assertEqual (length $ powerset lst4) (length res4) show
              ][]
    where sms lst = sort $ map sort lst
          lst1 = [1, 2] :: [Int]
          lst2 = ['a', 'b', 'c']
          lst3 = [] :: [Int]
          lst4 = [1..7] :: [Int]
          res1 = [[],[1],[1,2],[2]] :: [[Int]]
          res2 = ["","a","ab","abc","ac","b","bc","c"]
          res3 = [[]] :: [[Int]]
          res4 = [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[1,2,3,4,5,6],[1,2,3,4,5,6,7]
                  ,[1,2,3,4,5,7],[1,2,3,4,6],[1,2,3,4,6,7],[1,2,3,4,7],[1,2,3,5],[1,2,3,5,6],[1,2,3,5,6,7]
                  ,[1,2,3,5,7],[1,2,3,6],[1,2,3,6,7],[1,2,3,7],[1,2,4],[1,2,4,5],[1,2,4,5,6],[1,2,4,5,6,7],[1,2,4,5,7]
                  ,[1,2,4,6],[1,2,4,6,7],[1,2,4,7],[1,2,5],[1,2,5,6],[1,2,5,6,7],[1,2,5,7],[1,2,6],[1,2,6,7],[1,2,7]
                  ,[1,3],[1,3,4],[1,3,4,5],[1,3,4,5,6],[1,3,4,5,6,7],[1,3,4,5,7],[1,3,4,6],[1,3,4,6,7],[1,3,4,7],[1,3,5]
                  ,[1,3,5,6],[1,3,5,6,7],[1,3,5,7],[1,3,6],[1,3,6,7],[1,3,7],[1,4],[1,4,5],[1,4,5,6],[1,4,5,6,7],[1,4,5,7]
                  ,[1,4,6],[1,4,6,7],[1,4,7],[1,5],[1,5,6],[1,5,6,7],[1,5,7],[1,6],[1,6,7],[1,7],[2],[2,3],[2,3,4]
                  ,[2,3,4,5],[2,3,4,5,6],[2,3,4,5,6,7],[2,3,4,5,7],[2,3,4,6],[2,3,4,6,7],[2,3,4,7],[2,3,5],[2,3,5,6],[2,3,5,6,7]
                  ,[2,3,5,7],[2,3,6],[2,3,6,7],[2,3,7],[2,4],[2,4,5],[2,4,5,6],[2,4,5,6,7],[2,4,5,7],[2,4,6]
                  ,[2,4,6,7],[2,4,7],[2,5],[2,5,6],[2,5,6,7],[2,5,7],[2,6],[2,6,7],[2,7],[3],[3,4],[3,4,5],[3,4,5,6],[3,4,5,6,7]
                  ,[3,4,5,7],[3,4,6],[3,4,6,7],[3,4,7],[3,5],[3,5,6],[3,5,6,7],[3,5,7],[3,6],[3,6,7]
                  ,[3,7],[4],[4,5],[4,5,6],[4,5,6,7],[4,5,7],[4,6],[4,6,7],[4,7],[5],[5,6],[5,6,7],[5,7],[6],[6,7],[7]] 

testMaximumPlay :: Bool -> IO Bool
testMaximumPlay q =
    let playss1 = [[["ZZ"],["AQUA", "AQUA"],["ABC", "DEF"]]
                  ,[["ZZ"],["ABC", "DEF"],["AQUA", "AQUA"]]
                  ,[["AQUA", "AQUA"],["ZZ"],["ABC", "DEF"]]]
        plays2 = [replicate 6 "ZZ", ["ZZZ"], ["QQQQQQQQQQQ"]]
        plays3 = []
    in printTests q "Testing maximumPlay"
      [ assertEqual (map maximumPlay playss1) (replicate 3 ["AQUA","AQUA"]) show
      , assertEqual (maximumPlay plays2) (head plays2) show
      ][assertError (maximumPlay plays3) "You should throw an error on an empty list!"
      ]


testNaiveBrutePlay :: Bool -> IO Bool
testNaiveBrutePlay q = printTests q "Testing naiveBrutePlay"
        [ assertEqual (naiveBrutePlay ["THEIR"] "THEIR") ["THEIR"] show
        , assertEqual (sort $ naiveBrutePlay customDict "FIVEO") ["IVE", "OF"] show
        , assertEqual (scorePlay $ naiveBrutePlay perfectDict "CLOSEFLOOR") (score "CLOSEFLOOR") show
        ][]
    where
        perfectDict = ["CLOSE", "FLOOR", "OTHER", "YOUR", "FORCE", "SO"]
        customDict = ["FIVE","IVE","OF"]

testSmartBrutePlay :: Bool -> IO Bool
testSmartBrutePlay q = printTests q "Testing smartBrutePlay"
        [ assertEqual (smartBrutePlay cTinyDict "THEIR") ["THEIR"] show
        , assertEqual (sort $ smartBrutePlay customDict "FIVEO") ["IVE", "OF"] show
        , assertEqual (scorePlay $ smartBrutePlay shortDict "CLOSEFLOOR") (score "CLOSEFLOOR") show
        ] []
        where cTinyDict = tinyDict \\ ["IT"]
              customDict = ["FIVE","IVE","OF"] ++ [replicate k 'Z' | k <- [1..11]]

--we need more and better tests, but it's not critical for now.
testRecBestPlay:: Bool -> IO Bool
testRecBestPlay q = printTests q "Testing recBestPlay"
        [ assertEqual (recBestPlay cTinyDict "THEIR") ["THEIR"] show
        , assertEqual (scorePlay $ recBestPlay shortDict "CLOSEFLOOR") (score "CLOSEFLOOR") show
        , assertEqual (scorePlay $ recBestPlay otherTinyDict "CLOSECLOSECLOSEFLOORFLOORFLOOR") (3*(score "CLOSEFLOOR")) show
        , assertEqual (scorePlay $ recBestPlay shortDict "AAAAA") (score "AAAAA") show
        ] []
        where cTinyDict = tinyDict \\ ["IT"]
              otherTinyDict = ["CELL","CLOSE","FLOOR","FOR","FORCE", "THING", "THINK", "THIS",
                "THOSE", "TIME", "TO", "TWO", "UP", "USE", "VERY", "WANT", "WAY", "WE", "WELL",
                "WHAT", "WHEN", "WHICH", "WHO", "WILL", "WITH", "WOULD", "YEAR", "YOU", "YOUR"] 

testRunTimes :: Bool -> IO Bool
testRunTimes q = 
    do unless q $ putStrLn $ unlines ["Estimating complexities. These tests do count, but are non-critical and less important than correctness."
                                     ,"They are also estimates, so verify unexpected results by running them a few times."]
       printTests q "\tcanMake" [assertLinear (genCM "ABCDEFG") (uncurry canMake)][]
       printTests q "\tvalidMoves" [assertLinear (genVW "OTHERYOUR") (uncurry validMoves) ][]
       printTests q "\tgreedyPlay" [assertPoly (genGP "OTHERYOUR") (uncurry greedyPlay) 5][]
       printTests q "\tpowerset" [assertNonPoly genPS powerset 20] []
       return True
    where genCM str n = 
            let strA = concat $ map (replicate (16*n)) str
            in (strA, reverse strA)
          genVW str n = 
            let strA = concat $ map (replicate 4) str
                shortIshDict = sort $ concat $ replicate (8*n) shortDict
            in  (shortIshDict, strA)
          genGP str n = 
            let strA = concat $ map (replicate (10*n)) str
                dictA = concat $ replicate n shortDict
            in  (shortDict, strA)
          genPS n = [1..n]

runTests :: Bool -> IO ()
runTests q = do
        milestone <- fmap and $ sequence $ 
                  [ testScore q
                  , testScorePlay q
                  , testRemove q
                  , testIsValidMove q
                  , testCanMake q
                  , testUpdateHand q
                  , testIsValidPlay q
                  , testValidMoves q
                  ]
        if not milestone 
        then putRedLn "Milestone not completed."
        else do 
        putGreenLn "You passed critical tests for the milestone.\n" 
        project <- fmap and $ sequence $ 
                  [ testPowerset q
                  , testMaximumMove q
                  , testGreedyPlay q
                  , testMaximumPlay q
                  , testNaiveBrutePlay q
                  , testSmartBrutePlay q
                  , testRecBestPlay q
                  ]
        if project 
        then do
              putGreenLn "You pass all critical tests. Try running the game!\n"
              void $ testRunTimes q
        else putRedLn "Project not completed."

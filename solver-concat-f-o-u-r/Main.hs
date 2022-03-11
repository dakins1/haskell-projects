module Main where
import Solver
import Con4
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char
import System.Environment
import System.IO
import System.Console.GetOpt

data Flag = Help | Winner | Depth String | Move String deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print a help message and exit."
               , Option ['w'] ["winner"] (NoArg Winner) "Print out the best move using exhaustive search."
               , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Use <num> as a cutoff depth"
               , Option ['m'] ["move"] (ReqArg Move "<num>") "Make <move> and print out the board"
               ]
main :: IO ()
main = do args <- getArgs
          let (flags, others, errors) = getOpt Permute options args
          dispatch flags others


dispatch :: [Flag] -> [String] -> IO ()
dispatch flags others 
    | Help `elem` flags = printHelp
    | null others = putStrLn $ "Please actually give me a file."
    | Winner `elem` flags = do mGame <- fileToGame (head others)
                               case mGame of
                                  Nothing -> putStrLn $ "Please give me a better file."
                                  Just game -> case bestMove game of
                                                 Nothing -> putStrLn $ "Game already over."
                                                 Just mv -> putStrLn $ "Best move: " ++ (show mv)
    | (getMove flags)/=Nothing = do mGame <- fileToGame (head others)
                                    case mGame of
                                      Nothing -> putStrLn $ "Please give me a better file."
                                      Just game -> case (getMove flags) of
                                                     Just mv ->  case (makeMove (mv-1) game) of
                                                                    Nothing -> putStrLn $ "Invalid move."
                                                                    Just game -> showGame game
                                                     Nothing -> putStrLn $ "You should never see this message"
    | (getDepth flags)/=Nothing = do mGame <- fileToGame (head others)
                                     case mGame of
                                      Nothing -> putStrLn $ "Please give me a better file."
                                      Just game -> case (getDepth flags) of
                                                        Nothing -> putStrLn $ "You should never see this message."
                                                        Just dp -> case (goodMove dp game) of
                                                                    Nothing -> putStrLn $ "Board is full."
                                                                    Just mv -> putStrLn $ "Good move: " ++ (show mv)
    | null flags = do mGame <- fileToGame (head others)
                      case mGame of Nothing -> putStrLn $ "Please give me a better file."
                                    Just game -> case goodMove defaultDepth game of
                                                 Nothing -> putStrLn $ "Game already over."
                                                 Just mv -> putStrLn $ "Good move: " ++ (show mv)
                                  
      where fname = head others

defaultDepth :: Int
defaultDepth = 3

getDepth :: [Flag] -> Maybe Int
getDepth ((Depth s):fs) = Just (read s)
getDepth (_:fs) = getDepth fs
getDepth [] = Nothing

getMove :: [Flag] -> Maybe Int
getMove ((Move s):fs) = Just (read s)
getMove (_:fs) = getMove fs
getMove [] = Nothing
   {-| bool <- do mGame <- fileToGame fname 
                 putStrLn $ (show mGame)
                 return $ mGame == Nothing 
           = do putStrLn $ "Please give me a better file."   -}
 
    -- | (Move mv) `elem` flags = putStrLn $ (show mv)

printHelp :: IO ()
printHelp = putStrLn $ usageInfo "Connect4 [OPTION] [file]" options

--IO Stuff
fileToGame :: FilePath -> IO(Maybe GameState)
fileToGame str = do gameText <- readFile str
                    return $ gameOfString (gameText)

stringToPlayer :: String -> Maybe Player
stringToPlayer s | s=="Player1" = Just Player1
                 | s=="Player2" = Just Player2
                 | otherwise    = Nothing
 
gameOfString :: String -> Maybe GameState
gameOfString str = do
            (hw:p:cs) <- case (lines str) of 
                            [x] -> Nothing
                            [x,y] -> Nothing
                            lst -> Just lst
            let (h:w) = [read x | x<- (words hw)]
            maybeAssert $ ((length (h:w))==2) && (and [isDigit x| x<-(filter (\c -> c/=' ') hw)])
            player <- stringToPlayer p
            board <- sequence [buildCol c h | c<-cs]
            return (board, player)

maybeAssert :: Bool -> Maybe ()
maybeAssert True = Just ()
maybeAssert False = Nothing

buildCol :: String -> Int -> Maybe Column
buildCol str height = do 
    let parseCol :: String -> Slot -> [Maybe Slot]
        parseCol [] _ = []
        parseCol ('0':ss) prev = (Just Empty):(parseCol ss Empty)
        parseCol ('1':ss) (Occ p) = (Just (Occ Player1)):(parseCol ss (Occ Player1))
        parseCol ('2':ss) (Occ p) = (Just (Occ Player2)):(parseCol ss (Occ Player2))
        parseCol ('1':ss) Empty = Nothing:[]
        parseCol ('2':ss) Empty = Nothing:[]
        parseCol (s:ss) prev  = Nothing:[]
        column = parseCol str (Occ Player1) --occ here is a dummy placement holder
    maybeAssert $ (length column)==height 
    sequence column
   
gameToFile :: GameState -> FilePath -> IO ()
gameToFile game path = do writeFile path (stringOfGame game)

stringOfGame :: GameState -> String
stringOfGame ((c:cs), p) = let height = show $ length c
                               width = show $ length (c:cs)
                               player = if (p==Player1) then "Player1" else "Player2"
                               board = concat [stringOfColumn x  | x <-(c:cs)]
                            in height ++ " " ++ width ++ "\n" ++ player ++ "\n" ++ board 

stringOfColumn :: Column -> String
stringOfColumn [] = "\n"
stringOfColumn (s:ss) = (stringOfSlot s)++(stringOfColumn ss)

stringOfSlot :: Slot -> String
stringOfSlot Empty = "0"
stringOfSlot (Occ Player1) = "1"
stringOfSlot (Occ Player2) = "2"
module Mushroom where
import Data.Ratio ((%))
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace


--
-- --                                      Pre-defined functions.
-- 
-- These functions are used to convert between counts, rationals, and string percentages. The counts
-- are similar to those used in the summaries for Project 1. You may not need these functions at all,
-- depending on your implementation choices.

ratioOfCount :: (Int, Int) -> Rational
ratioOfCount (a,b) = (fromIntegral a) % (fromIntegral b)

percentOfRatio :: Rational -> String
percentOfRatio r = (show $ truncate $ 100 * r) ++ "%"

percentOfCount :: (Int, Int) -> String
percentOfCount c = percentOfRatio $ ratioOfCount c 

-- All undefined values and functions should be completed. Your code will compile 
-- even if some functions are left undefined.

--
-- --                                       Milestone
--

-- Mushroom edibility is either Nom or NoNom. We should only eat mushrooms we are pretty certain are
-- Noms. Please do not actually eat any mushrooms based on advice from this project.
data Edible = Nom | NoNom deriving (Show, Eq, Ord)

--Define an algebraic data type for all the different possible attributes.
--You may decompose this by having sub-types, but every possible attribute should be representable
--as an Attribute. You may not use Strings or Integers in this type.
--You should Eq and Show (at least to start). You may also derive Ord, but do not rely on the
--ordering.

data Color = Brown | Green | Purple | White | Yellow deriving (Eq, Ord) 
data ShapeC = Bell | Conical | Knob deriving (Eq, Ord)
data ShapeS = Bulbous | Club | Missing deriving (Eq, Ord)
data Size = Broad | Narrow deriving (Eq, Ord)
data Smell = Almond | Anise | Foul | None | Musty deriving (Eq, Ord)
data Attribute = StalkColor Color | CapColor Color | SporeColor Color | CapShape ShapeC | StalkShape ShapeS | GillSize Size | Odor Smell deriving (Eq, Ord)

instance Show Color where
    show c = case c of 
                    Brown -> "brown"
                    Green -> "green"
                    Purple -> "purple"
                    White -> "white"
                    Yellow -> "yellow"
instance Show ShapeC where
    show s = let sh = case s of 
                    Bell -> "bell"
                    Conical -> "conical"
                    Knob -> "knobbed"
              in sh ++ " cap"
instance Show ShapeS where
    show s = let sh = case s of
                    Bulbous -> "bulbous"
                    Club -> "club"
                    Missing -> "missing"
              in sh ++ " stalk"
instance Show Size where
    show s = case s of 
            Broad -> "broad gills"
            Narrow -> "narrow gills"
instance Show Smell where
    show s = case s of 
            Almond -> "almond odor"
            Anise -> "anise odor"
            Foul -> "foul odor"
            None -> "no odor"
            Musty -> "musty odor"
instance Show Attribute where
    show a = case a of
            StalkColor c -> (aOrAn (show c)) ++ (show c) ++ " stalk"
            CapColor c -> (aOrAn (show c)) ++ (show c) ++ " cap"
            SporeColor c -> show c ++  " spores"
            CapShape s -> (aOrAn (show s)) ++ (show s) 
            StalkShape s -> (aOrAn (show s)) ++ (show s) 
            GillSize s -> show s
            Odor o -> if o==None then (show o) else (aOrAn (show o)) ++ (show o)
            
aOrAn :: String -> String
aOrAn (s:ss) = if s `elem` "aeiou" then "an " else "a " 


-- Make a list of all possible attributes. There should be 28 different attributes.
allAttributes :: [Attribute]
allAttributes = [StalkColor Brown, StalkColor Green, StalkColor Purple, StalkColor White, 
                 StalkColor Yellow, CapColor Brown, CapColor Green, CapColor Purple, 
                 CapColor White, CapColor Yellow, SporeColor Brown, SporeColor Green,
                 SporeColor Purple, SporeColor White, SporeColor Yellow, CapShape Bell, 
                 CapShape Conical, CapShape Knob, StalkShape Bulbous, StalkShape Club,
                 StalkShape Missing, GillSize Broad, GillSize Narrow, Odor Almond, Odor Anise,
                 Odor Foul, Odor None, Odor Musty]

 

--A mushroom is a list of attributes.
type Mushroom = [Attribute]

--An observation is a mushroom that is known to be nomable, or not nomable.  Thus, it is a tuple of
--a mushroom and an edibility.
type Observation = (Mushroom, Edible)

-- readObservation takes a single line of the input file, in the format described on the
-- project page, and return the corresponding observation.  You may find the splitOn function
-- helpful. splitOn takes a string delimiter, a string to be split, and splits the string based on
-- the delimiter.  For instance (words str) is the same as (splitOn " " str)
-- I suggest you make helper functions for reading different columns.
-- The sequence function may be helpful.
readSmell :: String -> Attribute
readSmell str = Odor $ case str of 
                        "almond" -> Almond
                        "anise" -> Anise
                        "foul" -> Foul
                        "none" -> None
                        "musty" -> Musty
readCapShape :: String -> Attribute
readCapShape str = CapShape $ case str of 
                        "bell" -> Bell
                        "conical" -> Conical
                        "knobbed" -> Knob  
readStalkShape :: String -> Attribute
readStalkShape str = StalkShape $ case str of
                        "bulbous" -> Bulbous
                        "club" -> Club
                        "missing" -> Missing
readSize :: String -> Attribute
readSize str = GillSize $ case str of 
                        "broad" -> Broad
                        "narrow" -> Narrow
readEdible :: String -> Edible
readEdible str = case str of 
                        "edible" -> Nom
                        "poison" -> NoNom
readColor :: String -> Color
readColor str = case str of
                        "brown" -> Brown
                        "green" -> Green
                        "purple" -> Purple
                        "white" -> White
                        "yellow" -> Yellow
readStalkColor :: String -> Attribute
readStalkColor str = StalkColor $ readColor str
readCapColor :: String -> Attribute
readCapColor str = CapColor $ readColor str
readSporeColor :: String -> Attribute
readSporeColor str = SporeColor $ readColor str

readObservation :: String -> Maybe Observation
readObservation line = let aux :: Int -> [String] -> [Attribute]
                           aux col [] = [] 
                           aux col (s:ss) = let att = case col of
                                                        1 -> readStalkColor s
                                                        2 -> readCapColor s
                                                        3 -> readSporeColor s
                                                        4 -> readCapShape s
                                                        5 -> readStalkShape s
                                                        6 -> readSize s
                                                        7 -> readSmell s
                                            in att:(aux (col+1) ss)
                           (x:xs) = splitOn "," line 
                    in Just ((aux 1 xs), (readEdible x))

-- readObservationFile takes the entire contents of an entire file and return the list of
-- observations. Note the first line has header information and is not a valid observation. 
-- The lines function may be helpful. 
defOb = ([Odor Almond, Odor Almond, Odor Almond,
          Odor Almond, Odor Almond, Odor Almond, Odor Almond], Nom)
--not really sure what the default should be? Not sure why these are maybes either???
readObservationFile :: String ->  Maybe [Observation]
readObservationFile input = let (s:ss) = lines input
                            in Just [fromMaybe defOb (readObservation x) | x<-(ss)]
testFile = "edibility,stalk-color,cap-color,spore-color,cap-shape,stalk-shape,gill-size,odor\npoison,white,brown,brown,knobbed,missing,narrow,musty\nedible,white,yellow,brown,knobbed,club,broad,almond\nedible,white,white,brown,bell,club,broad,anise\npoison,white,white,brown,knobbed,missing,narrow,musty\nedible,white,brown,brown,knobbed,missing,broad,none\nedible,white,yellow,brown,knobbed,club,broad,almond\nedible,white,white,brown,bell,club,broad,almond\nedible,white,white,brown,bell,club,broad,anise\npoison,white,white,brown,knobbed,missing,narrow,musty\nedible,white,yellow,brown,bell,club,broad,almond\nedible,white,yellow,brown,knobbed,club,broad,anise\nedible,white,yellow,brown,knobbed,club,broad,almond"
testOb = fromMaybe defOb $readObservation "poison,white,brown,brown,knobbed,missing,narrow,musty"
testShroom = fst testOb

testObs = fromMaybe [defOb] $readObservationFile testFile

--numCorrect computes how much information about edibility can be gained by checking a specific
--attribute. It takes a single attribute and a list of observations, and answers the question: 
--"If all we had to go on was this attribute, how many mushrooms would we label correctly?"
--1) Split the observations into the those with the attribute, and those without the attribute. 
--2) One of these sets will have a higher percentage of edible mushrooms. Call that set A, and the
--   other set B. Note that A may be the set with the attribute, or the set without the attribute.
--3) If mushrooms in set A are assumed to be edible, and those in the other set Bare assumed to be
--   inedible, return the number of correct guesses.
--4) Important: if either set is empty, no information is gained by looking at the attribute. Return 0.
--
--You may find the built-in partition function useful.

testPart :: Attribute -> [Observation] -> ([Observation], [Observation])
testPart att lst = let parts = partition (\l -> att `elem` fst l) lst
                      in parts

percentEdible :: [Observation] -> Double
percentEdible obs =100*(fromIntegral (length [x | x<-obs, (snd x) == Nom]) / fromIntegral (length obs))

testLst = [(1.0,'a'), (2.3,'b'), (7.4,'c'), (3.9,'d')]

testNum = [([CapColor Brown],Nom), ([CapColor Brown],Nom), ([CapColor Brown],Nom), ([CapColor Brown], Nom)]

numCorrect :: Attribute -> [Observation] -> Int
numCorrect att lst = let (f,s) = partition (\l -> att `elem` fst l) lst
                         setA = snd $ (max (percentEdible f,f) (percentEdible s,s))
                         setB = snd $ (min (percentEdible f,f) (percentEdible s,s))
                         guessA = length [(o,e) | (o,e)<-setA, e == Nom]
                         guessB = length [(o,e) | (o,e)<-setB, e == NoNom]
                      in if (null setA || null setB) then 0 else guessA + guessB


-- A decision tree is a binary tree that stores the likelihood of a mushroom being edible
-- its attributes.  Decision nodes are labeled with an attribute and have two children, with the
-- left child applying to mushrooms with that attribute, and the right child applying to mushrooms
-- without that attribute.  End nodes are leaves, and  should store enough information to compute
-- the percent chance of a mushroom being edible.  Do not store lists of observations or mushrooms.
-- Doubles are likely not precise enough, but Rationals or tuples (or even triples) of Integers will
-- be sufficient.

-- Define an algebraic data type for decision trees.
data DTree = DecNode Attribute DTree DTree
           | EndNode (Int, Int) deriving Show

-- Given a list of attributes and a list of observations, build a decision tree.
--  * If all the observations have the same edibility, you can safely make an end node: there is no
--    need to further analyze a mushroom.  
--  * If all the observations have the same attributes, you must make an end node : there is no way
--    to futher analyze a mushroom.
--  * Otherwise, go through the list of attributes and find the one that gives you the most
--    information about edibility, as measured by the number of correct guesses that can be obtained if
--    this was the only attribute used.  Then create a decision node using that attribute as a pivot.
--  * For efficiency, you can delete the pivot from the list of attributes, but it doesn't really
--    matter.
--  * You should create helper functions for this problem. 
obEq :: Observation -> Observation -> Bool
obEq ([],e) ([],e1) = True
obEq ((m1:m1s),e) ((m2:m2s),e1) = m1 == m2 && obEq (m1s,e) (m2s,e1)

edEq :: Observation -> Observation -> Bool
edEq (_,e1) (_,e2) = e1 == e2

checkSame :: (a -> a -> Bool) -> [a] -> Bool
checkSame comp [o1] = True
checkSame comp [o1,o2] = comp o1 o2
checkSame comp (o1:o2:os) = (comp o1 o2) && (checkSame comp (o2:os))

buildTree :: [Attribute] -> [Observation] -> DTree
buildTree atts obs | (checkSame obEq obs) = EndNode ((length [m|(m,e)<-obs,e==Nom]), length obs)
                   | (checkSame edEq obs) = EndNode ((length [m|(m,e)<-obs,e==Nom]), length obs)
                   | otherwise = let att = snd $ maximum [((numCorrect a obs),a)|a<-atts] 
                                     (left,right) = partition (\(o,e) -> att `elem` o) obs
                                     newAtts = [x|x<-atts, x /= att]
                                  in DecNode att (buildTree newAtts left) (buildTree newAtts right)
testTree = buildTree allAttributes testObs
--accounted for with the partition step of the algorithm. 
--have something that caps off the tree

--originally at checkSame edEq obs EndNode (if (snd (head obs)==Nom) then (1,1) else (0,1))

--
-- --                                       Core Project
--

-- rateMushroom takes a mushroom, a decision tree, and a safety limit, and returns a string
-- describing if we can eat the mushroom.  Follow the decision tree for this mushroom, and check if
-- the corresponding end node estimates the chance of edibility to be higher than the safety limit.
-- If it is greater than the safety limit, return the string "Eat the mushroom" 
-- If it is less than or equal to the safety limit, return the string "Do not eat the mushroom"
-- For full credit, append the estimate to the string similar to the below:
--   "Eat the mushroom: estimate a 95% chance of being edible."
--   "Do not eat the mushroom: estimate a 40% chance of being poisonous."
-- The ``precentOfRatio`` and ``percentOfCount`` functions may be helful.
navTree :: DTree -> Mushroom -> (Int, Int)
navTree (EndNode (x,y)) s = (x,y)
navTree (DecNode att left right) shroom = if (att `elem` shroom) 
                                          then navTree left shroom
                                          else navTree right shroom
dontEat :: Rational -> String
dontEat rat = "Do not eat the mushroom: estimate a "++(percentOfRatio (1-rat))++" of being poisonous."

doEat :: Rational -> String
doEat rat = "Eat the mushroom: estimate a " ++ (percentOfRatio rat) ++ " of being edible."

rateMushroom :: Mushroom -> DTree -> Rational -> String
rateMushroom shroom tree thresh = let rat = ratioOfCount $ (navTree tree shroom) 
                                  in if rat >= thresh then doEat rat else dontEat rat

-- buildGuide takes a decision tree, a safety limit, and return an itemized guide. 
-- Each line is numbered separately and should have one of two forms.
--  "n: Eat the mushroom." / "n: Do not eat the mushroom."
--  "n: If the mushroom has (attribute) go to step x, otherwise go to step y."
-- For this implementation, every node in the tree will have a separate line.
-- You will need helper functions.
showAtt :: Attribute -> String
showAtt att = "If the mushroom has " ++ (show att) ++ " then go to step "

shouldEat :: (Int,Int) -> Rational -> String
shouldEat (x,y) thresh = let rat = ratioOfCount (x,y)
                          in if rat > thresh then dontEat rat else doEat rat

shouldEat' :: (Int,Int) -> Rational -> String
shouldEat' (x,y) thresh = let rat = ratioOfCount (x,y)
                          in if rat >= thresh then "Do not eat the mushroom." else "Eat the mushroom."
buildGuide :: DTree -> Rational -> [String]
buildGuide tree thresh = let builder :: DTree -> Int -> [String]
                             builder (EndNode (x,y)) step = [(show step)++": "++(shouldEat' (x,y) thresh)]
                             builder (DecNode att left right) step = ((show step)++": "++this):(strLeft++strRight)
                                                                    where strLeft = builder left (step+1)
                                                                          szLeft = length strLeft
                                                                          strRight = builder right (step+szLeft+1) 
                                                                          conditional =(showAtt att)++(show (step+1))++", otherwise go to step " 
                                                                          this = conditional++(show ((length strLeft)+step+1))
                          in builder tree 1                                              
--
-- --                                       Full Credit
--

-- For the first full credit, improve on the derived Show instance for attributes.  Make a custom
-- instance of Show that returns a proper English clause describing the attribute. For instance, "a
-- club stalk", "narrow gills", or "an almond odor." 


-- For the second full credit portion, you will eliminate redundancies in the guide. This will be
-- done using common subexpression elimination. We will keep an index mapping strings to integer
-- locations. Since indexes are useful for other types, we will write generic functions.
type Index a = [(a, Int)]

-- makeEntry adds an element to the index. It returns the location of the element in 
-- the index and, if necessary, an updated index.
-- If the element is already in the index, you should not add it again. 
-- If it does not occur in the index, find the next largest location and associate that element with
-- that location.
-- Index locations should start at 1.
addEntry :: Eq a => a -> Index a -> (Int, Index a)
addEntry entry index = let newLoc = if (null index) then 1 else 1 + (maximum [i | (v,i)<-index])
                           newIndex = index ++ [(entry, (newLoc))]
                        in (newLoc, newIndex)
makeEntry :: Eq a => a -> Index a -> (Int, Index a)
makeEntry entry index = let 
    location = [loc| (val,loc)<-index, val==entry]
    (l:ls) = location
    in if (null location) then (addEntry entry index)
       else (l, index) --returns the location and un-updated index

entry1 = makeEntry 'x' [('a',1),('x',2),('b',3)] 
entry2 = makeEntry 'y' [('a',1),('x',2),('b',3)]

-- For instance: makeEntry 'x' [('a',1),('x',2),('b',3)] = (2, [('a',1),('x',2),('b',3)])
-- For instance: makeEntry 'y' [('a',1),('x',2),('b',3)] = (4, [('a',1),('x',2),('b',3),('y',4)])
-- The order of the entries in the index does not matter, and will quite likely be reversed.

-- Once makeEntry is working, make a version of buildGuide (buildGuideCSE) that passes around an index of
-- strings.  When you want to add a string to the index, use makeEntry to avoid creating duplicates.
-- As a natural consequence of this, you will return an "upside-down" guide: the entry point will
-- have the largest location.
buildIndex :: DTree -> Rational -> Index String -> (Int, Index String)
buildIndex tree thresh index = case tree of 
                EndNode (x,y) -> if (ratioOfCount (x,y)) <= thresh 
                                 then makeEntry "Do not eat the mushroom." index 
                                 else makeEntry "Eat the mushroom." index 
                DecNode att left right -> let prevEntryLeft = buildIndex left thresh index
                                              (lastLocL, indexL) = prevEntryLeft
                                              prevEntryRight = buildIndex right thresh indexL
                                              (lastLocR, indexR) = prevEntryRight
                                              newStr = concat ["If the mushroom has " ++ (show att)
                                                              , " then go to step " ++ (show lastLocL)
                                                              , ", otherwise go to step " ++ (show lastLocR)]
                                            in makeEntry newStr indexR
--Current issue is that this only considers one half of the tree
--make a combin index problem?

buildGuideCSE :: DTree -> Rational -> [String]
buildGuideCSE tree thresh = 
    let (l, index) = buildIndex tree thresh []
        start = "Start on line " ++ show (maximum [l | (s,l)<-index]) ++ "."
     in start:((reverse [(show l) ++ ": " ++ s | (s,l)<-index]))
                             

-- For extra credit, change indexes from association lists to association binary search trees.

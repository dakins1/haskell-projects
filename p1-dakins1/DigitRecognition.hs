module DigitRecognition where
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Ratio (numerator, denominator)
import Data.Ratio ((%))
import Debug.Trace

-- All undefined values and functions should be completed. Your code will compile and test 
-- (with the -- test flag) even if some functions are left undefined.

--                                          Type Aliases
-- These type aliases help to abstract our code. You will use them extensively in DigitRecognition.hs
--
type PixelImage = [[Bool]] 
-- A pixel image is a two-dimensional list of booleans.
-- False represents an empty pixel, and True a grey or black pixel. Each pixel image will be 28x28.
type Feature = Int
-- Each pixel location is considered a separate feature for classification. Because the image is
-- 28x28, there are 784 total features.
type Digit = Integer
-- Each image is of a specific digit, 0 through 9. To distinguish the labels or guesses from
-- other numbers, we use a type alias.


--                                      Primitive Functions
-- These functions will be used in your implementation of the classifier. Be
-- sure you understand how they are used, but you do not have to undersatnd how they work.
--

--hasFeature checks if an image has a specific feature: i.e. if that pixel is white or blank.
--
--This encapsulates the ugliness of storing images as nested lists. Notice the normally
--forbidden use of !!. This suggests that there should be a better way to handle and process
--images. For the purposes of this project we will accept this.  We can take reassurance that
--lists of length 28 are so short that better storage methods are probably unnecessary.
hasFeature :: PixelImage -> Feature -> Bool
hasFeature img ftr = 
    let dim = length img
        row = img !! (ftr `div` dim)
        pixel = row !! (ftr `mod` dim)
    in pixel
-- Example:    img `hasFeature` ftr

--outOf wraps around Haskell's built-in Rational data type. Rationals store fractional values
--precisely, with no possibility of underflow. Internally, the numerator and denominator are
--kept as Integers, which have no maximum outside the space limitations of computer memory. You
--will use this function to return an estimated probability. 
outOf :: Int -> Int -> Rational
outOf a b =  (fromIntegral a) % (fromIntegral b)
--Example:      2 `outOf` 10
--              (length [1..3]) `outOf` (length [1..10])
   

--                                       Milestone 

-- Create a list of all possible features, starting at 0.
allFeatures :: [Feature]
allFeatures = [0..783]

-- Create a list of all possible digit labels. 
allDigits :: [Digit]
allDigits = [0..9]

-- showPixelImage should take a PixelImage and turn it into a single string.
-- Since we have lost gray colors (see readPixelImage in Framework.hs), our
-- string will have '#' for black pixels, and ' ' for white pixels.
--
-- I suggest a helper function that takes an individual row of a pixel image and turns it into a
-- string. You can then use the built-in (unlines) function, which takes a list of strings and
-- turns them into a single string, separated by newline.
-- 
-- Example: showPixelImage [[True, True], [True, False]]
--          "##\n# \n"
showPixelImageHelper :: [Bool] -> String
showPixelImageHelper lst = [if x==True then '#' else ' ' | x <- lst]

showPixelImage :: PixelImage -> String
showPixelImage img = unlines ([showPixelImageHelper x | x <- img]) 

-- lookupVal takes a key of type a, an association list from a to b, and returns the hopefully
-- unique value associated with the key. If lst contains the tuple (k, v), then 
-- lookupVal k lst should return v.
--
-- For full credit, ensure that the key matches exactly one tuple in the list.
-- -- Example: lookupVal 7 [(8, 'a'), (7, 'b'), (9,'c')]
--          'b'
lookupVal :: Eq a => a -> [(a, b)] -> b
--lookupVal key lst = if (snd (head [(x,y) | (x,y) <- lst, x==key])
lookupVal key lst = if length [(x,y) | (x,y) <- lst, x==key] == 1
                        then snd (head [(x,y) | (x,y) <- lst, x==key])
                        else error "Improper pixel image!"

--not sure how we would ensure there's only one tuple? What would we do instead? 

-- A corpus is an association list between digits and the images that are labeled with that
-- digit. By storing the information this way, we avoid the frequent computation of which images
-- are labeled with which digit. 
type OldCorpus = [(Digit, [PixelImage])]

type Summary = [[(Int, Int)]]
type DigitSummary = [(Digit, Summary)]
type DigitCount = [(Digit, Int)]
type Corpus = (DigitCount, DigitSummary)
--and PixelImage is a type of [[Bool]]; a two dimensional list of bools

-- When we read in the files, we get a list of image-label tuples. It is far more efficient to
-- group the images by their label as a Corpus. buildCorpus takes the list of tuples and
-- separates it into sub-lists for each label. Order does not matter, either of the digits or of
-- the images associated with each digit.
--
-- I suggest a helper function that takes a digit and returns the list of all images labeled with
-- that digit.
--
-- For full credit, only create entries in the Corpus for digits that actually have associated
-- images. You will need to use the (nub) function, which returns the set version of a list (i.e.
-- all duplicate elements have been removed). Your function must still run quickly!
-- 
-- aka don't let a 7 be in the corpus if there isn't an image for 7
--
--------------------------New Corpus-------------------------------------------------
--
featureGrid :: [[Feature]]
featureGrid = chunksOf 28 allFeatures --just the list of features in a grid format
pairDigit :: Digit -> [(PixelImage, Digit)] -> [PixelImage]
pairDigit num img = [fst x | x <- img, snd x == num]--take a digit and returns all images with that digit


--count the number of times a feature appears in a list of images
countOccurences :: [PixelImage] -> Feature -> Int
countOccurences imgLst ft = length [ x | x<-imgLst, x `hasFeature` ft] 

--makes a list of how many times each feature occurs, with the index representing the feature
forgeSummary :: [PixelImage] -> [[(Int,Int)]]
forgeSummary imgLst = chunksOf 28 tupLst
            where tupLst = [ (x,(length imgLst)-x) | y<-allFeatures, x<-[imgLst `countOccurences` y]] 


--take a digit and returns all images with that digit
returnImgs :: Digit -> [(PixelImage, Digit)] -> [PixelImage]
returnImgs num tups = [fst x|x<-tups, snd x == num]

--DigitSummary :: [(Digit, Summary)]
forgeDigitSummary :: [(PixelImage, Digit)] -> DigitSummary
forgeDigitSummary imgs = [ (x, forgeSummary y) | x<-allDigits, y<-[returnImgs x imgs],
                                                    y /= []]

--DigitCount :: [(Digit, Int)]
forgeDigitCount :: [(PixelImage, Digit)] -> DigitCount
forgeDigitCount imgLst = [ (x,length y) | x<-allDigits, y<-[returnImgs x imgLst], y/=[]]

--Corpus :: (DigitCount, DigitSummary)
buildCorpus :: [(PixelImage, Digit)] -> Corpus
buildCorpus imgLst = (forgeDigitCount imgLst, forgeDigitSummary imgLst)

--testing corpus data
img1 = [[True, True],[False,False]] --2
img2 = [[True, True],[False,False]] --2
img3 = [[True, True],[False,False]] --2

img4 = [[False, False],[True, True]] --6
img5 = [[False, False],[True,True]] --6
img6 = [[False, False],[True,True]] --6
testImage2 = [[True, True], [False, False]]
testImage6 = [[False, False], [True, True]]
imgInput = [(img1,2),(img2,2),(img3,2),(img4,6),(img5,6)]
corpus = buildCorpus imgInput
imgLst2 = [img1,img2,img3]
imgLst6 = [img4,img5,img6]
img2summ = forgeSummary imgLst2
img6summ = forgeSummary imgLst6
--make sure to adjust allFeatures and allDigits to use this test case
--allFeatures = [0,1,2,3]
--allDigits = [2,6]
--and change chunksOf to parse at 2 and not 28
--
--                                  Core Project 

-- Given a corpus and a specific digit Y, probOfDigit estimates P(Y). This is the fraction
-- of the images in the corpus that are labeled with Y.  You will need to use `outOf` to create
-- the fraction.
-- You may find the (sum) function helpful: it takes a list of numbers and adds them together.
-- Example: probOfDigit corpus 9
--         2 % 3
-- Corpus :: ( )
--DigitSummary = [(Digit, Int)]
probOfDigit :: Corpus -> Digit -> Rational
probOfDigit corpus digit = top `outOf` bottom
                   where bottom = sum [snd x|x<-(fst corpus)]
                         top = (snd (head [x|x<-(fst corpus), fst x==digit]))
--WARNING: Throws error on empty list; not sure if this will be an issue

-- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
-- probOfFeature imgs ftr estimates the probability P(ftr=Black | Y). See the assignment page for
-- details.
-- hasFeature :: PixelImage -> Feature -> Bool
getFeature :: Summary -> Feature -> (Int,Int)
getFeature sum ind =
            let row = sum !! (ind `div` 28) 
                val = row !! (ind `mod` 28)
            in val

probOfFeature :: Summary -> Feature -> Rational
probOfFeature summary ftr = if cfy==0 then (cfy+1) `outOf` cy else cfy `outOf` cy
                         where cfy = fst (summary `getFeature` ftr)
                               cy = (fst (head (head summary))) + (snd (head (head summary)))

-- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
-- probOfNOFeature imgs ftr estimates the probability P(ftr=White | Y). See the assignment page
-- for details.  probOfNoFeature :: Summary -> Feature -> Rational probOfNoFeature summary ftr = (cfy+1) `outOf` cy 
probOfNoFeature :: Summary -> Feature -> Rational
probOfNoFeature summary ftr = if (cfy==0) then (cfy+1) `outOf` cy else cfy `outOf` cy
                         where cfy = snd (summary `getFeature` ftr)
                               cy = (fst (head (head summary))) + (snd (head (head summary)))




-- rankOfDigit should estimate the rank of a given digit for a given instance, as specified on
-- the assignment page.
-- You will need to use both probOfDigit, probOfFeature, and probOfNoFeature. 
-- You may find the (product) function helpful.
-- I recommend you calculate the values for positive features (those that occur in newImg)
-- and negative features (those that do not occur in newImg) separately.
rankOfDigit :: Corpus -> Digit -> PixelImage -> Rational
rankOfDigit corpus digit newImg = digProb * whiteProb * blackProb
            where digProb = probOfDigit corpus digit
                  summ = head [snd x | x<-(snd corpus), fst x== digit]
                  blackProb = product [probOfFeature summ y | y<-allFeatures, newImg `hasFeature` y]
                  whiteProb = product [probOfNoFeature summ y | y<-allFeatures, not (newImg `hasFeature` y)]

-- classifyImage should return the most likely digit, based on the rank computed by rankOfDigit.
-- You will need to use the maximum function.
-- An important fact: if you have a tuple of two values, maximum returns based on the first 
-- value.
-- For full credit, make sure you check that the maximum rank is greater than 0. If it is not,
-- print an error message. You will get errors until smoothing is working correctly!
classifyImage :: Corpus -> PixelImage -> Digit
classifyImage corpus newImg = if (maximum probs)==0 then error "Max prob is 0!"
                              else head [fst x | x <- digitProbs, snd x == maximum probs]
                     where digitProbs = [(x, rankOfDigit corpus x newImg) | x<-allDigits]
                           probs = [snd x | x<-digitProbs]
                    


--                                  Optional Helpful Functions
-- These functions are optional, but may be helpful with debugging. They are not worth any points.

-- valueOfRank takes a rank and turns it into a somewhat reasonable integer, suitable for
-- printing. The ranks may be negative, that's perfectly fine.
valueOfRank :: Rational -> Int
valueOfRank r = 350 + ratLog r 
    where numDigits x = length $ show x
          ratLog r = (numDigits $ numerator r) - (numDigits $ denominator r)


-- rankImage is similar to classify image, but instead of returning the best digit, it should
-- return the list of digits and their ranks. Used by the --ranking flag.
-- It is helpful, but not necessary, to sort the list.
rankImage :: Corpus -> PixelImage -> [(Digit, Int)]
rankImage corpus newImg = 
    undefined

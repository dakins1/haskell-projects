type Digit = Integer
type PixelImage = [[Bool]]
type Corpus = [(Digit, [PixelImage])]

allDigits = [0..9]

funnnc :: PixelImage -> PixelImage
funnnc lst = [x | x <- lst]

imgA = [[True, False]]
imgB = [[False, True]]
imgC = [[False, False]]
allImgs = [imgA, imgB, imgC]
imgLbls = [(imgA, 9), (imgB, 2), (imgC, 9)]

--helper :: Digit -> [(PixelImage, Digit)] -> [PixelImage]
--helper num lst = [ fst (x, y) | (x, y) <- lst, y <- allDigits, y == num]
--helper num lst = [ fst (x, y) | y <- allDigits, y == num, (x,y) <- lst]
dumbHelper num lst = [ fst (x,y) | y <- allDigits, y == num, x <- [fst (w,z) | (w,z)<-lst], (x,y) <- imgLbls]
--helper1 num lst = [fst (y,x) | y <- allDigits, y ==num, (x,y) <- (helper num lst)]

helper num img = [fst x | x <- imgLbls, snd x == num]

buildCorpus :: [(PixelImage, Digit)] -> Corpus
buildCorpus imgLst = [  x  | x <- [(y, helper y imgLst) | y <- allDigits], snd x /= []]

buildCorpus2 imgLst = [(x, y) | x <- allDigits, y == (helper x imgLst), y /= []]
--[(x, helper x imgLst) | x <- allDigits] first idea

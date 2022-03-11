
type Digit = Integer
type PixelImage = [[Bool]]
type Feature = Int
type Corpus = [(Digit, [PixelImage])]

imgA = [[True, False]]
imgB = [[False, False]]
imgC = [[False, True]]
imgLbls = [(imgA, 9), (imgB, 2), (imgC, 9)]
corps = buildCorpus imgLbls 

helper :: Digit -> [(PixelImage, Digit)] -> [PixelImage]
helper num img = [fst x | x <- img, snd x == num]

buildCorpus :: [(PixelImage, Digit)] -> Corpus
buildCorpus imgLst = [ x | x <- [(y, helper y imgLst) | y <- allDigits], snd x /= [] ] 



digitProb ::Corpus -> Digit -> Rational
digitProb corpus digit = (length (snd (head [x|x<-corpus, fst x==digit]))) `outOf` (sum [length (snd x) | x <-corpus])


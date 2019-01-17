module Impl where

-- command to run the test:
-- runhaskell Test.hs

-- colored printing enabled
coloredPrint = True



-- write your solutions here


-- Camping basics

addEntry :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
addEntry x xs = x : xs

removeNewestEntry :: [(Char, Int)] -> [(Char, Int)]
removeNewestEntry [] = []
removeNewestEntry (_:xs) = xs

removeOldestEntry :: [(Char, Int)] -> [(Char, Int)]
removeOldestEntry = reverse . removeNewestEntry . reverse

getAllKeys :: [(Char, Int)] -> [Char]
getAllKeys = map fst

getAllValues :: [(Char, Int)] -> [Int]
getAllValues = map snd


-- Navigating in the woods

removeEntry :: Char -> [(Char, Int)] -> [(Char, Int)]
removeEntry k = filter (\x -> k /= fst x)

-- hint: data Maybe Int = Just Int | Nothing 
searchEntry :: Char -> [(Char, Int)] -> Maybe Int
searchEntry k d = maybeFirst $ getAllValues $ filter (\x -> k == fst x) d

maybeFirst [] = Nothing
maybeFirst (x:_) = Just x

optimize :: [(Char, Int)] -> [(Char, Int)]
optimize [] = []
optimize (x:xs) = x : (optimize $ removeEntry (fst x) xs)


-- Two maps, we are lost

subtractDictionary :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
subtractDictionary xs ys = filter (\x -> not $ elem (fst x) (getAllKeys ys)) xs

mergeDictionary :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
mergeDictionary a b = a ++ subtractDictionary b a

mergeDictionaryWith :: (Int -> Int -> Int) -> [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
mergeDictionaryWith f a b = let onlyA = subtractDictionary a b
                                onlyB = subtractDictionary b a
                                conflicted = [(fst x, (f (snd x) (snd y))) | x <- a, y <- b, fst x == fst y]
                            in onlyA ++ conflicted ++ onlyB


-- Being asked by a kind traveler

yourAnswer = "Hi!"

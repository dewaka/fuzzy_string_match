module FuzzyMatch where

matchingCount xs ys = length $ filter (uncurry (==)) $ zip xs ys

bigrams [] = []
bigrams [_] = []
bigrams xs@(_:xs') = take 2 xs : bigrams xs'

intersection [] _ = []
intersection (x:xs) ys = if x `elem` ys
                         then x : intersection xs ys
                         else intersection xs ys

diceIndex x y = fromIntegral (2*nT) / fromIntegral (nX + nY)
  where
    nT = length $ intersection bX bY
    nX = length bX
    nY = length bY
    bX = bigrams x
    bY = bigrams y

hammingDistance [] [] = Just 0
hammingDistance [] _ = Nothing
hammingDistance _ [] = Nothing
hammingDistance (x:xs) (y:ys)
  | x == y = case hammingDistance xs ys of
              Nothing -> Nothing
              Just n -> Just (n+1)
  | otherwise = hammingDistance xs ys


jaroWrinkler sOne sTwo  = undefined

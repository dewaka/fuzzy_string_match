module FuzzyMatch where

matchingCount xs ys = length $ filter (uncurry (==)) $ zip xs ys

jaroWrinkler sOne sTwo  = undefined

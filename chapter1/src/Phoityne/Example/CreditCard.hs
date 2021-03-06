module Phoityne.Example.CreditCard
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , validate
    , cardValidation
    , boolToString
    ) where

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | otherwise = toDigits(x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse(toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse . doubleElements $ reverse  x
-- doubleEveryOther x = reverse(doubleElements(reverse x ) )

doubleElements :: [Integer] -> [Integer]
doubleElements [] = []
doubleElements [x] = [x]
doubleElements (x:y:zs) = x : (y * 2) : doubleElements zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
-- sumDigits (x:xs) = (sum . toDigits $ x) + (sumDigits xs)
-- sumDigits xs = foldr (+) 0 xs
sumDigits xs = foldr (\ x -> (+) (sum . toDigits $ x)) 0 xs

validate :: Integer -> Bool
validate x
    | x `mod` 10 == 0 = True
    | otherwise       = False

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

cardValidation :: IO ()
cardValidation = putStrLn (boolToString(validate (sumDigits . doubleEveryOther . toDigits $ 4012888888881881)))
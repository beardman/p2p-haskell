module Main where

import Phoityne.Example.CreditCard

main :: IO ()
main = putStrLn (boolToString(validate (sumDigits (doubleEveryOther (toDigits 4012888888881881)))))
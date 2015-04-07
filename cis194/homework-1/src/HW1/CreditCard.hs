module HW1.CreditCard
( toDigits
, toDigitsRev
, doubleEveryOther
, sumDigits
, validate
) where

type CCNumber = Integer

toDigits :: CCNumber -> [Integer]
toDigits n = reverse(toDigitsRev n)

toDigitsRev :: CCNumber -> [Integer]
toDigitsRev n
  | n <= 0                             = []
  | n `div` 10 == 0 && n `mod` 10 == n = [n] -- a single decimal digit
  | otherwise                          = n `mod` 10 : toDigitsRev(n `div` 10)

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft []         = []  -- do nothing to the empty list
doubleEveryOtherFromLeft (x:[])     = [x] -- do nothing to lists with a single element
doubleEveryOtherFromLeft (x:y:z)    = x : (y * 2) : doubleEveryOtherFromLeft z

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse(doubleEveryOtherFromLeft(reverse n))

sumDigits :: [Integer] -> Integer
sumDigits n = sum(concat(map toDigits n))

validate :: CCNumber -> Bool
validate n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0

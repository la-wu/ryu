module Data.Floating.Ryu.Common
    ( (.>>)
    , (.<<)
    , mask
    , asWord
    , special
    , decimalLength9
    , decimalLength17
    , pow5bits
    , log10pow2
    , log10pow5
    , multipleOfPowerOf5
    , multipleOfPowerOf2
    , apply
    , lastDigitToChar
    , prependIf
    ) where

import Data.Bits
import Data.Char (chr, ord)

(.>>) :: (Bits a, Integral b) => a -> b -> a
a .>> s = shiftR a (fromIntegral s)

(.<<) :: (Bits a, Integral b) => a -> b -> a
a .<< s = shiftL a (fromIntegral s)

mask :: (Bits a, Integral a) => a -> a
mask = flip (-) 1 . (.<<) 1

asWord :: Integral w => Bool -> w
asWord = fromIntegral . fromEnum

-- Returns the number of decimal digits in v, which must not contain more than 9 digits.
decimalLength9 :: Integral a => a -> Int
decimalLength9 v
  | v >= 100000000 = 9
  | v >= 10000000 = 8
  | v >= 1000000 = 7
  | v >= 100000 = 6
  | v >= 10000 = 5
  | v >= 1000 = 4
  | v >= 100 = 3
  | v >= 10 = 2
  | otherwise = 1

-- Returns the number of decimal digits in v, which must not contain more than 17 digits.
decimalLength17 :: Integral a => a -> Int
decimalLength17 v
  | v >= 10000000000000000 = 17
  | v >= 1000000000000000 = 16
  | v >= 100000000000000 = 15
  | v >= 10000000000000 = 14
  | v >= 1000000000000 = 13
  | v >= 100000000000 = 12
  | v >= 10000000000 = 11
  | v >= 1000000000 = 10
  | v >= 100000000 = 9
  | v >= 10000000 = 8
  | v >= 1000000 = 7
  | v >= 100000 = 6
  | v >= 10000 = 5
  | v >= 1000 = 4
  | v >= 100 = 3
  | v >= 10 = 2
  | otherwise = 1

--         Sign -> Exp  -> Mantissa
special :: Bool -> Bool -> Bool   ->   String
special    _       _       True   =    "NaN"
special    True    False   _      =    "-0E0"
special    False   False   _      =    "0E0"
special    True    True    _      =    "-Infinity"
special    False   True    _      =    "Infinity"


-- Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 3528.
pow5bits :: (Bits a, Integral a) => a -> a
pow5bits e = (e * 1217359) .>> 19 + 1

-- Returns floor(log_10(2^e)); requires 0 <= e <= 1650.
log10pow2 :: (Bits a, Integral a) => a -> a
log10pow2 e = (e * 78913) .>> 18

-- Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
log10pow5 :: (Bits a, Integral a) => a -> a
log10pow5 e = (e * 732928) .>> 20

pow5factor :: Integral a => a -> a
pow5factor value
  | value `mod` 5 /= 0 = 0
  | otherwise = 1 + (pow5factor $ value `div` 5)

multipleOfPowerOf5 :: Integral a => a -> a -> Bool
multipleOfPowerOf5 value p = pow5factor value >= p

multipleOfPowerOf2 :: (Bits a, Integral a) => a -> a -> Bool
multipleOfPowerOf2 value p = value .&. mask p == 0

apply :: (a -> b) -> (a -> a) -> a -> [b]
apply f next x = f x : apply f next (next x)

lastDigitToChar :: (Integral a) => a -> Char
lastDigitToChar = chr . fromIntegral . (+) (fromIntegral $ ord '0') . flip mod 10

prependIf :: Bool -> a -> [a] -> [a]
prependIf True x xs = x : xs
prependIf False _ xs = xs


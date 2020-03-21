{-# LANGUAGE Strict, StrictData #-}

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
    , toChars
    , prependIf
    ) where

import Data.Array.Unboxed
import Data.Bits
import Data.Char (chr, ord)
import Data.Int (Int32)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Word (Word16, Word32, Word64)

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

forDigitsN :: (Integral a, Monoid m) => (a -> m) -> m -> Int -> a -> (m, a)
forDigitsN _ r 0 x = (r, x)
forDigitsN f r n x = forDigitsN f (f x <> r) (n - 1) (x `div` 10)

toAscii :: (Integral a, Integral b) => a -> b
toAscii = fromIntegral . (+) (fromIntegral $ ord '0')

asBuilder :: (Integral a) => a -> BB.Builder
asBuilder = BB.int8 . toAscii

lastDigitToBuilder :: (Integral a) => a -> BB.Builder
lastDigitToBuilder = asBuilder . flip mod 10

prependIf :: Bool -> a -> [a] -> [a]
prependIf True x xs = x : xs
prependIf False _ xs = xs

asAsciiWord :: (Int, Int) -> Word16
asAsciiWord (a, b) = toAscii a * 256 + toAscii b

builder_table :: UArray Int32 Word16
builder_table = listArray (0, 99) [ asAsciiWord (a, b) | a <- [0..9], b <- [0..9] ]

printExp :: Int32 -> BB.Builder
printExp exp
  | exp >= 100 = BB.word16BE (builder_table ! (exp `div` 10)) <> asBuilder (exp `mod` 10)
  | exp >= 10 = BB.word16BE $ builder_table ! exp
  | otherwise = asBuilder exp

class Integral a => Mantissa a where
    decimalLength :: a -> Int

instance Mantissa Word32 where
    decimalLength = decimalLength9

instance Mantissa Word64 where
    decimalLength = decimalLength17

-- TODO: optimize
toChars :: (Mantissa a) => a -> Int32 -> String
toChars mantissa exponent =
    let olength = decimalLength mantissa
        (front, sig) = forDigitsN lastDigitToBuilder mempty (olength - 1) mantissa
        exp = exponent + fromIntegral olength - 1
        back = BB.char7 'E' <> if exp < 0
                                  then BB.char7 '-' <> printExp (-exp)
                                  else printExp exp
     in BS.unpack . BBE.toLazyByteStringWith (BBE.untrimmedStrategy 32 BBE.smallChunkSize) BS.empty $ if olength > 1
           then asBuilder sig <> BB.char7 '.' <> front <> back
           else asBuilder sig <> back


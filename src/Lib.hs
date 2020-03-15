{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( f2s
    ) where

import Data.Array
import Data.Bits.Floating
import Data.Bits
import Data.Char (chr, ord)
import Data.Int (Int32)
import Control.Lens
import GHC.Word (Word32, Word64)

float_mantissa_bits :: Word32
float_mantissa_bits = 23

float_exponent_bits :: Word32
float_exponent_bits = 8

float_bias :: Word32
float_bias = 127

float_pow5_inv_bitcount = 59
float_pow5_bitcount = 61

float_pow5_inv_split :: Array Word32 Word64
float_pow5_inv_split = listArray (0, 30)
    [ 576460752303423489 , 461168601842738791 , 368934881474191033 , 295147905179352826
    , 472236648286964522 , 377789318629571618 , 302231454903657294 , 483570327845851670
    , 386856262276681336 , 309485009821345069 , 495176015714152110 , 396140812571321688
    , 316912650057057351 , 507060240091291761 , 405648192073033409 , 324518553658426727
    , 519229685853482763 , 415383748682786211 , 332306998946228969 , 531691198313966350
    , 425352958651173080 , 340282366920938464 , 544451787073501542 , 435561429658801234
    , 348449143727040987 , 557518629963265579 , 446014903970612463 , 356811923176489971
    , 570899077082383953 , 456719261665907162 , 365375409332725730
    ]

float_pow5_split :: Array Word32 Word64
float_pow5_split = listArray (0, 46)
    [ 1152921504606846976 , 1441151880758558720 , 1801439850948198400 , 2251799813685248000
    , 1407374883553280000 , 1759218604441600000 , 2199023255552000000 , 1374389534720000000
    , 1717986918400000000 , 2147483648000000000 , 1342177280000000000 , 1677721600000000000
    , 2097152000000000000 , 1310720000000000000 , 1638400000000000000 , 2048000000000000000
    , 1280000000000000000 , 1600000000000000000 , 2000000000000000000 , 1250000000000000000
    , 1562500000000000000 , 1953125000000000000 , 1220703125000000000 , 1525878906250000000
    , 1907348632812500000 , 1192092895507812500 , 1490116119384765625 , 1862645149230957031
    , 1164153218269348144 , 1455191522836685180 , 1818989403545856475 , 2273736754432320594
    , 1421085471520200371 , 1776356839400250464 , 2220446049250313080 , 1387778780781445675
    , 1734723475976807094 , 2168404344971008868 , 1355252715606880542 , 1694065894508600678
    , 2117582368135750847 , 1323488980084844279 , 1654361225106055349 , 2067951531382569187
    , 1292469707114105741 , 1615587133892632177 , 2019483917365790221
    ]

(.>>) :: Bits a => a -> Word32 -> a
a .>> s = shiftR a (fromIntegral s)

(.<<) :: Bits a => a -> Word32 -> a
a .<< s = shiftL a (fromIntegral s)

mask :: Word32 -> Word32
mask = flip (-) 1 . (.<<) 1

special :: Bool -> Bool -> Bool   ->   String
special    _       _       True   =    "NaN"
special    True    False   _      =    "-0E0"
special    False   False   _      =    "0E0"
special    True    True    _      =    "-Infinity"
special    False   True    _      =    "Infinity"

data FloatingDecimal = FloatingDecimal
    { mantissa :: Word32
    , exponent :: Int32
    } deriving (Show, Eq)

asWord :: Integral w => Bool -> w
asWord = fromIntegral . fromEnum

toS :: Word32 -> Int32
toS = fromIntegral

toU :: Int32 -> Word32
toU = fromIntegral

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

-- Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 3528.
pow5bits :: (Bits a, Integral a) => a -> a
pow5bits e = (e * 1217359) .>> 19 + 1

-- Returns floor(log_10(2^e)); requires 0 <= e <= 1650.
log10pow2 :: (Bits a, Integral a) => a -> a
log10pow2 e = (e * 78913) .>> 18

-- Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
log10pow5 :: (Bits a, Integral a) => a -> a
log10pow5 e = (e * 732928) .>> 20

pow5factor :: Word32 -> Word32
pow5factor value
  | value `mod` 5 /= 0 = 0
  | otherwise = 1 + (pow5factor $ value `div` 5)

multipleOfPowerOf5 :: Word32 -> Word32 -> Bool
multipleOfPowerOf5 value p = pow5factor value >= p

multipleOfPowerOf2 :: Word32 -> Word32 -> Bool
multipleOfPowerOf2 value p = value .&. mask p == 0

mulShift32 :: Word32 -> Word64 -> Int32 -> Word32
mulShift32 m factor shift =
    let factorLo = fromIntegral factor :: Word32
        factorHi = fromIntegral (factor .>> 32) :: Word32
        m' = fromIntegral m :: Word64
        bits0 = m' * fromIntegral factorLo
        bits1 = m' * fromIntegral factorHi
        sum = bits0 .>> 32 + bits1
     in fromIntegral $ sum .>> (fromIntegral $ shift - 32)

mulPow5InvDivPow2 :: Word32 -> Word32 -> Int32 -> Word32
mulPow5InvDivPow2 m q j = mulShift32 m (float_pow5_inv_split ! q) j

mulPow5divPow2 :: Word32 -> Word32 -> Int32 -> Word32
mulPow5divPow2 m i j = mulShift32 m (float_pow5_split ! i) j

acceptBounds :: Word32 -> Bool
acceptBounds v = v `div` 4 .&. 1 == 0

data BoundsState = BoundsState
    { _v :: Word32
    , _vu :: Word32
    , _vv :: Word32
    , _vw :: Word32
    , _lastRemovedDigit :: Word32
    , _vuIsTrailingZeros :: Bool
    , _vvIsTrailingZeros :: Bool
    } deriving Show

makeLenses ''BoundsState

clear' :: (BoundsState -> Bool) -> BoundsState -> (BoundsState, Int32)
clear' f d
  | f d = fmap ((+) 1) . clear' f $
                d & vu %~ flip div 10
                  & vv %~ flip div 10
                  & vw %~ flip div 10
                  & lastRemovedDigit .~ (d ^. vv `mod` 10)
                  & vuIsTrailingZeros .~ (d ^. vu `mod` 10 == 0)
                  & vvIsTrailingZeros .~ (d ^. lastRemovedDigit == 0)
  | otherwise = (d, 0)

clear :: BoundsState -> (BoundsState, Int32)
clear = clear' (\d -> d ^. vw `div` 10 > d ^. vu `div` 10)

f2dGT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState, Int32)
f2dGT e2 u v w =
    let q = toU $ log10pow2 e2
        e10 = toS $ q
        k = float_pow5_inv_bitcount + pow5bits (toS q) - 1
        i = -e2 + toS q + k
        vu = mulPow5InvDivPow2 u q i
        vv = mulPow5InvDivPow2 v q i
        vw = mulPow5InvDivPow2 w q i
        lastRemovedDigit =
            if q /= 0 && (vw - 1) `div` 10 <= vu `div` 10
               -- We need to know one removed digit even if we are not going to loop
               -- below. We could use q = X - 1 above, except that would require 33
               -- bits for the result, and we've found that 32-bit arithmetic is
               -- faster even on 64-bit machines.
               then let l = float_pow5_inv_bitcount + pow5bits (toS q - 1) - 1
                     in (mulPow5InvDivPow2 v (q - 1) (-e2 + toS q - 1 + l)) `mod` 10
               else 0
        (vvIsTrailingZeros, vuIsTrailingZeros, vw') =
            case () of
              _ | q <= 9 && v `mod` 5 == 0 -> (multipleOfPowerOf5 v q, False, vw)
                | q <= 9 && acceptBounds v -> (False, multipleOfPowerOf5 u q, vw)
                | q <= 9                   -> (False, False, vw - asWord (multipleOfPowerOf5 w q))
                | otherwise                -> (False, False, vw)
     in (BoundsState v vu vv vw' lastRemovedDigit vuIsTrailingZeros vvIsTrailingZeros, e10)

f2dLT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState, Int32)
f2dLT e2 u v w =
    let q = toU $ log10pow5 (-e2)
        e10 = toS q + e2
        i = -e2 - toS q
        k = pow5bits i - float_pow5_bitcount
        j = toS q - k
        vu = mulPow5divPow2 u (toU i) j
        vv = mulPow5divPow2 v (toU i) j
        vw = mulPow5divPow2 w (toU i) j
        lastRemovedDigit =
            if q /= 0 && (vw - 1) `div` 10 <= vu `div` 10
               then let j = toS q - 1 - (pow5bits (i + 1) - float_pow5_bitcount)
                     in (mulPow5divPow2 v (toU $ i + 1) j) `mod` 10
               else 0
        (vvIsTrailingZeros, vuIsTrailingZeros, vw') =
            case () of
              _ | q <= 1 && acceptBounds v -> (True, w - v == 2, vw) -- mmShift == 1
                | q <= 1                   -> (True, False, vw - 1)
                | q < 31                   -> (multipleOfPowerOf2 v (q - 1), False, vw)
                | otherwise                -> (False, False, vw)
     in (BoundsState v vu vv vw' lastRemovedDigit vuIsTrailingZeros vvIsTrailingZeros, e10)

calculate :: BoundsState -> (Word32, Int32)
calculate d
  = let (d', r) = if d ^. vuIsTrailingZeros
                     then clear' (\d -> d ^. vu `mod` 10 == 0) d
                     else (d, 0)
        d'' = if d' ^. vvIsTrailingZeros && d' ^. lastRemovedDigit == 5 && d' ^. vv `mod` 2 == 0
                 then d' & lastRemovedDigit .~ 4
                 else d'
        roundUp = _vv d'' == _vu d'' && (not (acceptBounds $ _v d'') || not (_vuIsTrailingZeros d''))
                    || _lastRemovedDigit d'' >= 5
     in (_vv d'' + asWord roundUp, r)

f2d :: Word32 -> Word32 -> FloatingDecimal
f2d m e =
    let mf = if e == 0
                then m
                else (1 .<< float_mantissa_bits) .|. m
        ef = if e == 0
                then toS 1 - toS float_bias - toS float_mantissa_bits
                else toS e - toS float_bias - toS float_mantissa_bits
        e2 = ef - 2
        -- Step 2. 3-tuple (u, v, w) * 2**e2
        u = 4 * mf - 1 - asWord (m /= 0 || e <= 1)
        v = 4 * mf
        w = 4 * mf + 2
        -- Step 3. convert to decimal power base
        (state, e10) =
            if e2 >= 0
               then f2dGT e2 u v w
               else f2dLT e2 u v w
        -- Step 4: Find the shortest decimal representation in the interval of
        -- valid representations.
        (state', removed) = clear state
        general = _vvIsTrailingZeros state || _vuIsTrailingZeros state
        reset = if general then id else const False
        (output, removed') = calculate $ state' & (vvIsTrailingZeros %~ reset)
                                                & (vuIsTrailingZeros %~ reset)
        exp = e10 + removed + removed'
     in FloatingDecimal output exp

apply :: (a -> b) -> (a -> a) -> a -> [b]
apply f next x = f x : apply f next (next x)

lastDigitToChar :: (Integral a) => a -> Char
lastDigitToChar = chr . fromIntegral . (+) (fromIntegral $ ord '0') . flip mod 10

prependIf :: Bool -> a -> [a] -> [a]
prependIf True x xs = x : xs
prependIf False _ xs = xs

-- TODO: optimize
toChars :: FloatingDecimal -> String
toChars fd@(FloatingDecimal mantissa exponent) =
    let olength = decimalLength9 mantissa
        serialize :: Int -> Word32 -> String
        serialize l = reverse . foldr (:) "" . take l . apply lastDigitToChar (flip div 10)
        front = serialize olength mantissa
        exp = exponent + fromIntegral olength - 1
        exp' = abs exp
        back = 'E' : (prependIf (exp < 0) '-' (serialize (decimalLength9 exp') . toU $ exp'))
     in if olength > 1
           then head front : '.' : tail front ++ back
           else front ++ back

f2s :: Float -> String
f2s f = let bits = coerceToWord f
            sign = ((bits .>> (float_mantissa_bits + float_exponent_bits)) .&. 1) /= 0
            mantissa = bits .&. mask float_mantissa_bits
            exponent = (bits .>> float_mantissa_bits) .&. mask float_exponent_bits
         in if (exponent == mask float_exponent_bits) || (exponent == 0 && mantissa == 0)
               then special sign (exponent > 0) (mantissa > 0)
               else prependIf sign '-' $ toChars (f2d mantissa exponent)


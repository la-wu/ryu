{-# LANGUAGE Strict #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.Floating.Ryu.F2S
    ( f2s
    , f2sFixed
    , f2sScientific
    , f2sGeneral
    , f2sBuffered
    , f2sScientific'
    , f2sFixed'
    , f2s'
    , f2Intermediate
    , FloatingDecimal(..)
    ) where

import Debug.Trace
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Base (UArray(..), unsafeAt, unsafeRead, unsafeWrite)
import Data.Bits.Floating
import Data.Bits
import Data.Char (ord)
import Data.Floating.Ryu.Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Control.Monad (foldM)
import Control.Monad.ST
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (minusPtr)
import GHC.Int (Int32(..))
import GHC.Word (Word8, Word32(..), Word64(..))
import GHC.Exts
import GHC.Prim
import System.IO.Unsafe (unsafePerformIO)

float_mantissa_bits :: Word32
float_mantissa_bits = 23

float_exponent_bits :: Word32
float_exponent_bits = 8

float_bias :: Word32
float_bias = 127

float_pow5_inv_bitcount = 59
float_pow5_bitcount = 61

float_pow5_inv_split :: UArray Word32 Word64
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

float_pow5_split :: UArray Word32 Word64
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

data FloatingDecimal = FloatingDecimal
    { mantissa :: Word32
    , exponent :: Int32
    } deriving (Show, Eq)

toS :: Word32 -> Int32
toS = fromIntegral

toU :: Int32 -> Word32
toU = fromIntegral

mulShift32Unboxed :: Word# -> Word# -> Int# -> Word#
mulShift32Unboxed m factor shift =
    let factorLo = narrow32Word# factor
        factorHi = factor `uncheckedShiftRL#` 32#
        bits0 = m `timesWord#` factorLo
        bits1 = m `timesWord#` factorHi
        sum = (bits0 `uncheckedShiftRL#` 32#) `plusWord#` bits1
     in narrow32Word# (sum `uncheckedShiftRL#` (shift -# 32#))

get_float_pow5_inv_split :: Int# -> Word#
get_float_pow5_inv_split i =
    let (UArray _ _ _ arr) = float_pow5_inv_split
     in indexWord64Array# arr i

get_float_pow5_split :: Int# -> Word#
get_float_pow5_split i =
    let (UArray _ _ _ arr) = float_pow5_split
     in indexWord64Array# arr i

mulPow5InvDivPow2 :: Word# -> Word# -> Int# -> Word#
mulPow5InvDivPow2 m q j = mulShift32Unboxed m (get_float_pow5_inv_split (word2Int# q)) j

mulPow5DivPow2 :: Word# -> Int# -> Int# -> Word#
mulPow5DivPow2 m i j = mulShift32Unboxed m (get_float_pow5_split i) j

acceptBounds :: Word32 -> Bool
acceptBounds v = v `quot` 4 .&. 1 == 0

data BoundsState = BoundsState
    { vu :: Word32
    , vv :: Word32
    , vw :: Word32
    , lastRemovedDigit :: Word32
    , vuIsTrailingZeros :: Bool
    , vvIsTrailingZeros :: Bool
    }

trimTrailing' :: BoundsState -> (BoundsState, Int32)
trimTrailing' d
  | vw' > vu' =
      let (vv', vvRem) = fquotRem10Boxed $ vv d
       in fmap ((+) 1) . trimTrailing' $
           d { vu = vu'
             , vv = vv'
             , vw = vw'
             , lastRemovedDigit = vvRem
             , vuIsTrailingZeros = vuRem == 0
             , vvIsTrailingZeros = lastRemovedDigit d == 0
             }
  | otherwise = (d, 0)
  where
      (vu', vuRem) = fquotRem10Boxed $ vu d
      vw' = fwrapped fquot10 (vw d)

trimTrailing'' :: BoundsState -> (BoundsState, Int32)
trimTrailing'' d
  | vuRem == 0 =
      let (vv', vvRem) = fquotRem10Boxed $ vv d
          vw' = fwrapped fquot10 (vw d)
       in fmap ((+) 1) . trimTrailing'' $
           d { vu = vu'
             , vv = vv'
             , vw = vw'
             , lastRemovedDigit = vvRem
             , vvIsTrailingZeros = lastRemovedDigit d == 0
             }
  | otherwise = (d, 0)
  where
      (vu', vuRem) = fquotRem10Boxed $ vu d

trimTrailing :: BoundsState -> (BoundsState, Int32)
trimTrailing d
  = let (d', r) = trimTrailing' d
        (d'', r') = if vuIsTrailingZeros d'
                       then trimTrailing'' d'
                       else (d', 0)
        res = if vvIsTrailingZeros d'' && lastRemovedDigit d'' == 5 && vv d'' `rem` 2 == 0
                 then d'' { lastRemovedDigit = 4 }
                 else d''
     in (res, r + r')

trimNoTrailing' :: Word# -> Word# -> Word# -> Word# -> Int# -> (# Word#, Word#, Word#, Int# #)
trimNoTrailing' vu vv vw lastRemovedDigit count =
    case vw' `gtWord#` vu' of
      1# -> let (# vv', ld #) = fquotRem10 vv
             in trimNoTrailing' vu' vv' vw' ld (count +# 1#)
      0# -> (# vu, vv, lastRemovedDigit, count #)
    where
        vu' = fquot10 vu
        vw' = fquot10 vw

trimNoTrailing :: BoundsState -> (BoundsState, Int32)
trimNoTrailing (BoundsState (W32# vu) (W32# vv) (W32# vw) (W32# ld) _ _) =
    let (# vu', vv', ld', c' #) = trimNoTrailing' vu vv vw ld 0#
     in (BoundsState (W32# vu') (W32# vv') 0 (W32# ld') False False, I32# c')

f2dGT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState, Int32)
f2dGT (I32# e2) (W32# u) (W32# v) (W32# w) =
    let q = int2Word# (log10pow2Unboxed e2)
        e10 = word2Int# q
        k = unbox float_pow5_inv_bitcount +# pow5bitsUnboxed (word2Int# q) -# 1#
        i = negateInt# e2 +# word2Int# q +# k
        vu = mulPow5InvDivPow2 u q i
        vv = mulPow5InvDivPow2 v q i
        vw = mulPow5InvDivPow2 w q i
        lastRemovedDigit =
            case (q `neWord#` 0##) `andI#` ((fquot10 (vw `minusWord#` 1##)) `leWord#` fquot10 vu) of
               -- We need to know one removed digit even if we are not going to loop
               -- below. We could use q = X - 1 above, except that would require 33
               -- bits for the result, and we've found that 32-bit arithmetic is
               -- faster even on 64-bit machines.
               1# -> let l = unbox float_pow5_inv_bitcount +# pow5bitsUnboxed (word2Int# q -# 1#) -# 1#
                      in frem10 (mulPow5InvDivPow2 v (q `minusWord#` 1##) (negateInt# e2 +# word2Int# q -# 1# +# l))
               0# -> 0##
        (# vvIsTrailingZeros, vuIsTrailingZeros, vw' #) =
            case () of
              _ | boxToBool ((q `leWord#` 9##) `andI#` (frem5 v `eqWord#` 0##))
                    -> (# multipleOfPowerOf5_UnboxedB v q, False, vw #)
                | boxToBool ((q `leWord#` 9##) `andI#` acceptBoundsUnboxed v)
                    -> (# False, multipleOfPowerOf5_UnboxedB u q, vw #)
                | boxToBool (q `leWord#` 9##)
                    -> (# False, False, vw `minusWord#` int2Word# (multipleOfPowerOf5_Unboxed w q) #)
                | otherwise
                    -> (# False, False, vw #)
     in (BoundsState (W32# vu) (W32# vv) (W32# vw') (W32# lastRemovedDigit) vuIsTrailingZeros vvIsTrailingZeros, (I32# e10))

f2dLT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState, Int32)
f2dLT (I32# e2) (W32# u) (W32# v) (W32# w) =
    let q = int2Word# (log10pow5Unboxed (negateInt# e2))
        e10 = word2Int# q +# e2
        i = (negateInt# e2) -# word2Int# q
        k = pow5bitsUnboxed i -# unbox float_pow5_bitcount
        j = word2Int# q -# k
        vu = mulPow5DivPow2 u i j
        vv = mulPow5DivPow2 v i j
        vw = mulPow5DivPow2 w i j
        lastRemovedDigit =
            case (q `neWord#` 0##) `andI#` ((fquot10 (vw `minusWord#` 1##)) `leWord#` fquot10 vu) of
              1# -> let j = word2Int# q -# 1# -# (pow5bitsUnboxed (i +# 1#) -# unbox float_pow5_bitcount)
                     in frem10 (mulPow5DivPow2 v (i +# 1#) j)
              0# -> 0##
        (# vvIsTrailingZeros, vuIsTrailingZeros, vw' #) =
            case () of
              _ | boxToBool ((q `leWord#` 1##) `andI#` acceptBoundsUnboxed v)
                    -> (# True, boxToBool ((w `minusWord#` v) `eqWord#` 2##), vw #) -- mmShift == 1
                | boxToBool (q `leWord#` 1##)
                    -> (# True, False, vw `minusWord#` 1## #)
                | boxToBool (q `ltWord#` 31##)
                    -> (# boxToBool (multipleOfPowerOf2Unboxed v (q `minusWord#` 1##)), False, vw #)
                | otherwise
                    -> (# False, False, vw #)
     in (BoundsState (W32# vu) (W32# vv) (W32# vw') (W32# lastRemovedDigit) vuIsTrailingZeros vvIsTrailingZeros, (I32# e10))

roundUp :: Bool -> BoundsState -> Bool
roundUp b s = (vv s == vu s && b) || lastRemovedDigit s >= 5

calculate :: Bool -> BoundsState -> Word32
calculate b s = vv s + asWord (roundUp b s)

f2d :: Word32 -> Word32 -> FloatingDecimal
f2d m e =
    let mf = if e == 0
                then m
                else (1 .<< float_mantissa_bits) .|. m
        ef = if e == 0
                then toS 1 - toS (float_bias + float_mantissa_bits)
                else toS e - toS (float_bias + float_mantissa_bits)
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
        (output, removed) =
            if vvIsTrailingZeros state || vuIsTrailingZeros state
               then pmap (\s -> calculate (not (acceptBounds v)
                                        || not (vuIsTrailingZeros s)) s)
                                          $ trimTrailing state
               else pmap (calculate True) $ trimNoTrailing state
        exp = e10 + removed
     in FloatingDecimal output exp

breakdown :: Float -> (Bool, Word32, Word32)
breakdown f = let bits = coerceToWord f
                  sign = ((bits .>> (float_mantissa_bits + float_exponent_bits)) .&. 1) /= 0
                  mantissa = bits .&. mask float_mantissa_bits
                  exponent = (bits .>> float_mantissa_bits) .&. mask float_exponent_bits
                in (sign, mantissa, exponent)

f2Intermediate :: Float -> FloatingDecimal
f2Intermediate f = let (sign, mantissa, exponent) = breakdown f
                    in if (exponent == mask float_exponent_bits) || (exponent == 0 && mantissa == 0)
                          then FloatingDecimal mantissa (toS exponent)
                          else f2d mantissa exponent

{-# INLINE f2s' #-}
f2s' :: (Bool -> Word32 -> Int32 -> a) -> (Bool -> Bool -> Bool -> a) -> Float -> a
f2s' formatter special f =
    let (sign, mantissa, exponent) = breakdown f
     in if (exponent == mask float_exponent_bits) || (exponent == 0 && mantissa == 0)
           then special sign (exponent > 0) (mantissa > 0)
           else let FloatingDecimal m e = f2d mantissa exponent
                 in formatter sign m e

f2sScientific' :: Float -> BS.ByteString
f2sScientific' f = f2s' toCharsScientific (BS.packChars ... special) f

f2sBuffered :: ForeignPtr Word8 -> Float -> IO BS.ByteString
f2sBuffered fp f = f2s' (toCharsBuffered fp) (return ... BS.packChars ... special) f

-- manual long division
largeFloatToChars :: Bool -> Word32 -> Int32 -> BS.ByteString
largeFloatToChars sign mantissa exponent =
    let (d, filled, fs) = loop
        olength = if d >= 1000000000 then 10 else decimalLength9 d
        totalLength = 1 + olength + 9 * filled
     in unsafePerformIO $ do
         fp <- BS.mallocByteString totalLength
         withForeignPtr fp $ \p0 -> do
             p1 <- writeSign p0 sign
             p2 <- appendNDigits p1 d olength
             p3 <- foldM append9Digits p2 (reverse . take filled . elems $ fs)
             return $ BS.PS fp 0 (p3 `minusPtr` p0)
    where
        loop :: (Word32, Int, UArray Int Word32)
        loop = runST $ do
            let maxBits = 24 + exponent
                maxIdx = fromIntegral (maxBits + 31) `quot` 32 - 1
                shift = exponent `rem` 32
            ds <- newArray (0, 3) 0 :: ST s (STUArray s Int Word32)
            if shift <= 8
               then do
                   unsafeWrite ds maxIdx (mantissa .<< shift)
               else do
                   unsafeWrite ds (maxIdx - 1) (mantissa .<< shift)
                   unsafeWrite ds maxIdx (mantissa .>> (32 - shift))
            if maxIdx /= 0
               then do
                   fs <- newArray (0, 3) 0 :: ST s (STUArray s Int Word32)
                   let longDivide 0 iters = return iters
                       longDivide idx iters = do
                           rem <- foldM (longDivide' ds) 0 (reverse [0..idx])
                           unsafeWrite fs iters rem
                           quot <- unsafeRead ds idx
                           longDivide (idx - asWord (quot == 0)) (iters + 1)
                   iters <- longDivide maxIdx 0
                   d <- unsafeRead ds 0
                   t <- freeze fs
                   return (d, iters, t)
               else do
                   -- maxIdx == 0 -> exponent <= 8
                   -- doesn't matter what the 'filled' array is populated with.
                   -- just return someting
                   t <- freeze ds
                   return (mantissa .<< shift, 0, t)

        longDivide' :: STUArray s Int Word32 -> Word32 -> Int -> ST s (Word32)
        longDivide' ds rem idx = do
            d <- unsafeRead ds idx
            let full = (fromIntegral rem .<< 32 :: Word64) .|. fromIntegral d
                qt = fromIntegral $ full `quot` 1000000000
            unsafeWrite ds idx qt
            return $ fromIntegral full - 1000000000 * qt

fixupLargeFixed :: Float -> Maybe BS.ByteString -> BS.ByteString
fixupLargeFixed f bs =
    case bs of
      Just res -> res
      Nothing ->
          let (sign, mantissa, exponent) = breakdown f
              m = (1 .<< float_mantissa_bits) .|. mantissa
              e = toS exponent - toS float_bias - toS float_mantissa_bits
           in largeFloatToChars sign m e

f2sFixed' :: Float -> BS.ByteString
f2sFixed' f = fixupLargeFixed f $ f2s' toCharsFixed (Just ... BS.packChars ... specialFixed) f

f2sGeneral :: Float -> BS.ByteString
f2sGeneral f = fixupLargeFixed f $ f2s' dispatch (Just ... BS.packChars ... specialFixed) f
    where
        dispatch :: Bool -> Word32 -> Int32 -> Maybe BS.ByteString
        dispatch sign m e =
            let olength = decimalLength9 m
                (lower, upper) =
                    if olength == 1
                       -- Value | Fixed   | Scientific
                       -- 1e-3  | "0.001" | "1e-03"
                       -- 1e4   | "10000" | "1e+04"
                       then (-3, 4)
                       -- Value   | Fixed       | Scientific
                       -- 1234e-7 | "0.0001234" | "1.234e-04"
                       -- 1234e5  | "123400000" | "1.234e+08"
                       else (-(olength + 3), 5)
             in if lower <= fromIntegral e && fromIntegral e <= upper
                   then toCharsFixed sign m e
                   else Just $ toCharsScientific sign m e

f2sScientific :: Float -> String
f2sScientific = BS.unpackChars . f2sScientific'

f2sFixed :: Float -> String
f2sFixed = BS.unpackChars . f2sFixed'

f2s :: Float -> String
f2s = BS.unpackChars . f2sGeneral


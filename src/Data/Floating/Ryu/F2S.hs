{-# LANGUAGE Strict #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.Floating.Ryu.F2S
    ( f2s
    , f2sFixed
    , f2sScientific
    , f2sGeneral
    , f2sScientific'
    , f2sFixed'
    , f2s'
    , f2Intermediate
    , FloatingDecimal(..)
    ) where

import Debug.Trace
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.Base (UArray(..), unsafeAt, unsafeRead, unsafeWrite)
import Data.Bits.Floating
import Data.Bits
import Data.Char (ord)
import Data.Floating.Ryu.Common
import Data.Floating.Ryu.F2STable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Internal as BS
import Control.Monad (foldM)
import Control.Monad.ST
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (minusPtr)
import GHC.Int (Int32(..))
import GHC.Word (Word8, Word32(..), Word64(..))
import GHC.Exts
import GHC.Prim

float_mantissa_bits :: Word32
float_mantissa_bits = 23

float_exponent_bits :: Word32
float_exponent_bits = 8

float_bias :: Word32
float_bias = 127

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

f2sScientific' :: Float -> BB.Builder
f2sScientific' f = BP.primBounded (f2s' toCharsScientific special f) ()

-- manual long division
largeFloatToChars :: Bool -> Word32 -> Int32 -> BB.Builder
largeFloatToChars sign mantissa exponent =
    let toBS = BB.toLazyByteStringWith (BB.safeStrategy 128 BB.smallChunkSize) BL.empty
        signB = if sign then BB.char7 '-' else mempty
     in signB <> BB.integerDec (toInteger mantissa .<< exponent)

fixupLargeFixed :: Float -> Maybe (BP.BoundedPrim ()) -> BB.Builder
fixupLargeFixed f bs =
    case bs of
      Just res -> BP.primBounded res ()
      Nothing ->
          let (sign, mantissa, exponent) = breakdown f
              m = (1 .<< float_mantissa_bits) .|. mantissa
              e = toS exponent - toS float_bias - toS float_mantissa_bits
           in largeFloatToChars sign m e

f2sFixed' :: Float -> BB.Builder
f2sFixed' f = fixupLargeFixed f $ f2s' toCharsFixed (Just ... specialFixed) f

f2sGeneral :: Float -> BB.Builder
f2sGeneral f = fixupLargeFixed f $ f2s' dispatch (Just ... specialFixed) f
    where
        dispatch :: Bool -> Word32 -> Int32 -> Maybe (BP.BoundedPrim ())
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
f2sScientific = BLC.unpack . BB.toLazyByteString . f2sScientific'

f2sFixed :: Float -> String
f2sFixed = BLC.unpack . BB.toLazyByteString . f2sFixed'

f2s :: Float -> String
f2s = BLC.unpack . BB.toLazyByteString . f2sGeneral


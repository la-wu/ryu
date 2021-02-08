{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.Floating.Ryu.Common
    ( (.>>)
    , (.<<)
    , mask
    , asWord
    , pmap
    , dot
    , (...)
    , special
    , specialFixed
    , decimalLength9
    , decimalLength17
    , pow5bitsUnboxed
    , log10pow2Unboxed
    , log10pow5Unboxed
    , multipleOfPowerOf5_Unboxed
    , multipleOfPowerOf5_UnboxedB
    , multipleOfPowerOf2Unboxed
    , acceptBoundsUnboxed
    , writeSign
    , appendNDigits
    , append9Digits
    , toCharsScientific
    , toCharsFixed
    -- hand-rolled division and remainder for f2s and d2s
    , fquot10
    , frem10
    , fquotRem10
    , fquotRem10Boxed
    , fquot5
    , frem5
    , fquotRem5
    , fwrapped
    , dquot10
    , drem10
    , dquotRem10
    , dquotRem10Boxed
    , dquot5
    , drem5
    , dquotRem5
    , dquot100
    , dwrapped
    -- prim-op helpers
    , boxToBool
    , box
    , unbox
    ) where

import Control.Monad (foldM)
import Data.Array.Unboxed
import Data.Array.Base (unsafeAt)
import Data.Bits
import Data.Char (chr, ord)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE
import qualified Data.ByteString.Builder.Prim.Internal as BP
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Int (Int(..), Int32)
import GHC.Word (Word8, Word16, Word32(..), Word64(..))
import GHC.Prim
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (moveBytes)
import Foreign.Ptr (Ptr, minusPtr, plusPtr, castPtr)
import Foreign.Storable (poke)
import System.IO.Unsafe (unsafeDupablePerformIO)

{-# INLINABLE (.>>) #-}
(.>>) :: (Bits a, Integral b) => a -> b -> a
a .>> s = unsafeShiftR a (fromIntegral s)

{-# INLINABLE (.<<) #-}
(.<<) :: (Bits a, Integral b) => a -> b -> a
a .<< s = unsafeShiftL a (fromIntegral s)

{-# INLINABLE mask #-}
mask :: (Bits a, Integral a) => a -> a
mask = flip (-) 1 . (.<<) 1

{-# INLINABLE asWord #-}
asWord :: Integral w => Bool -> w
asWord = fromIntegral . fromEnum

pmap :: (a -> c) -> (a, b) -> (c, b)
pmap f (a, b) = (f a, b)

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

infixr 9 `dot`
dot :: (a -> b) -> (c -> d -> a) -> c -> d -> b
dot = (.) . (.)

infixr 9 ...
(...) :: (a -> b) -> (c -> d -> e -> a) -> c -> d -> e -> b
(...) = dot . (.)

-- TODO calculate / prove?
maxEncodedLength = 32

-- TODO TH?
pokeAll :: String -> Ptr Word8 -> IO (Ptr Word8)
pokeAll s ptr = foldM pokeOne ptr s
  where pokeOne ptr c = poke ptr (BS.c2w c) >> return (ptr `plusPtr` 1)

boundString :: String -> BP.BoundedPrim ()
boundString s = BP.boudedPrim maxEncodedLength $ const (pokeAll s)

--         Sign -> Exp  -> Mantissa
special :: Bool -> Bool -> Bool -> BP.BoundedPrim ()
special    _       _       True =  boundString "NaN"
special    True    False   _    =  boundString "-0E0"
special    False   False   _    =  boundString "0E0"
special    True    True    _    =  boundString "-Infinity"
special    False   True    _    =  boundString "Infinity"

-- same as above but for fixed-point / general conversion
specialFixed :: Bool -> Bool -> Bool -> BP.BoundedPrim ()
specialFixed    _       _       True =  boundString "NaN"
specialFixed    True    False   _    =  boundString "-0"
specialFixed    False   False   _    =  boundString "0"
specialFixed    True    True    _    =  boundString "-Infinity"
specialFixed    False   True    _    =  boundString "Infinity"

-- Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 3528.
pow5bitsUnboxed :: Int# -> Int#
pow5bitsUnboxed e = (e *# 1217359#) `uncheckedIShiftRL#` 19# +# 1#

-- Returns floor(log_10(2^e)); requires 0 <= e <= 1650.
log10pow2Unboxed :: Int# -> Int#
log10pow2Unboxed e = (e *# 78913#) `uncheckedIShiftRL#` 18#

-- Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
log10pow5Unboxed :: Int# -> Int#
log10pow5Unboxed e = (e *# 732928#) `uncheckedIShiftRL#` 20#

acceptBoundsUnboxed :: Word# -> Int#
acceptBoundsUnboxed v = ((v `uncheckedShiftRL#` 2#) `and#` 1##) `eqWord#` 0##

fquot10 :: Word# -> Word#
fquot10 w = (w `timesWord#` 0xCCCCCCCD##) `uncheckedShiftRL#` 35#

frem10 :: Word# -> Word#
frem10 w = let w' = fquot10 w
           in w `minusWord#` (w' `timesWord#` 10##)

fquotRem10 :: Word# -> (# Word#, Word# #)
fquotRem10 w = let w' = fquot10 w
               in (# w', w `minusWord#` (w' `timesWord#` 10##) #)

fquot5 :: Word# -> Word#
fquot5 w = (w `timesWord#` 0xCCCCCCCD##) `uncheckedShiftRL#` 34#

frem5 :: Word# -> Word#
frem5 w = let w' = fquot5 w
          in w `minusWord#` (w' `timesWord#` 5##)

fquotRem5 :: Word# -> (# Word#, Word# #)
fquotRem5 w = let w' = fquot5 w
              in (# w', w `minusWord#` (w' `timesWord#` 5##) #)

fquotRem10Boxed :: Word32 -> (Word32, Word32)
fquotRem10Boxed (W32# w) = let (# q, r #) = fquotRem10 w in (W32# q, W32# r)

fwrapped :: (Word# -> Word#) -> Word32 -> Word32
fwrapped f (W32# w) = W32# (f w)

dquot10 :: Word# -> Word#
dquot10 w
  = let (# rdx, rax #) = w `timesWord2#` 0xCCCCCCCCCCCCCCCD##
     in rdx `uncheckedShiftRL#` 3#

dquot100 :: Word# -> Word#
dquot100 w
  = let (# rdx, rax #) = (w `uncheckedShiftRL#` 2#) `timesWord2#` 0x28F5C28F5C28F5C3##
     in rdx `uncheckedShiftRL#` 2#

drem10 :: Word# -> Word#
drem10 w = let w' = dquot10 w
           in w `minusWord#` (w' `timesWord#` 10##)

dquotRem10 :: Word# -> (# Word#, Word# #)
dquotRem10 w = let w' = dquot10 w
               in (# w', w `minusWord#` (w' `timesWord#` 10##) #)

dquot5 :: Word# -> Word#
dquot5 w = let (# rdx, rax #) = w `timesWord2#` 0xCCCCCCCCCCCCCCCD##
             in rdx `uncheckedShiftRL#` 2#

drem5 :: Word# -> Word#
drem5 w = let w' = dquot5 w
          in w `minusWord#` (w' `timesWord#` 5##)

dquotRem5 :: Word# -> (# Word#, Word# #)
dquotRem5 w = let w' = dquot5 w
              in (# w', w `minusWord#` (w' `timesWord#` 5##) #)

dquotRem10Boxed :: Word64 -> (Word64, Word64)
dquotRem10Boxed (W64# w) = let (# q, r #) = dquotRem10 w in (W64# q, W64# r)

dwrapped :: (Word# -> Word#) -> Word64 -> Word64
dwrapped f (W64# w) = W64# (f w)

boxToBool :: Int# -> Bool
boxToBool i = case i of
                1# -> True
                0# -> False

box :: Int# -> Int
box i = I# i

unbox :: Int -> Int#
unbox (I# i) = i

pow5_factor :: Word# -> Int# -> Int#
pow5_factor w count
  = let (# q, r #) = fquotRem5 w
     in case r `eqWord#` 0## of
          0# -> count
          1# -> pow5_factor q (count +# 1#)

multipleOfPowerOf5_Unboxed :: Word# -> Word# -> Int#
multipleOfPowerOf5_Unboxed value p = pow5_factor value 0# >=# word2Int# p

multipleOfPowerOf5_UnboxedB :: Word# -> Word# -> Bool
multipleOfPowerOf5_UnboxedB value p = boxToBool (multipleOfPowerOf5_Unboxed value p)

multipleOfPowerOf2Unboxed :: Word# -> Word# -> Int#
multipleOfPowerOf2Unboxed value p = (value `and#` ((1## `uncheckedShiftL#` word2Int# p) `minusWord#` 1##)) `eqWord#` 0##

class (IArray UArray a, FiniteBits a, Integral a) => Mantissa a where
    decimalLength :: a -> Int
    max_representable_pow10 :: a -> Int32
    max_shifted_mantissa :: UArray Int32 a
    quotRem100 :: a -> (a, a)
    quotRem10000 :: a -> (a, a)

instance Mantissa Word32 where
    decimalLength = decimalLength9
    max_representable_pow10 = const 10
    max_shifted_mantissa = listArray (0, 10) [ (2^24 - 1) `quot` 5^x | x <- [0..10] ]
    quotRem100 (W32# w)
      = let w' = (w `timesWord#` 0x51EB851F##) `uncheckedShiftRL#` 37#
         in (W32# w', W32# (w `minusWord#` (w' `timesWord#` 100##)))
    quotRem10000 (W32# w)
      = let w' = (w `timesWord#` 0xD1B71759##) `uncheckedShiftRL#` 45#
         in (W32# w', W32# (w `minusWord#` (w' `timesWord#` 10000##)))

instance Mantissa Word64 where
    decimalLength = decimalLength17
    max_representable_pow10 = const 22
    max_shifted_mantissa = listArray (0, 22) [ (2^53- 1) `quot` 5^x | x <- [0..22] ]
    quotRem100 (W64# w)
      = let w' = dquot100 w
         in (W64# w', W64# (w `minusWord#` (w' `timesWord#` 100##)))
    quotRem10000 (W64# w)
      = let (# rdx, rax #) = w `timesWord2#` 0x346DC5D63886594B##
            w' = rdx `uncheckedShiftRL#` 11#
         in (W64# w', W64# (w `minusWord#` (w' `timesWord#` 10000##)))

type DigitStore = Word16

toAscii :: (Integral a, Integral b) => a -> b
toAscii = fromIntegral . (+) (fromIntegral $ ord '0')

digit_table :: UArray Int32 DigitStore
digit_table = listArray (0, 99) [ (toAscii b .<< 8) .|. toAscii a | a <- [0..9], b <- [0..9] ]

copy :: DigitStore -> Ptr Word8 -> IO ()
copy d p = poke (castPtr p) d

first :: DigitStore -> Word8
first = fromIntegral . flip (.>>) 8

second :: DigitStore -> Word8
second = fromIntegral

-- for loop recursively...
{-# SPECIALIZE writeMantissa :: Ptr Word8 -> Int -> Word32 -> IO (Ptr Word8) #-}
{-# SPECIALIZE writeMantissa :: Ptr Word8 -> Int -> Word64 -> IO (Ptr Word8) #-}
writeMantissa :: (Mantissa a) => Ptr Word8 -> Int -> a -> IO (Ptr Word8)
writeMantissa ptr olength = go (ptr `plusPtr` olength)
    where
        go p mantissa
          | mantissa >= 10000 = do
              let (m', c) = quotRem10000 mantissa
                  (c1, c0) = quotRem100 c
              copy (digit_table `unsafeAt` fromIntegral c0) (p `plusPtr` (-1))
              copy (digit_table `unsafeAt` fromIntegral c1) (p `plusPtr` (-3))
              go (p `plusPtr` (-4)) m'
          | mantissa >= 100 = do
              let (m', c) = quotRem100 mantissa
              copy (digit_table `unsafeAt` fromIntegral c) (p `plusPtr` (-1))
              finalize m'
          | otherwise = finalize mantissa
        finalize mantissa
          | mantissa >= 10 = do
              let bs = digit_table `unsafeAt` fromIntegral mantissa
              poke (ptr `plusPtr` 2) (first bs)
              poke (ptr `plusPtr` 1) (BS.c2w '.')
              poke ptr (second bs)
              return (ptr `plusPtr` (olength + 1))
          | otherwise = do
              copy ((fromIntegral (BS.c2w '.') .<< 8) .|. toAscii mantissa) ptr
              return $ ptr `plusPtr` if olength > 1 then (olength + 1) else 1

writeExponent :: Ptr Word8 -> Int32 -> IO (Ptr Word8)
writeExponent ptr exponent
  | exponent >= 100 = do
      let (e1, e0) = fquotRem10Boxed (fromIntegral exponent)
      copy (digit_table `unsafeAt` fromIntegral e1) ptr
      poke (ptr `plusPtr` 2) (toAscii e0 :: Word8)
      return $ ptr `plusPtr` 3
  | exponent >= 10 = do
      copy (digit_table `unsafeAt` fromIntegral exponent) ptr
      return $ ptr `plusPtr` 2
  | otherwise = do
      poke ptr (toAscii exponent)
      return $ ptr `plusPtr` 1

writeSign :: Ptr Word8 -> Bool -> IO (Ptr Word8)
writeSign ptr True = do
    poke ptr (BS.c2w '-')
    return $ ptr `plusPtr` 1
writeSign ptr False = return ptr

{-# INLINABLE toCharsScientific #-}
{-# SPECIALIZE toCharsScientific :: Bool -> Word32 -> Int32 -> BP.BoundedPrim () #-}
{-# SPECIALIZE toCharsScientific :: Bool -> Word64 -> Int32 -> BP.BoundedPrim () #-}
toCharsScientific :: (Mantissa a) => Bool -> a -> Int32 -> BP.BoundedPrim ()
toCharsScientific sign mantissa exponent = BP.boudedPrim maxEncodedLength $ \_ p0 -> do
    let olength = decimalLength mantissa
        exp = exponent + fromIntegral olength - 1
    p1 <- writeSign p0 sign
    p2 <- writeMantissa p1 olength mantissa
    poke p2 (BS.c2w 'E')
    p3 <- writeSign (p2 `plusPtr` 1) (exp < 0)
    writeExponent p3 (abs exp)


--
-- fixed implementation derived from MSVC STL
--

{-# INLINABLE trimmedDigits #-}
{-# SPECIALIZE trimmedDigits :: Word32 -> Int32 -> Bool #-}
{-# SPECIALIZE trimmedDigits :: Word64 -> Int32 -> Bool #-}
trimmedDigits :: (Mantissa a) => a -> Int32 -> Bool
trimmedDigits mantissa exponent =
    -- Ryu generated X: mantissa * 10^exponent
    -- mantissa == 2^zeros* (mantissa >> zeros)
    -- 10^exponent == 2^exponent * 5^exponent

    -- for float
    -- zeros is [0, 29] (aside: because 2^29 is the largest power of 2
    --                   with 9 decimal digits, which is float's round-trip
    --                   limit.)
    -- exponent is [1, 10].
    -- Normalization adds [2, 23] (aside: at least 2 because the pre-normalized
    --                             mantissa is at least 5).
    -- This adds up to [3, 62], which is well below float's maximum binary
    -- exponent 127
    --
    -- for double
    -- zeros is [0, 56]
    -- exponent is [1, 22].
    -- Normalization adds [2, 52]
    -- This adds up to [3, 130], which is well below double's maximum binary
    -- exponent 1023
    --
    -- In either case, the pow-2 part is entirely encodeable in the exponent bits

    -- Therefore, we just need to consider (mantissa >> zeros) * 5^exponent.

    -- If that product would exceed 24 (53) bits, then X can't be exactly
    -- represented as a float.  (That's not a problem for round-tripping,
    -- because X is close enough to the original float, but X isn't
    -- mathematically equal to the original float.) This requires a
    -- high-precision fallback.
    let zeros = countTrailingZeros mantissa
        shiftMantissa = mantissa .>> zeros
     in shiftMantissa > max_shifted_mantissa `unsafeAt` fromIntegral exponent

{-# INLINABLE writeRightAligned #-}
{-# SPECIALIZE writeRightAligned :: Ptr Word8 -> Word32 -> IO () #-}
{-# SPECIALIZE writeRightAligned :: Ptr Word8 -> Word64 -> IO () #-}
writeRightAligned :: (Mantissa a) => Ptr Word8 -> a -> IO ()
writeRightAligned ptr v
  | v >= 10000 = do
      let (v', c) = quotRem10000 v
          (c1, c0) = quotRem100 c
      copy (digit_table `unsafeAt` fromIntegral c0) (ptr `plusPtr` (-2))
      copy (digit_table `unsafeAt` fromIntegral c1) (ptr `plusPtr` (-4))
      writeRightAligned (ptr `plusPtr` (-4)) v'
  | v >= 100 = do
      let (v', c) = quotRem100 v
      copy (digit_table `unsafeAt` fromIntegral c) (ptr `plusPtr` (-2))
      writeRightAligned (ptr `plusPtr` (-2)) v'
  | v >= 10 = do
      copy (digit_table `unsafeAt` fromIntegral v) (ptr `plusPtr` (-2))
  | otherwise = do
      poke (ptr `plusPtr` (-1)) (toAscii v :: Word8)

appendNDigits :: Ptr Word8 -> Word32 -> Int -> IO (Ptr Word8)
appendNDigits ptr w n = do
    let end = ptr `plusPtr` n
    writeRightAligned end w
    return end

-- TODO: handroll hardcoded write?
append9Digits :: Ptr Word8 -> Word32 -> IO (Ptr Word8)
append9Digits ptr w = do
    BS.memset ptr (BS.c2w '0') 9
    appendNDigits ptr w 9

-- exponent| Printed  | wholeDigits | totalLength          | Notes
-- --------|----------|-------------|----------------------|---------------------------------------
--       2 | 172900   |  6          | wholeDigits          | Ryu can't be used for printing
--       1 | 17290    |  5          | (sometimes adjusted) | when the trimmed digits are nonzero.
-- --------|----------|-------------|----------------------|---------------------------------------
--       0 | 1729     |  4          | wholeDigits          | Unified length cases.
-- --------|----------|-------------|----------------------|---------------------------------------
--      -1 | 172.9    |  3          | olength + 1          | This case can't happen for
--      -2 | 17.29    |  2          |                      | olength == 1, but no additional
--      -3 | 1.729    |  1          |                      | code is needed to avoid it.
-- --------|----------|-------------|----------------------|---------------------------------------
--      -4 | 0.1729   |  0          | 2 - exponent         | Print at least one digit before
--      -5 | 0.01729  | -1          |                      | decimal
--      -6 | 0.001729 | -2          |                      |
--
-- returns Nothing when we can't represent through ryu. need to fall back to a
-- higher precision method that is dependent on the original (float / double)
-- input value and type
{-# INLINABLE toCharsFixed #-}
{-# SPECIALIZE toCharsFixed :: Bool -> Word32 -> Int32 -> Maybe (BP.BoundedPrim ()) #-}
{-# SPECIALIZE toCharsFixed :: Bool -> Word64 -> Int32 -> Maybe (BP.BoundedPrim ()) #-}
toCharsFixed :: (Show a, Mantissa a) => Bool -> a -> Int32 -> Maybe (BP.BoundedPrim ())
toCharsFixed sign mantissa exponent =
    if exponent >= 0
       then
           if exponent > max_representable_pow10 mantissa || trimmedDigits mantissa exponent
              then Nothing -- large integer
              else Just $ wrap case1
       else do
           if wholeDigits > 0
              then Just $ wrap case2
              else Just $ wrap case3
    where
        olength = decimalLength mantissa
        wholeDigits = fromIntegral olength + exponent

        wrap :: (Ptr Word8 -> IO (Ptr Word8)) -> BP.BoundedPrim ()
        wrap f = BP.boudedPrim maxEncodedLength $ const f

        case1 :: Ptr Word8 -> IO (Ptr Word8)
        case1 p0 = do
            -- case 172900 .. 1729
            let totalLength = wholeDigits
            p1 <- writeSign p0 sign
            let p2 = p1 `plusPtr` olength
            writeRightAligned p2 mantissa
            BS.memset p2 (BS.c2w '0') (fromIntegral exponent)
            return $ p0 `plusPtr` (fromIntegral totalLength + asWord sign)

        case2 p0 = do
            -- case 17.29
            let totalLength = fromIntegral olength + 1
            p1 <- writeSign p0 sign
            writeRightAligned (p1 `plusPtr` fromIntegral totalLength) mantissa
            moveBytes p1 (p1 `plusPtr` 1) (fromIntegral wholeDigits)
            poke (p1 `plusPtr` fromIntegral wholeDigits) (BS.c2w '.')
            return $ p0 `plusPtr` (fromIntegral totalLength + asWord sign)

        case3 p0 = do
            -- case 0.001729
            let totalLength = 2 - exponent
            p1 <- writeSign p0 sign
            writeRightAligned (p1 `plusPtr` fromIntegral totalLength) mantissa
            BS.memset p1 (BS.c2w '0') (fromIntegral (-wholeDigits) + 2)
            poke (p1 `plusPtr` 1) (BS.c2w '.')
            return $ p0 `plusPtr` (fromIntegral totalLength + asWord sign)


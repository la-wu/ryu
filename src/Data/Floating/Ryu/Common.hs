{-# LANGUAGE Strict, StrictData #-}
{-# LANGUAGE FlexibleContexts #-}

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
    , pow5bits
    , log10pow2
    , log10pow5
    , pow5_32
    , multipleOfPowerOf5_32
    , multipleOfPowerOf5_64
    , multipleOfPowerOf2
    , writeSign
    , appendNDigits
    , append9Digits
    , toCharsScientific
    , toCharsBuffered
    , toCharsFixed
    , toChars
    ) where

import Data.Array.Unboxed
import Data.Array.Base (unsafeAt)
import Data.Bits
import Data.Char (chr, ord)
import Data.Int (Int32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (moveBytes)
import Foreign.Ptr (Ptr, minusPtr, plusPtr, castPtr)
import Foreign.Storable (poke)
import System.IO.Unsafe (unsafePerformIO)

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

--         Sign -> Exp  -> Mantissa
special :: Bool -> Bool -> Bool   ->   String
special    _       _       True   =    "NaN"
special    True    False   _      =    "-0E0"
special    False   False   _      =    "0E0"
special    True    True    _      =    "-Infinity"
special    False   True    _      =    "Infinity"

-- same as above but for fixed-point / general conversion
specialFixed :: Bool -> Bool -> Bool   ->   String
specialFixed    _       _       True   =    "NaN"
specialFixed    True    False   _      =    "-0"
specialFixed    False   False   _      =    "0"
specialFixed    True    True    _      =    "-Infinity"
specialFixed    False   True    _      =    "Infinity"


-- Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 3528.
{-# INLINABLE pow5bits #-}
{-# SPECIALIZE pow5bits :: Int32 -> Int32 #-}
pow5bits :: (Bits a, Integral a) => a -> a
pow5bits e = (e * 1217359) .>> 19 + 1

-- Returns floor(log_10(2^e)); requires 0 <= e <= 1650.
{-# INLINABLE log10pow2 #-}
{-# SPECIALIZE log10pow2 :: Int32 -> Int32 #-}
log10pow2 :: (Bits a, Integral a) => a -> a
log10pow2 e = (e * 78913) .>> 18

-- Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
{-# INLINABLE log10pow5 #-}
{-# SPECIALIZE log10pow5 :: Int32 -> Int32 #-}
log10pow5 :: (Bits a, Integral a) => a -> a
log10pow5 e = (e * 732928) .>> 20

pow5_32 :: UArray Int Word32
pow5_32 = listArray (0, 9) [5 ^ x | x <- [0..9]]

pow5_64 :: UArray Int Word64
pow5_64 = listArray (0, 21) [5 ^ x | x <- [0..21]]

multipleOfPowerOf5_32 :: Word32 -> Word32 -> Bool
multipleOfPowerOf5_32 value p = value `mod` (pow5_32 `unsafeAt` fromIntegral p) == 0

multipleOfPowerOf5_64 :: Word64 -> Word64 -> Bool
multipleOfPowerOf5_64 value p = value `mod` (pow5_64 `unsafeAt` fromIntegral p) == 0

multipleOfPowerOf2 :: (Bits a, Integral a) => a -> a -> Bool
multipleOfPowerOf2 value p = value .&. mask p == 0

toAscii :: (Integral a, Integral b) => a -> b
toAscii = fromIntegral . (+) (fromIntegral $ ord '0')

class (IArray UArray a, FiniteBits a, Integral a) => Mantissa a where
    decimalLength :: a -> Int
    max_representable_pow10 :: a -> Int32
    max_shifted_mantissa :: UArray Int32 a

instance Mantissa Word32 where
    decimalLength = decimalLength9
    max_representable_pow10 = const 10
    max_shifted_mantissa = listArray (0, 10) [ (2^24 - 1) `quot` 5^x | x <- [0..10] ]

instance Mantissa Word64 where
    decimalLength = decimalLength17
    max_representable_pow10 = const 22
    max_shifted_mantissa = listArray (0, 22) [ (2^53- 1) `quot` 5^x | x <- [0..22] ]

type DigitStore = Word16

digit_table :: UArray Int32 DigitStore
digit_table = listArray (0, 99) [ (toAscii b .<< 8) .|. toAscii a | a <- [0..9], b <- [0..9] ]

copy :: DigitStore -> Ptr Word8 -> IO ()
copy d p = poke (castPtr p) d

first :: DigitStore -> Word8
first = fromIntegral . flip (.>>) 8

second :: DigitStore -> Word8
second = fromIntegral

-- for loop recursively...
{-# SPECIALIZE writeMantissa :: Ptr Word8 -> Int -> Int -> Word32 -> IO (Ptr Word8) #-}
{-# SPECIALIZE writeMantissa :: Ptr Word8 -> Int -> Int -> Word64 -> IO (Ptr Word8) #-}
writeMantissa :: (Integral a) => Ptr Word8 -> Int -> Int -> a -> IO (Ptr Word8)
writeMantissa ptr olength i mantissa
  | mantissa >= 10000 = do
      let (m', c) = mantissa `quotRem` 10000
          (c1, c0) = c `quotRem` 100
      copy (digit_table `unsafeAt` fromIntegral c0) (ptr `plusPtr` (olength - i - 1))
      copy (digit_table `unsafeAt` fromIntegral c1) (ptr `plusPtr` (olength - i - 3))
      writeMantissa ptr olength (i + 4) m'
  | mantissa >= 100 = do
      let (m', c) = mantissa `quotRem` 100
      copy (digit_table `unsafeAt` fromIntegral c) (ptr `plusPtr` (olength - i - 1))
      writeMantissa ptr olength (i + 2) m'
  | mantissa >= 10 = do
      let bs = digit_table `unsafeAt` fromIntegral mantissa
      poke (ptr `plusPtr` 2) (first bs)
      poke (ptr `plusPtr` 1) (BS.c2w '.')
      poke ptr (second bs)
      return (ptr `plusPtr` (olength + 1))
  | otherwise = do
      poke ptr (toAscii mantissa)
      -- returning a truncated length if olength == 1 means we can always poke
      -- the ptr here. might be faster to wait for the branch
      poke (ptr `plusPtr` 1) (BS.c2w '.')
      return $ ptr `plusPtr` if olength > 1 then (olength + 1) else 1

writeExponent :: Ptr Word8 -> Int32 -> IO (Ptr Word8)
writeExponent ptr exponent
  | exponent >= 100 = do
      let (e1, e0) = exponent `quotRem` 10
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
{-# SPECIALIZE toCharsScientific :: Bool -> Word32 -> Int32 -> BS.ByteString #-}
{-# SPECIALIZE toCharsScientific :: Bool -> Word64 -> Int32 -> BS.ByteString #-}
toCharsScientific :: (Mantissa a) => Bool -> a -> Int32 -> BS.ByteString
toCharsScientific sign mantissa exponent = unsafePerformIO $ do
    fp <- BS.mallocByteString 32 :: IO (ForeignPtr Word8)
    toCharsBuffered fp sign mantissa exponent

{-# INLINABLE toCharsBuffered #-}
{-# SPECIALIZE toCharsBuffered :: ForeignPtr Word8 -> Bool -> Word32 -> Int32 -> IO BS.ByteString #-}
{-# SPECIALIZE toCharsBuffered :: ForeignPtr Word8 -> Bool -> Word64 -> Int32 -> IO BS.ByteString #-}
toCharsBuffered :: (Mantissa a) => ForeignPtr Word8 -> Bool -> a -> Int32 -> IO BS.ByteString
toCharsBuffered fp sign mantissa exponent =
    withForeignPtr fp $ \p0 -> do
        let olength = decimalLength mantissa
        p1 <- writeSign p0 sign
        p2 <- writeMantissa p1 olength 0 mantissa
        poke p2 (BS.c2w 'E')
        let exp = exponent + fromIntegral olength - 1
            pe = p2 `plusPtr` 1
        end <- if exp < 0
                  then poke pe (BS.c2w '-') >> writeExponent (pe `plusPtr` 1) (-exp)
                  else writeExponent pe exp
        return $ BS.PS fp 0 (end `minusPtr` p0)


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
      let (v', c) = v `quotRem` 10000
          (c1, c0) = c `quotRem` 100
      copy (digit_table `unsafeAt` fromIntegral c0) (ptr `plusPtr` (-2))
      copy (digit_table `unsafeAt` fromIntegral c1) (ptr `plusPtr` (-4))
      writeRightAligned (ptr `plusPtr` (-4)) v'
  | v >= 100 = do
      let (v', c) = v `quotRem` 100
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
{-# SPECIALIZE toCharsFixed :: Bool -> Word32 -> Int32 -> Maybe BS.ByteString #-}
{-# SPECIALIZE toCharsFixed :: Bool -> Word64 -> Int32 -> Maybe BS.ByteString #-}
toCharsFixed :: (Show a, Mantissa a) => Bool -> a -> Int32 -> Maybe BS.ByteString
toCharsFixed sign mantissa exponent = unsafePerformIO $ do
    fp <- BS.mallocByteString 32 :: IO (ForeignPtr Word8)
    let olength = decimalLength mantissa
        wholeDigits = fromIntegral olength + exponent
        totalLength = case () of
                        _ | exponent >= 0   -> wholeDigits
                          | wholeDigits > 0 -> fromIntegral olength + 1
                          | otherwise       -> 2 - exponent
        finalize = Just $ BS.PS fp 0 (fromIntegral totalLength + asWord sign)
    withForeignPtr fp $ \p0 -> do
        p1 <- writeSign p0 sign
        if exponent >= 0
           then
               if exponent > max_representable_pow10 mantissa || trimmedDigits mantissa exponent
                  then return Nothing -- large integer
                  else do
                      -- case 172900 .. 1729
                      let p2 = p1 `plusPtr` olength
                      writeRightAligned p2 mantissa
                      BS.memset p2 (BS.c2w '0') (fromIntegral exponent)
                      return finalize
           else do
               writeRightAligned (p1 `plusPtr` fromIntegral totalLength) mantissa
               if wholeDigits > 0
                  then do
                      -- case 17.29
                      moveBytes p1 (p1 `plusPtr` 1) (fromIntegral wholeDigits)
                      poke (p1 `plusPtr` fromIntegral wholeDigits) (BS.c2w '.')
                      return finalize
                  else do
                      -- case 0.001729
                      BS.memset p1 (BS.c2w '0') (fromIntegral (-wholeDigits) + 2)
                      poke (p1 `plusPtr` 1) (BS.c2w '.')
                      return finalize

{-# INLINABLE toChars #-}
{-# SPECIALIZE toChars :: Bool -> Word32 -> Int32 -> String #-}
{-# SPECIALIZE toChars :: Bool -> Word64 -> Int32 -> String #-}
toChars :: (Mantissa a) => Bool -> a -> Int32 -> String
toChars s m = BS.unpackChars . toCharsScientific s m


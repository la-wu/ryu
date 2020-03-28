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
    , multipleOfPowerOf5_32
    , multipleOfPowerOf5_64
    , multipleOfPowerOf2
    , toChars
    , toCharsBS
    , toChars'
    , prependIf
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
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (poke)
import System.IO.Unsafe (unsafePerformIO)

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
toChars :: (Mantissa a) => Bool -> a -> Int32 -> String
toChars sign mantissa exponent =
    let olength = decimalLength mantissa
        signB = if sign then BB.char7 '-' else mempty
        (front, sig) = forDigitsN lastDigitToBuilder mempty (olength - 1) mantissa
        exp = exponent + fromIntegral olength - 1
        back = BB.char7 'E' <> if exp < 0
                                  then BB.char7 '-' <> printExp (-exp)
                                  else printExp exp
     in BL.unpack . BBE.toLazyByteStringWith (BBE.untrimmedStrategy 32 BBE.smallChunkSize) BL.empty $ if olength > 1
           then signB <> asBuilder sig <> BB.char7 '.' <> front <> back
           else signB <> asBuilder sig <> back

digit_table :: Array Int32 BS.ByteString
digit_table = listArray (0, 99) [ BS.packBytes [toAscii a, toAscii b] | a <- [0..9], b <- [0..9] ]

copy :: BS.ByteString -> Ptr Word8 -> IO ()
copy (BS.PS fp off len) ptr =
  withForeignPtr fp $ \src -> do
    BS.memcpy ptr (src `plusPtr` off) len
    return ()

-- for loop recursively...
writeMantissa :: (Mantissa a) => Ptr Word8 -> Int -> Int -> a -> IO (Ptr Word8)
writeMantissa ptr olength i mantissa
  | mantissa >= 10000 = do
      let (m', c) = mantissa `divMod` 10000
          (c1, c0) = c `divMod` 100
      copy (digit_table ! fromIntegral c0) (ptr `plusPtr` (olength - i - 1))
      copy (digit_table ! fromIntegral c1) (ptr `plusPtr` (olength - i - 3))
      writeMantissa ptr olength (i + 4) m'
  | mantissa >= 100 = do
      let (m', c) = mantissa `divMod` 100
      copy (digit_table ! fromIntegral c) (ptr `plusPtr` (olength - i - 1))
      writeMantissa ptr olength (i + 2) m'
  | mantissa >= 10 = do
      let bs = digit_table ! fromIntegral mantissa
      poke (ptr `plusPtr` (olength  - i)) (BS.last bs)
      poke ptr (BS.head bs)
      finalize ptr
  | otherwise = do
      poke ptr (toAscii mantissa :: Word8)
      finalize ptr
  where finalize p = if olength > 1
                        then poke (p `plusPtr` 1) (BS.c2w '.') >> return (p `plusPtr` (olength + 1))
                        else return (p `plusPtr` 1)

writeExponent :: Ptr Word8 -> Int32 -> IO (Ptr Word8)
writeExponent ptr exponent
  | exponent >= 100 = do
      let (e1, e0) = exponent `divMod` 10
      copy (digit_table ! e1) ptr
      poke (ptr `plusPtr` 2) (toAscii e0 :: Word8)
      return $ ptr `plusPtr` 3
  | exponent >= 10 = do
      copy (digit_table ! exponent) ptr
      return $ ptr `plusPtr` 2
  | otherwise = do
      poke ptr (toAscii exponent)
      return $ ptr `plusPtr` 1

writeSign :: Ptr Word8 -> Bool -> IO (Ptr Word8)
writeSign ptr True = do
    poke ptr (BS.c2w '-')
    return $ ptr `plusPtr` 1
writeSign ptr False = return ptr

toCharsBS :: (Mantissa a) => Bool -> a -> Int32 -> BS.ByteString
toCharsBS sign mantissa exponent = unsafePerformIO $ do
    let olength = decimalLength mantissa
    fp <- BS.mallocByteString 32 :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p0 -> do
        p1 <- writeSign p0 sign
        p2 <- writeMantissa p1 olength 0 mantissa
        poke p2 (BS.c2w 'E')
        let exp = exponent + fromIntegral olength - 1
            pe = p2 `plusPtr` 1
        end <- if exp < 0
                  then poke pe (BS.c2w '-') >> writeExponent (pe `plusPtr` 1) (-exp)
                  else writeExponent pe exp
        return $ BS.PS fp 0 (end `minusPtr` p0)

toChars' :: (Mantissa a) => Bool -> a -> Int32 -> String
toChars' s m = BS.unpackChars . toCharsBS s m


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Floating.Ryu.TableGenerator where

import Language.Haskell.TH
import Data.Bits ((.&.))
import Data.Floating.Ryu.Common ((.<<), (.>>))
import Data.WideWord.Word128

float_pow5_inv_bitcount :: Int
float_pow5_inv_bitcount = 59

float_pow5_bitcount :: Int
float_pow5_bitcount = 61

double_pow5_bitcount :: Int
double_pow5_bitcount = 125

double_pow5_inv_bitcount :: Int
double_pow5_inv_bitcount = 125

blen :: Integer -> Integer
blen 0 = 0
blen 1 = 1
blen n = 1 + blen (n `quot` 2)

finv :: Integer -> Integer -> Integer
finv bitcount i =
    let p = 5^i
     in (1 .<< (blen p - 1 + bitcount)) `div` p + 1

f :: Integer -> Integer -> Integer
f bitcount i =
    let p = 5^i
     in p .>> (blen p - bitcount)

gen_table_f :: (Integral a) => a -> (a -> Integer) -> Q Exp
gen_table_f n f = return $ ListE (fmap (LitE . IntegerL . f) [0..n])

gen_table_d :: forall a. (Integral a) => a -> (a -> Integer) -> Q Exp
gen_table_d n f = return $ ListE (fmap ff [0..n])
    where
        ff :: a -> Exp
        ff c = let r = f c
                   hi = r .>> 64
                   lo = r .&. ((1 .<< 64) - 1)
                in AppE (AppE (ConE 'Word128) (LitE . IntegerL $ hi)) (LitE . IntegerL $ lo)


get_range :: RealFloat ff => ff -> (Integer, Integer)
get_range f =
    let (emin, emax) = floatRange f
        mantissaDigits = floatDigits f
        emin' = fromIntegral $ emin - mantissaDigits - 2
        emax' = fromIntegral $ emax - mantissaDigits - 2
        log10 x = log x / log 10
     in ((-emin') - floor (fromIntegral (-emin') * log10 5), floor $ emax' * log10 2)

(float_max_split, float_max_inv_split) = get_range (undefined :: Float)

-- we take a slightly different codepath s.t we need one extra entry
(double_max_split, double_max_inv_split) =
    let (m, mi) = get_range (undefined :: Double)
     in (m + 1, mi)


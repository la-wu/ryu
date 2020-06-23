{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Floating.Ryu.D2STable
    ( double_pow5_inv_bitcount
    , double_pow5_bitcount
    , double_pow5_inv_split
    , double_pow5_split
    , double_max_inv_split
    , double_max_split
    ) where

import Language.Haskell.TH
import Data.Array.Base
import Data.Array.ST
import Data.Array.Unboxed
import GHC.Word (Word32(..), Word64(..))
import GHC.Int (Int32(..), Int64(..), Int(..))
import Data.Floating.Ryu.TableGenerator
import Data.WideWord.Word128
import GHC.Prim
import GHC.ST (ST(..), runST)

instance MArray (STUArray s) Word128 (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)

    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n

    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (*# 16#)

    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr) (I# i) = ST $ \s1 ->
        let (# s2, w1 #) = readWord64Array# marr (i *# 2#) s1
            (# s3, w2 #) = readWord64Array# marr (i *# 2# +# 1#) s2
         in (# s2, Word128 (W64# w1) (W64# w2) #)

    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr) (I# i) (Word128 (W64# w1) (W64# w2)) = ST $ \s1 ->
        let s2 = writeWord64Array# marr (i *# 2#) w1 s1
            s3 = writeWord64Array# marr (i *# 2# +# 1#) w2 s2
         in (# s3, () #)

instance IArray UArray Word128 where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)

    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n

    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)

    -- NB: don't actually use this anywhere but...
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr) (I# i)
      = let w1 = indexWord64Array# arr (i *# 2#)
            w2 = indexWord64Array# arr (i *# 2# +# 1#)
         in Word128 (W64# w1) (W64# w2)

double_pow5_inv_split :: UArray Word64 Word128
double_pow5_inv_split = listArray (0, fromIntegral double_max_inv_split)
    $(gen_table_d double_max_inv_split (finv $ fromIntegral double_pow5_inv_bitcount))

double_pow5_split :: UArray Word64 Word128
double_pow5_split = listArray (0, fromIntegral double_max_split)
    $(gen_table_d double_max_split (f $ fromIntegral double_pow5_bitcount))


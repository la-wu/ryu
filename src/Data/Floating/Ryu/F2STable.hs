{-# LANGUAGE TemplateHaskell #-}

module Data.Floating.Ryu.F2STable
    ( float_pow5_inv_bitcount
    , float_pow5_bitcount
    , float_pow5_inv_split
    , float_pow5_split
    , float_max_inv_split
    , float_max_split
    ) where

import Language.Haskell.TH
import Data.Array.Unboxed
import GHC.Word (Word32(..), Word64(..))
import Data.Floating.Ryu.TableGenerator

float_pow5_inv_split :: UArray Word32 Word64
float_pow5_inv_split = listArray (0, fromIntegral float_max_inv_split)
    $(gen_table_f float_max_inv_split (finv $ fromIntegral float_pow5_inv_bitcount))

float_pow5_split :: UArray Word32 Word64
float_pow5_split = listArray (0, fromIntegral float_max_split)
    $(gen_table_f float_max_split (f $ fromIntegral float_pow5_bitcount))


import Control.DeepSeq
import Criterion.Main
import Data.Bits.Floating (coerceToFloat)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BB
import Data.Floating.Ryu.D2S
import Data.Floating.Ryu.F2S
import Data.Floating.Ryu.Common
import Data.Floating.RealFloat
import Data.Functor.Identity
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8, Word32, Word64)
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Numeric

-- TODO: better generation
floats :: Int -> Float -> Float -> IO [Float]
floats n l u
  = fmap ( force
         . fmap ((+) l)
         . fmap ((*) (u - l))
         . fmap ((flip (/) 0xFFFFFFFF) . fromIntegral))
  $ sequence [randomIO :: IO Word32 | _ <- [1..n]]

doubles :: Int -> Double -> Double -> IO [Double]
doubles n l u
  = fmap ( force
         . fmap ((+) l)
         . fmap ((*) (u - l))
         . fmap ((flip (/) 0xFFFFFFFFFFFFFFFF) . fromIntegral))
  $ sequence [randomIO :: IO Word64 | _ <- [1..n]]

instance NFData Data.Floating.Ryu.F2S.FloatingDecimal where
    rnf (Data.Floating.Ryu.F2S.FloatingDecimal mantissa exponent) = rnf mantissa `seq` rnf exponent

instance NFData Data.Floating.Ryu.D2S.FloatingDecimal where
    rnf (Data.Floating.Ryu.D2S.FloatingDecimal mantissa exponent) = rnf mantissa `seq` rnf exponent

nRepl :: Int
nRepl = 10000

{-# NOINLINE intData #-}
intData :: [Int]
intData = [1..nRepl]

{-# NOINLINE floatData #-}
floatData :: [Float]
floatData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# NOINLINE doubleData #-}
doubleData :: [Double]
doubleData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# INLINE benchB #-}
benchB :: String -> a -> (a -> BB.Builder) -> Benchmark
benchB name x b =
    bench (name ++" (" ++ show nRepl ++ ")") $
        whnf (BS.length . BB.toLazyByteString . b) x

main :: IO ()
main = do
    defaultMain
        [ bgroup "fold"
            [ benchB "foldMap f2s        " floatData $ foldMap (formatFloat FFGeneric Nothing)
            , benchB "foldMap show float " floatData $ foldMap (BB.string7 . show)
            , benchB "foldMap d2s        " doubleData $ foldMap (formatDouble FFGeneric Nothing)
            , benchB "foldMap show double" doubleData $ foldMap (BB.string7 . show)
            ]
        , bgroup "intermdiate"
            [ bench "fmap f2Intermediate" $ nf (fmap f2Intermediate) floatData
            , bench "fmap d2Intermediate" $ nf (fmap d2Intermediate) doubleData
            ]
        ]

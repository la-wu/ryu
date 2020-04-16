import Control.DeepSeq
import Criterion.Main
import Data.Bits.Floating (coerceToFloat)
import qualified Data.ByteString.Internal as BS
import Data.Floating.Ryu
import Data.Floating.Ryu.F2S
import Data.Floating.Ryu.Common
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8, Word32, Word64)
import System.Random
import Numeric

-- TODO: better generation
floats :: Int -> Float -> Float -> IO [Float]
floats n l u = fmap ( force
                    . fmap ((+) l)
                    . fmap ((*) (u - l))
                    . fmap ((flip (/) 0xFFFFFFFF) . fromIntegral))
             $ sequence [randomIO :: IO Word32 | _ <- [1..n]]

instance NFData Data.Floating.Ryu.F2S.FloatingDecimal where
    rnf (Data.Floating.Ryu.F2S.FloatingDecimal mantissa exponent) = rnf mantissa `seq` rnf exponent

main :: IO ()
main = do
    tenths <- floats 30 0.1 1
    small <- floats 30 (1e-15) (1e-12)
    large <- floats 30 (1e12) (1e15)
    xlarge <- floats 30 (1e30) (1e35)
    fp <- BS.mallocByteString 32 :: IO (ForeignPtr Word8)
    let suite' strength mapper =
            [ bench "tenths" $ strength mapper tenths
            , bench "small" $ strength mapper small
            , bench "large" $ strength mapper large
            , bench "xlarge" $ strength mapper xlarge
            ]
        suite strength mapper = suite' strength (fmap mapper)
    defaultMain
        [ bgroup "baseline" [ bench "id" $ nf (fmap id) tenths ]
        , bgroup "f2Intermediate" $ suite nf f2Intermediate
        , bgroup "trailing" $
            [ bench "0.1"         $ nf (fmap f2Intermediate) (take 30 . repeat $ 0.1)
            , bench "0.11"        $ nf (fmap f2Intermediate) (take 30 . repeat $ 0.11)
            , bench "0.111"       $ nf (fmap f2Intermediate) (take 30 . repeat $ 0.111)
            , bench "0.1111"      $ nf (fmap f2Intermediate) (take 30 . repeat $ 0.1111)
            , bench "0.11111"     $ nf (fmap f2Intermediate) (take 30 . repeat $ 0.11111)
            , bench "0.111111"    $ nf (fmap f2Intermediate) (take 30 . repeat $ 0.111111)
            , bench "0.1111111"   $ nf (fmap f2Intermediate) (take 30 . repeat $ 0.1111111)
            , bench "0.11111111"  $ nf (fmap f2Intermediate) (take 30 . repeat $ 0.11111111)
            ]
        , bgroup "f2s E Buffered" $ suite' nfAppIO (sequence . fmap (f2sBuffered fp))
        , bgroup "f2s E BS" $ suite nf f2sScientific'
        , bgroup "f2s F BS" $ suite nf f2sFixed'
        , bgroup "f2s G BS" $ suite nf f2sGeneral
        , bgroup "showEFloat" $ suite nf (flip (showEFloat Nothing) [])
        , bgroup "showFFloat" $ suite nf (flip (showFFloat Nothing) [])
        ]

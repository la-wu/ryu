import Control.DeepSeq
import Criterion.Main
import Data.Bits.Floating (coerceToFloat)
import Data.Floating.Ryu
import Data.Floating.Ryu.F2S
import GHC.Word (Word32, Word64)
import System.Random
import Numeric

-- TODO: better generation
floats :: Int -> Float -> Float -> IO [Float]
floats n l u = fmap ( force
                    . fmap ((+) l)
                    . fmap ((*) (u - l))
                    . fmap ((flip (/) 0xFFFFFFFF) . fromIntegral))
             $ sequence [randomIO :: IO Word32 | _ <- [1..n]]

main :: IO ()
main = do
    tenths <- floats 30 0.1 1
    small <- floats 30 (1e-15) (1e-12)
    large <- floats 30 (1e12) (1e15)
    let suite strength mapper =
            [ bench "tenths" $ strength (fmap mapper) tenths
            , bench "small" $ strength (fmap mapper) small
            , bench "large" $ strength (fmap mapper) large
            ]
    defaultMain
        [ bgroup "f2s'" $ suite whnf f2s'
        , bgroup "f2s" $ suite nf f2s
        , bgroup "showEFloat" $ suite nf (flip (showEFloat Nothing) [])
        ]

import Data.Floating.Ryu
import Data.Floating.Ryu.Common
import Data.Bits
import Data.Bits.Floating (coerceToFloat)
import Data.Int (Int32)
import GHC.Word (Word32, Word64)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "common" $ do
        it "decimalLength9" $ do
            decimalLength9 0 `shouldBe` 1
            decimalLength9 1 `shouldBe` 1
            decimalLength9 9 `shouldBe` 1
            decimalLength9 10 `shouldBe` 2
            decimalLength9 99 `shouldBe` 2
            decimalLength9 100 `shouldBe` 3
            decimalLength9 999 `shouldBe` 3
            decimalLength9 999999999 `shouldBe` 9

        it "log10pow2" $ do
            log10pow2 0 `shouldBe` (0 :: Word32)
            log10pow2 1 `shouldBe` (0 :: Word32)
            log10pow2 2 `shouldBe` (0 :: Word32)
            log10pow2 3 `shouldBe` (0 :: Word32)
            log10pow2 4 `shouldBe` (1 :: Word32)
            log10pow2 1650 `shouldBe` (496 :: Word32)

        it "log10pow5" $ do
            log10pow5 0 `shouldBe` (0 :: Word32)
            log10pow5 1 `shouldBe` (0 :: Word32)
            log10pow5 2 `shouldBe` (1 :: Word32)
            log10pow5 3 `shouldBe` (2 :: Word32)
            log10pow5 4 `shouldBe` (2 :: Word32)
            log10pow5 2620 `shouldBe` (1831 :: Word32)

    describe "f2s" $ do
        it "basic" $ do
            f2s 0.0    `shouldBe` "0E0"
            f2s (-0.0) `shouldBe` "-0E0"
            f2s 1.0    `shouldBe` "1E0"
            f2s (-1.0) `shouldBe` "-1E0"
            f2s (0/0)  `shouldBe` "NaN"
            f2s (1/0)  `shouldBe` "Infinity"
            f2s (-1/0) `shouldBe` "-Infinity"

        it "subnormal" $ do
            f2s 1.1754944e-38 `shouldBe` "1.1754944E-38"

        it "min and max" $ do
            f2s (coerceToFloat 0x7f7fffff) `shouldBe` "3.4028235E38"
            f2s (coerceToFloat 0x00000001) `shouldBe` "1E-45"

        it "boundary round even" $ do
            f2s 3.355445e7   `shouldBe` "3.355445E7"
            f2s 8.999999e9   `shouldBe` "9E9"
            f2s 3.4366717e10 `shouldBe` "3.436672E10"

        it "exact value round even" $ do
            f2s 3.0540412e5 `shouldBe` "3.0540412E5"
            f2s 8.0990312e3 `shouldBe` "8.0990312E3"

        it "lots of trailing zeros" $ do
            -- Pattern for the first test: 00111001100000000000000000000000
            f2s 2.4414062e-4 `shouldBe` "2.4414062E-4"
            f2s 2.4414062e-3 `shouldBe` "2.4414062E-3"
            f2s 4.3945312e-3 `shouldBe` "4.3945312E-3"
            f2s 6.3476562e-3 `shouldBe` "6.3476562E-3"

        it "regression" $ do
            f2s 4.7223665e21   `shouldBe` "4.7223665E21"
            f2s 8388608.0      `shouldBe` "8.388608E6"
            f2s 1.6777216e7    `shouldBe` "1.6777216E7"
            f2s 3.3554436e7    `shouldBe` "3.3554436E7"
            f2s 6.7131496e7    `shouldBe` "6.7131496E7"
            f2s 1.9310392e-38  `shouldBe` "1.9310392E-38"
            f2s (-2.47e-43)    `shouldBe` "-2.47E-43"
            f2s 1.993244e-38   `shouldBe` "1.993244E-38"
            f2s 4103.9003      `shouldBe` "4.1039004E3"
            f2s 5.3399997e9    `shouldBe` "5.3399997E9"
            f2s 6.0898e-39     `shouldBe` "6.0898E-39"
            f2s 0.0010310042   `shouldBe` "1.0310042E-3"
            f2s 2.8823261e17   `shouldBe` "2.882326E17"
            f2s 7.0385309e-26  `shouldBe` "7.038531E-26"
            f2s 9.2234038e17   `shouldBe` "9.223404E17"
            f2s 6.7108872e7    `shouldBe` "6.710887E7"
            f2s 1.0e-44        `shouldBe` "1E-44"
            f2s 2.816025e14    `shouldBe` "2.816025E14"
            f2s 9.223372e18    `shouldBe` "9.223372E18"
            f2s 1.5846085e29   `shouldBe` "1.5846086E29"
            f2s 1.1811161e19   `shouldBe` "1.1811161E19"
            f2s 5.368709e18    `shouldBe` "5.368709E18"
            f2s 4.6143165e18   `shouldBe` "4.6143166E18"
            f2s 0.007812537    `shouldBe` "7.812537E-3"
            f2s 1.4e-45        `shouldBe` "1E-45"
            f2s 1.18697724e20  `shouldBe` "1.18697725E20"
            f2s 1.00014165e-36 `shouldBe` "1.00014165E-36"
            f2s 200.0          `shouldBe` "2E2"
            f2s 3.3554432e7    `shouldBe` "3.3554432E7"

        it "looks like power of 5" $ do
            f2s (coerceToFloat 0x5D1502F9) `shouldBe` "6.7108864E17"
            f2s (coerceToFloat 0x5D9502F9) `shouldBe` "1.3421773E18"
            f2s (coerceToFloat 0x5E1502F9) `shouldBe` "2.6843546E18"

        it "output length" $ do
            f2s 1.0            `shouldBe` "1E0"
            f2s 1.2            `shouldBe` "1.2E0"
            f2s 1.23           `shouldBe` "1.23E0"
            f2s 1.234          `shouldBe` "1.234E0"
            f2s 1.2345         `shouldBe` "1.2345E0"
            f2s 1.23456        `shouldBe` "1.23456E0"
            f2s 1.234567       `shouldBe` "1.234567E0"
            f2s 1.2345678      `shouldBe` "1.2345678E0"
            f2s 1.23456735e-36 `shouldBe` "1.23456735E-36"

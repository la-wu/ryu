import Data.Floating.Ryu.F2S
import Data.Floating.Ryu.D2S
import Data.Floating.Ryu.Common
import Data.Floating.RealFloat
import Data.Bits
import Data.Bits.Floating (coerceToFloat)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Int (Int32)
import GHC.Word (Word8, Word32, Word64)
import Test.Hspec
import Test.QuickCheck

ieeeParts2Double :: Bool -> Int32 -> Word64 -> Double
ieeeParts2Double sign exponent mantissa =
    coerceToFloat $ (asWord sign .<< 63) .|. (fromIntegral exponent .<< 52) .|. mantissa

log10pow5 i = box (log10pow5Unboxed (unbox i))
log10pow2 i = box (log10pow2Unboxed (unbox i))


-- | Encode a 'String' of only ASCII characters using the ASCII encoding.
encodeASCII :: String -> [Word8]
encodeASCII =
    map encode
  where
    encode c
      | c < '\x7f' = fromIntegral $ ord c
      | otherwise  = error $ "encodeASCII: non-ASCII character '" ++ [c] ++ "'"

dec_list :: Show a =>  a -> [Word8]
dec_list = encodeASCII . show

check ref mkBuilder = property $
  \x -> let ws = ref x
         in (BL.unpack $ BB.toLazyByteString $ mkBuilder x `mappend` mkBuilder x) `shouldBe` (ws ++ ws)

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
            log10pow2 0 `shouldBe` 0
            log10pow2 1 `shouldBe` 0
            log10pow2 2 `shouldBe` 0
            log10pow2 3 `shouldBe` 0
            log10pow2 4 `shouldBe` 1
            log10pow2 1650 `shouldBe` 496

        it "log10pow5" $ do
            log10pow5 0 `shouldBe` 0
            log10pow5 1 `shouldBe` 0
            log10pow5 2 `shouldBe` 1
            log10pow5 3 `shouldBe` 2
            log10pow5 4 `shouldBe` 2
            log10pow5 2620 `shouldBe` 1831

    describe "f2sScientific" $ do
        it "basic" $ do
            f2sScientific 0.0    `shouldBe` "0e0"
            f2sScientific (-0.0) `shouldBe` "-0e0"
            f2sScientific 1.0    `shouldBe` "1.0e0"
            f2sScientific (-1.0) `shouldBe` "-1.0e0"
            f2sScientific (0/0)  `shouldBe` "NaN"
            f2sScientific (1/0)  `shouldBe` "Infinity"
            f2sScientific (-1/0) `shouldBe` "-Infinity"

        it "subnormal" $ do
            f2sScientific 1.1754944e-38 `shouldBe` "1.1754944e-38"

        it "min and max" $ do
            f2sScientific (coerceToFloat 0x7f7fffff) `shouldBe` "3.4028235e38"
            f2sScientific (coerceToFloat 0x00000001) `shouldBe` "1.0e-45"

        it "boundary round" $ do
            f2sScientific 3.355445e7   `shouldBe` "3.355445e7"
            f2sScientific 8.999999e9   `shouldBe` "9.0e9"
            f2sScientific 3.4366717e10 `shouldBe` "3.436672e10"

        it "exact value round" $ do
            f2sScientific 3.0540412e5 `shouldBe` "3.0540413e5"
            f2sScientific 8.0990312e3 `shouldBe` "8.0990313e3"

        it "lots of trailing zeros" $ do
            -- Pattern for the first test: 00111001100000000000000000000000
            f2sScientific 2.4414062e-4 `shouldBe` "2.4414063e-4"
            f2sScientific 2.4414062e-3 `shouldBe` "2.4414063e-3"
            f2sScientific 4.3945312e-3 `shouldBe` "4.3945313e-3"
            f2sScientific 6.3476562e-3 `shouldBe` "6.3476563e-3"

        it "regression" $ do
            f2sScientific 4.7223665e21   `shouldBe` "4.7223665e21"
            f2sScientific 8388608.0      `shouldBe` "8.388608e6"
            f2sScientific 1.6777216e7    `shouldBe` "1.6777216e7"
            f2sScientific 3.3554436e7    `shouldBe` "3.3554436e7"
            f2sScientific 6.7131496e7    `shouldBe` "6.7131496e7"
            f2sScientific 1.9310392e-38  `shouldBe` "1.9310392e-38"
            f2sScientific (-2.47e-43)    `shouldBe` "-2.47e-43"
            f2sScientific 1.993244e-38   `shouldBe` "1.993244e-38"
            f2sScientific 4103.9003      `shouldBe` "4.1039004e3"
            f2sScientific 5.3399997e9    `shouldBe` "5.3399997e9"
            f2sScientific 6.0898e-39     `shouldBe` "6.0898e-39"
            f2sScientific 0.0010310042   `shouldBe` "1.0310042e-3"
            f2sScientific 2.8823261e17   `shouldBe` "2.882326e17"
            f2sScientific 7.0385309e-26  `shouldBe` "7.038531e-26"
            f2sScientific 9.2234038e17   `shouldBe` "9.223404e17"
            f2sScientific 6.7108872e7    `shouldBe` "6.710887e7"
            f2sScientific 1.0e-44        `shouldBe` "1.0e-44"
            f2sScientific 2.816025e14    `shouldBe` "2.816025e14"
            f2sScientific 9.223372e18    `shouldBe` "9.223372e18"
            f2sScientific 1.5846085e29   `shouldBe` "1.5846086e29"
            f2sScientific 1.1811161e19   `shouldBe` "1.1811161e19"
            f2sScientific 5.368709e18    `shouldBe` "5.368709e18"
            f2sScientific 4.6143165e18   `shouldBe` "4.6143166e18"
            f2sScientific 0.007812537    `shouldBe` "7.812537e-3"
            f2sScientific 1.4e-45        `shouldBe` "1.0e-45"
            f2sScientific 1.18697724e20  `shouldBe` "1.18697725e20"
            f2sScientific 1.00014165e-36 `shouldBe` "1.00014165e-36"
            f2sScientific 200.0          `shouldBe` "2.0e2"
            f2sScientific 3.3554432e7    `shouldBe` "3.3554432e7"

        it "looks like power of 5" $ do
            f2sScientific (coerceToFloat 0x5D1502F9) `shouldBe` "6.7108864e17"
            f2sScientific (coerceToFloat 0x5D9502F9) `shouldBe` "1.3421773e18"
            f2sScientific (coerceToFloat 0x5e1502F9) `shouldBe` "2.6843546e18"

        it "output length" $ do
            f2sScientific 1.0            `shouldBe` "1.0e0"
            f2sScientific 1.2            `shouldBe` "1.2e0"
            f2sScientific 1.23           `shouldBe` "1.23e0"
            f2sScientific 1.234          `shouldBe` "1.234e0"
            f2sScientific 1.2345         `shouldBe` "1.2345e0"
            f2sScientific 1.23456        `shouldBe` "1.23456e0"
            f2sScientific 1.234567       `shouldBe` "1.234567e0"
            f2sScientific 1.2345678      `shouldBe` "1.2345678e0"
            f2sScientific 1.23456735e-36 `shouldBe` "1.23456735e-36"

    describe "f2sFixed" $ do
        it "basic" $ do
            f2sFixed 0.0    `shouldBe` "0"
            f2sFixed (-0.0) `shouldBe` "-0"
            f2sFixed 1.0    `shouldBe` "1"
            f2sFixed (-1.0) `shouldBe` "-1"
            f2sFixed (0/0)  `shouldBe` "NaN"
            f2sFixed (1/0)  `shouldBe` "Infinity"
            f2sFixed (-1/0) `shouldBe` "-Infinity"

        it "subnormal" $ do
            f2sFixed 1.1754944e-38 `shouldBe` "0.000000000000000000000000000000000000011754944"

        it "min and max" $ do
            f2sFixed (coerceToFloat 0x7f7fffff) `shouldBe` "340282346638528859811704183484516925440"
            f2sFixed (coerceToFloat 0x00000001) `shouldBe` "0.000000000000000000000000000000000000000000001"

        it "boundary round" $ do
            f2sFixed 3.355445e7   `shouldBe` "33554448"
            f2sFixed 8.999999e9   `shouldBe` "8999999488"
            f2sFixed 3.4366717e10 `shouldBe` "34366717952"

        it "exact value round" $ do
            f2sFixed 3.0540412e5 `shouldBe` "305404.13"
            f2sFixed 8.0990312e3 `shouldBe` "8099.0313"

        it "lots of trailing zeros" $ do
            -- Pattern for the first test: 00111001100000000000000000000000
            f2sFixed 2.4414062e-4 `shouldBe` "0.00024414063"
            f2sFixed 2.4414062e-3 `shouldBe` "0.0024414063"
            f2sFixed 4.3945312e-3 `shouldBe` "0.0043945313"
            f2sFixed 6.3476562e-3 `shouldBe` "0.0063476563"

        it "regression" $ do
            f2sFixed 4.7223665e21   `shouldBe` "4722366482869645213696"
            f2sFixed 8388608.0      `shouldBe` "8388608"
            f2sFixed 1.6777216e7    `shouldBe` "16777216"
            f2sFixed 3.3554436e7    `shouldBe` "33554436"
            f2sFixed 6.7131496e7    `shouldBe` "67131496"
            f2sFixed 1.9310392e-38  `shouldBe` "0.000000000000000000000000000000000000019310392"
            f2sFixed (-2.47e-43)    `shouldBe` "-0.000000000000000000000000000000000000000000247"
            f2sFixed 1.993244e-38   `shouldBe` "0.00000000000000000000000000000000000001993244"
            f2sFixed 4103.9003      `shouldBe` "4103.9004"
            f2sFixed 5.3399997e9    `shouldBe` "5339999744"
            f2sFixed 6.0898e-39     `shouldBe` "0.0000000000000000000000000000000000000060898"
            f2sFixed 0.0010310042   `shouldBe` "0.0010310042"
            f2sFixed 2.8823261e17   `shouldBe` "288232609534705664"
            f2sFixed 7.0385309e-26  `shouldBe` "0.00000000000000000000000007038531"
            f2sFixed 9.2234038e17   `shouldBe` "922340378525302784"
            f2sFixed 6.7108872e7    `shouldBe` "67108872"
            f2sFixed 1.0e-44        `shouldBe` "0.00000000000000000000000000000000000000000001"
            f2sFixed 2.816025e14    `shouldBe` "281602483552256"
            f2sFixed 9.223372e18    `shouldBe` "9223372036854775808"
            f2sFixed 1.5846085e29   `shouldBe` "158460858500352230046493048832"
            f2sFixed 1.1811161e19   `shouldBe` "11811160613755813888"
            f2sFixed 5.368709e18    `shouldBe` "5368709120000000000"
            f2sFixed 4.6143165e18   `shouldBe` "4614316599996841984"
            f2sFixed 0.007812537    `shouldBe` "0.007812537"
            f2sFixed 1.4e-45        `shouldBe` "0.000000000000000000000000000000000000000000001"
            f2sFixed 1.18697724e20  `shouldBe` "118697724999999946752"
            f2sFixed 1.00014165e-36 `shouldBe` "0.00000000000000000000000000000000000100014165"
            f2sFixed 200.0          `shouldBe` "200"
            f2sFixed 3.3554432e7    `shouldBe` "33554432"

        it "looks like power of 5" $ do
            f2sFixed (coerceToFloat 0x5D1502F9) `shouldBe` "671088640000000000"
            f2sFixed (coerceToFloat 0x5D9502F9) `shouldBe` "1342177280000000000"
            f2sFixed (coerceToFloat 0x5e1502F9) `shouldBe` "2684354560000000000"

        it "output length" $ do
            f2sFixed 1.0            `shouldBe` "1"
            f2sFixed 1.2            `shouldBe` "1.2"
            f2sFixed 1.23           `shouldBe` "1.23"
            f2sFixed 1.234          `shouldBe` "1.234"
            f2sFixed 1.2345         `shouldBe` "1.2345"
            f2sFixed 1.23456        `shouldBe` "1.23456"
            f2sFixed 1.234567       `shouldBe` "1.234567"
            f2sFixed 1.2345678      `shouldBe` "1.2345678"
            f2sFixed 1.23456735e-36 `shouldBe` "0.00000000000000000000000000000000000123456735"

        it "powers of 10" $ do
            f2sFixed 1.0e-45 `shouldBe` "0.000000000000000000000000000000000000000000001"
            f2sFixed 1.0e-44 `shouldBe` "0.00000000000000000000000000000000000000000001"
            f2sFixed 1.0e-43 `shouldBe` "0.0000000000000000000000000000000000000000001"
            f2sFixed 1.0e-42 `shouldBe` "0.000000000000000000000000000000000000000001"
            f2sFixed 1.0e-41 `shouldBe` "0.00000000000000000000000000000000000000001"
            f2sFixed 1.0e-40 `shouldBe` "0.0000000000000000000000000000000000000001"
            f2sFixed 1.0e-39 `shouldBe` "0.000000000000000000000000000000000000001"
            f2sFixed 1.0e-38 `shouldBe` "0.00000000000000000000000000000000000001"
            f2sFixed 1.0e-37 `shouldBe` "0.0000000000000000000000000000000000001"
            f2sFixed 1.0e-36 `shouldBe` "0.000000000000000000000000000000000001"
            f2sFixed 1.0e-35 `shouldBe` "0.00000000000000000000000000000000001"
            f2sFixed 1.0e-34 `shouldBe` "0.0000000000000000000000000000000001"
            f2sFixed 1.0e-33 `shouldBe` "0.000000000000000000000000000000001"
            f2sFixed 1.0e-32 `shouldBe` "0.00000000000000000000000000000001"
            f2sFixed 1.0e-31 `shouldBe` "0.0000000000000000000000000000001"
            f2sFixed 1.0e-30 `shouldBe` "0.000000000000000000000000000001"
            f2sFixed 1.0e-29 `shouldBe` "0.00000000000000000000000000001"
            f2sFixed 1.0e-28 `shouldBe` "0.0000000000000000000000000001"
            f2sFixed 1.0e-27 `shouldBe` "0.000000000000000000000000001"
            f2sFixed 1.0e-26 `shouldBe` "0.00000000000000000000000001"
            f2sFixed 1.0e-25 `shouldBe` "0.0000000000000000000000001"
            f2sFixed 1.0e-24 `shouldBe` "0.000000000000000000000001"
            f2sFixed 1.0e-23 `shouldBe` "0.00000000000000000000001"
            f2sFixed 1.0e-22 `shouldBe` "0.0000000000000000000001"
            f2sFixed 1.0e-21 `shouldBe` "0.000000000000000000001"
            f2sFixed 1.0e-20 `shouldBe` "0.00000000000000000001"
            f2sFixed 1.0e-19 `shouldBe` "0.0000000000000000001"
            f2sFixed 1.0e-18 `shouldBe` "0.000000000000000001"
            f2sFixed 1.0e-17 `shouldBe` "0.00000000000000001"
            f2sFixed 1.0e-16 `shouldBe` "0.0000000000000001"
            f2sFixed 1.0e-15 `shouldBe` "0.000000000000001"
            f2sFixed 1.0e-14 `shouldBe` "0.00000000000001"
            f2sFixed 1.0e-13 `shouldBe` "0.0000000000001"
            f2sFixed 1.0e-12 `shouldBe` "0.000000000001"
            f2sFixed 1.0e-11 `shouldBe` "0.00000000001"
            f2sFixed 1.0e-10 `shouldBe` "0.0000000001"
            f2sFixed 1.0e-9  `shouldBe` "0.000000001"
            f2sFixed 1.0e-8  `shouldBe` "0.00000001"
            f2sFixed 1.0e-7  `shouldBe` "0.0000001"
            f2sFixed 1.0e-6  `shouldBe` "0.000001"
            f2sFixed 1.0e-5  `shouldBe` "0.00001"
            f2sFixed 1.0e-4  `shouldBe` "0.0001"
            f2sFixed 1.0e-3  `shouldBe` "0.001"
            f2sFixed 1.0e-2  `shouldBe` "0.01"
            f2sFixed 1.0e-1  `shouldBe` "0.1"
            f2sFixed 1.0e0   `shouldBe` "1"
            f2sFixed 1.0e1   `shouldBe` "10"
            f2sFixed 1.0e2   `shouldBe` "100"
            f2sFixed 1.0e3   `shouldBe` "1000"
            f2sFixed 1.0e4   `shouldBe` "10000"
            f2sFixed 1.0e5   `shouldBe` "100000"
            f2sFixed 1.0e6   `shouldBe` "1000000"
            f2sFixed 1.0e7   `shouldBe` "10000000"
            f2sFixed 1.0e8   `shouldBe` "100000000"
            f2sFixed 1.0e9   `shouldBe` "1000000000"
            f2sFixed 1.0e10  `shouldBe` "10000000000"
            f2sFixed 1.0e11  `shouldBe` "99999997952"
            f2sFixed 1.0e12  `shouldBe` "999999995904"
            f2sFixed 1.0e13  `shouldBe` "9999999827968"
            f2sFixed 1.0e14  `shouldBe` "100000000376832"
            f2sFixed 1.0e15  `shouldBe` "999999986991104"
            f2sFixed 1.0e16  `shouldBe` "10000000272564224"
            f2sFixed 1.0e17  `shouldBe` "99999998430674944"
            f2sFixed 1.0e18  `shouldBe` "999999984306749440"
            f2sFixed 1.0e19  `shouldBe` "9999999980506447872"
            f2sFixed 1.0e20  `shouldBe` "100000002004087734272"
            f2sFixed 1.0e21  `shouldBe` "1000000020040877342720"
            f2sFixed 1.0e22  `shouldBe` "9999999778196308361216"
            f2sFixed 1.0e23  `shouldBe` "99999997781963083612160"
            f2sFixed 1.0e24  `shouldBe` "1000000013848427855085568"
            f2sFixed 1.0e25  `shouldBe` "9999999562023526247432192"
            f2sFixed 1.0e26  `shouldBe` "100000002537764290115403776"
            f2sFixed 1.0e27  `shouldBe` "999999988484154753734934528"
            f2sFixed 1.0e28  `shouldBe` "9999999442119689768320106496"
            f2sFixed 1.0e29  `shouldBe` "100000001504746621987668885504"
            f2sFixed 1.0e30  `shouldBe` "1000000015047466219876688855040"
            f2sFixed 1.0e31  `shouldBe` "9999999848243207295109594873856"
            f2sFixed 1.0e32  `shouldBe` "100000003318135351409612647563264"
            f2sFixed 1.0e33  `shouldBe` "999999994495727286427992885035008"
            f2sFixed 1.0e34  `shouldBe` "9999999790214767953607394487959552"
            f2sFixed 1.0e35  `shouldBe` "100000004091847875962975319375216640"
            f2sFixed 1.0e36  `shouldBe` "999999961690316245365415600208216064"
            f2sFixed 1.0e37  `shouldBe` "9999999933815812510711506376257961984"
            f2sFixed 1.0e38  `shouldBe` "99999996802856924650656260769173209088"
            f2sFixed 1.0e39  `shouldBe` "Infinity"

    describe "d2s" $ do
        it "basic" $ do
            d2s 0.0    `shouldBe` "0e0"
            d2s (-0.0) `shouldBe` "-0e0"
            d2s 1.0    `shouldBe` "1.0e0"
            d2s (-1.0) `shouldBe` "-1.0e0"
            d2s (0/0)  `shouldBe` "NaN"
            d2s (1/0)  `shouldBe` "Infinity"
            d2s (-1/0) `shouldBe` "-Infinity"

        it "subnormal" $ do
            d2s 2.2250738585072014e-308 `shouldBe` "2.2250738585072014e-308"

        it "min and max" $ do
            d2s (coerceToFloat 0x7fefffffffffffff) `shouldBe` "1.7976931348623157e308"
            d2s (coerceToFloat 0x0000000000000001) `shouldBe` "5.0e-324"

        it "lots of trailing zeros" $ do
            d2s 2.98023223876953125e-8 `shouldBe` "2.9802322387695313e-8"

        it "regression" $ do
            d2s (-2.109808898695963e16) `shouldBe` "-2.109808898695963e16"
            d2s 4.940656e-318           `shouldBe` "4.940656e-318"
            d2s 1.18575755e-316         `shouldBe` "1.18575755e-316"
            d2s 2.989102097996e-312     `shouldBe` "2.989102097996e-312"
            d2s 9.0608011534336e15      `shouldBe` "9.0608011534336e15"
            d2s 4.708356024711512e18    `shouldBe` "4.708356024711512e18"
            d2s 9.409340012568248e18    `shouldBe` "9.409340012568248e18"
            d2s 1.2345678               `shouldBe` "1.2345678e0"

        it "looks like power of 5" $ do
            d2s (coerceToFloat 0x4830F0CF064DD592) `shouldBe` "5.764607523034235e39"
            d2s (coerceToFloat 0x4840F0CF064DD592) `shouldBe` "1.152921504606847e40"
            d2s (coerceToFloat 0x4850F0CF064DD592) `shouldBe` "2.305843009213694e40"

        it "output length" $ do
            d2s 1                  `shouldBe` "1.0e0"
            d2s 1.2                `shouldBe` "1.2e0"
            d2s 1.23               `shouldBe` "1.23e0"
            d2s 1.234              `shouldBe` "1.234e0"
            d2s 1.2345             `shouldBe` "1.2345e0"
            d2s 1.23456            `shouldBe` "1.23456e0"
            d2s 1.234567           `shouldBe` "1.234567e0"
            d2s 1.2345678          `shouldBe` "1.2345678e0"
            d2s 1.23456789         `shouldBe` "1.23456789e0"
            d2s 1.234567895        `shouldBe` "1.234567895e0"
            d2s 1.2345678901       `shouldBe` "1.2345678901e0"
            d2s 1.23456789012      `shouldBe` "1.23456789012e0"
            d2s 1.234567890123     `shouldBe` "1.234567890123e0"
            d2s 1.2345678901234    `shouldBe` "1.2345678901234e0"
            d2s 1.23456789012345   `shouldBe` "1.23456789012345e0"
            d2s 1.234567890123456  `shouldBe` "1.234567890123456e0"
            d2s 1.2345678901234567 `shouldBe` "1.2345678901234567e0"

            -- Test 32-bit chunking
            d2s 4.294967294 `shouldBe` "4.294967294e0"
            d2s 4.294967295 `shouldBe` "4.294967295e0"
            d2s 4.294967296 `shouldBe` "4.294967296e0"
            d2s 4.294967297 `shouldBe` "4.294967297e0"
            d2s 4.294967298 `shouldBe` "4.294967298e0"

        it "min max shift" $ do
            let maxMantissa = mask 53 :: Word64
            d2s (ieeeParts2Double False 4 0) `shouldBe` "1.7800590868057611e-307"
            -- 32-bit opt-size=0:  49 <= dist <= 49
            -- 32-bit opt-size=1:  28 <= dist <= 49
            -- 64-bit opt-size=0:  50 <= dist <= 50
            -- 64-bit opt-size=1:  28 <= dist <= 50
            d2s (ieeeParts2Double False 6 maxMantissa) `shouldBe` "2.8480945388892175e-306"
            -- 32-bit opt-size=0:  52 <= dist <= 53
            -- 32-bit opt-size=1:   2 <= dist <= 53
            -- 64-bit opt-size=0:  53 <= dist <= 53
            -- 64-bit opt-size=1:   2 <= dist <= 53
            d2s (ieeeParts2Double False 41 0) `shouldBe` "2.446494580089078e-296"
            -- 32-bit opt-size=0:  52 <= dist <= 52
            -- 32-bit opt-size=1:   2 <= dist <= 52
            -- 64-bit opt-size=0:  53 <= dist <= 53
            -- 64-bit opt-size=1:   2 <= dist <= 53
            d2s (ieeeParts2Double False 40 maxMantissa) `shouldBe` "4.8929891601781557e-296"
            -- 32-bit opt-size=0:  57 <= dist <= 58
            -- 32-bit opt-size=1:  57 <= dist <= 58
            -- 64-bit opt-size=0:  58 <= dist <= 58
            -- 64-bit opt-size=1:  58 <= dist <= 58
            d2s (ieeeParts2Double False 1077 0) `shouldBe` "1.8014398509481984e16"
            -- 32-bit opt-size=0:  57 <= dist <= 57
            -- 32-bit opt-size=1:  57 <= dist <= 57
            -- 64-bit opt-size=0:  58 <= dist <= 58
            -- 64-bit opt-size=1:  58 <= dist <= 58
            d2s (ieeeParts2Double False 1076 maxMantissa) `shouldBe` "3.6028797018963964e16"
            -- 32-bit opt-size=0:  51 <= dist <= 52
            -- 32-bit opt-size=1:  51 <= dist <= 59
            -- 64-bit opt-size=0:  52 <= dist <= 52
            -- 64-bit opt-size=1:  52 <= dist <= 59
            d2s (ieeeParts2Double False 307 0) `shouldBe` "2.900835519859558e-216"
            -- 32-bit opt-size=0:  51 <= dist <= 51
            -- 32-bit opt-size=1:  51 <= dist <= 59
            -- 64-bit opt-size=0:  52 <= dist <= 52
            -- 64-bit opt-size=1:  52 <= dist <= 59
            d2s (ieeeParts2Double False 306 maxMantissa) `shouldBe` "5.801671039719115e-216"
            -- 32-bit opt-size=0:  49 <= dist <= 49
            -- 32-bit opt-size=1:  44 <= dist <= 49
            -- 64-bit opt-size=0:  50 <= dist <= 50
            -- 64-bit opt-size=1:  44 <= dist <= 50
            d2s (ieeeParts2Double False 934 0x000FA7161A4D6e0C) `shouldBe` "3.196104012172126e-27"

        it "small integers" $ do
            d2s 9007199254740991.0 `shouldBe` "9.007199254740991e15"
            d2s 9007199254740992.0 `shouldBe` "9.007199254740992e15"

            d2s 1.0e+0                `shouldBe` "1.0e0"
            d2s 1.2e+1                `shouldBe` "1.2e1"
            d2s 1.23e+2               `shouldBe` "1.23e2"
            d2s 1.234e+3              `shouldBe` "1.234e3"
            d2s 1.2345e+4             `shouldBe` "1.2345e4"
            d2s 1.23456e+5            `shouldBe` "1.23456e5"
            d2s 1.234567e+6           `shouldBe` "1.234567e6"
            d2s 1.2345678e+7          `shouldBe` "1.2345678e7"
            d2s 1.23456789e+8         `shouldBe` "1.23456789e8"
            d2s 1.23456789e+9         `shouldBe` "1.23456789e9"
            d2s 1.234567895e+9        `shouldBe` "1.234567895e9"
            d2s 1.2345678901e+10      `shouldBe` "1.2345678901e10"
            d2s 1.23456789012e+11     `shouldBe` "1.23456789012e11"
            d2s 1.234567890123e+12    `shouldBe` "1.234567890123e12"
            d2s 1.2345678901234e+13   `shouldBe` "1.2345678901234e13"
            d2s 1.23456789012345e+14  `shouldBe` "1.23456789012345e14"
            d2s 1.234567890123456e+15 `shouldBe` "1.234567890123456e15"

            -- 10^i
            d2s 1.0e+0  `shouldBe` "1.0e0"
            d2s 1.0e+1  `shouldBe` "1.0e1"
            d2s 1.0e+2  `shouldBe` "1.0e2"
            d2s 1.0e+3  `shouldBe` "1.0e3"
            d2s 1.0e+4  `shouldBe` "1.0e4"
            d2s 1.0e+5  `shouldBe` "1.0e5"
            d2s 1.0e+6  `shouldBe` "1.0e6"
            d2s 1.0e+7  `shouldBe` "1.0e7"
            d2s 1.0e+8  `shouldBe` "1.0e8"
            d2s 1.0e+9  `shouldBe` "1.0e9"
            d2s 1.0e+10 `shouldBe` "1.0e10"
            d2s 1.0e+11 `shouldBe` "1.0e11"
            d2s 1.0e+12 `shouldBe` "1.0e12"
            d2s 1.0e+13 `shouldBe` "1.0e13"
            d2s 1.0e+14 `shouldBe` "1.0e14"
            d2s 1.0e+15 `shouldBe` "1.0e15"

            -- 10^15 + 10^i
            d2s (1.0e+15 + 1.0e+0)  `shouldBe` "1.000000000000001e15"
            d2s (1.0e+15 + 1.0e+1)  `shouldBe` "1.00000000000001e15"
            d2s (1.0e+15 + 1.0e+2)  `shouldBe` "1.0000000000001e15"
            d2s (1.0e+15 + 1.0e+3)  `shouldBe` "1.000000000001e15"
            d2s (1.0e+15 + 1.0e+4)  `shouldBe` "1.00000000001e15"
            d2s (1.0e+15 + 1.0e+5)  `shouldBe` "1.0000000001e15"
            d2s (1.0e+15 + 1.0e+6)  `shouldBe` "1.000000001e15"
            d2s (1.0e+15 + 1.0e+7)  `shouldBe` "1.00000001e15"
            d2s (1.0e+15 + 1.0e+8)  `shouldBe` "1.0000001e15"
            d2s (1.0e+15 + 1.0e+9)  `shouldBe` "1.000001e15"
            d2s (1.0e+15 + 1.0e+10) `shouldBe` "1.00001e15"
            d2s (1.0e+15 + 1.0e+11) `shouldBe` "1.0001e15"
            d2s (1.0e+15 + 1.0e+12) `shouldBe` "1.001e15"
            d2s (1.0e+15 + 1.0e+13) `shouldBe` "1.01e15"
            d2s (1.0e+15 + 1.0e+14) `shouldBe` "1.1e15"

            -- Largest power of 2 <= 10^(i+1)
            d2s 8.0                `shouldBe` "8.0e0"
            d2s 64.0               `shouldBe` "6.4e1"
            d2s 512.0              `shouldBe` "5.12e2"
            d2s 8192.0             `shouldBe` "8.192e3"
            d2s 65536.0            `shouldBe` "6.5536e4"
            d2s 524288.0           `shouldBe` "5.24288e5"
            d2s 8388608.0          `shouldBe` "8.388608e6"
            d2s 67108864.0         `shouldBe` "6.7108864e7"
            d2s 536870912.0        `shouldBe` "5.36870912e8"
            d2s 8589934592.0       `shouldBe` "8.589934592e9"
            d2s 68719476736.0      `shouldBe` "6.8719476736e10"
            d2s 549755813888.0     `shouldBe` "5.49755813888e11"
            d2s 8796093022208.0    `shouldBe` "8.796093022208e12"
            d2s 70368744177664.0   `shouldBe` "7.0368744177664e13"
            d2s 562949953421312.0  `shouldBe` "5.62949953421312e14"
            d2s 9007199254740992.0 `shouldBe` "9.007199254740992e15"

            -- 1000 * (Largest power of 2 <= 10^(i+1))
            d2s 8.0e+3             `shouldBe` "8.0e3"
            d2s 64.0e+3            `shouldBe` "6.4e4"
            d2s 512.0e+3           `shouldBe` "5.12e5"
            d2s 8192.0e+3          `shouldBe` "8.192e6"
            d2s 65536.0e+3         `shouldBe` "6.5536e7"
            d2s 524288.0e+3        `shouldBe` "5.24288e8"
            d2s 8388608.0e+3       `shouldBe` "8.388608e9"
            d2s 67108864.0e+3      `shouldBe` "6.7108864e10"
            d2s 536870912.0e+3     `shouldBe` "5.36870912e11"
            d2s 8589934592.0e+3    `shouldBe` "8.589934592e12"
            d2s 68719476736.0e+3   `shouldBe` "6.8719476736e13"
            d2s 549755813888.0e+3  `shouldBe` "5.49755813888e14"
            d2s 8796093022208.0e+3 `shouldBe` "8.796093022208e15"

    describe "realFloat" $ do
        it "powers of 10" $ do
          let pows =
                [ 1.0e-45 , 1.0e-44 , 1.0e-43 , 1.0e-42 , 1.0e-41 , 1.0e-40
                , 1.0e-39 , 1.0e-38 , 1.0e-37 , 1.0e-36 , 1.0e-35 , 1.0e-34
                , 1.0e-33 , 1.0e-32 , 1.0e-31 , 1.0e-30 , 1.0e-29 , 1.0e-28
                , 1.0e-27 , 1.0e-26 , 1.0e-25 , 1.0e-24 , 1.0e-23 , 1.0e-22
                , 1.0e-21 , 1.0e-20 , 1.0e-19 , 1.0e-18 , 1.0e-17 , 1.0e-16
                , 1.0e-15 , 1.0e-14 , 1.0e-13 , 1.0e-12 , 1.0e-11 , 1.0e-10
                , 1.0e-9  , 1.0e-8  , 1.0e-7  , 1.0e-6  , 1.0e-5  , 1.0e-4
                , 1.0e-3  , 1.0e-2  , 1.0e-1  , 1.0e0   , 1.0e1   , 1.0e2
                , 1.0e3   , 1.0e4   , 1.0e5   , 1.0e6   , 1.0e7   , 1.0e8
                , 1.0e9   , 1.0e10  , 1.0e11  , 1.0e12  , 1.0e13  , 1.0e14
                , 1.0e15  , 1.0e16  , 1.0e17  , 1.0e18  , 1.0e19  , 1.0e20
                , 1.0e21  , 1.0e22  , 1.0e23  , 1.0e24  , 1.0e25  , 1.0e26
                , 1.0e27  , 1.0e28  , 1.0e29  , 1.0e30  , 1.0e31  , 1.0e32
                , 1.0e33  , 1.0e34  , 1.0e35  , 1.0e36  , 1.0e37  , 1.0e38
                , 1.0e39
                ]
          fmap (BL.unpack . BB.toLazyByteString . floatDec) pows `shouldBe` fmap dec_list pows

        it "floatDec" $ check dec_list floatDec
        it "doubleDec" $ check dec_list doubleDec

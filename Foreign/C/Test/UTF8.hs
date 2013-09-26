
-- |
-- Module      :  Foreign.C.Test.UTF8
-- Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  alistair@abayley.org
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- UTF8 decoder and encoder tests.


module Foreign.C.Test.UTF8 where

import Prelude hiding (catch)
import Control.Exception.Extensible
import Data.Char
import Foreign.C.String
import Foreign.C.UTF8
import Foreign.Marshal.Array
import Foreign.Ptr
import Test.MiniUnit
import qualified Test.QuickCheck as QC
import Data.Word (Word8)


runTest :: a -> [String] -> IO ()
runTest _ args = runTestTT "UTF8" mkTestList >> return ()

--mkTestList :: Test
--mkTestList = TestList (map TestCase testlist)
mkTestList = testlist

testlist :: [IO ()]
testlist = 
  [ testUTF8RoundTrip
  , testUTF8StringLenRoundTrip
  , quickCheckUTF8RoundTrip
  , testFromUTF8Failure5Bytes
  , testFromUTF8Failure6Bytes
  -- , testLargeFromUTF8
  -- , testLargeFromUTF8Len
  ]


instance QC.Arbitrary Char where
  arbitrary = QC.choose (chr 1, chr 0x10FFFF)
  coarbitrary = undefined

prop_roundtrip s = s == fromUTF8 (toUTF8 s)
quickCheckUTF8RoundTrip = QC.test prop_roundtrip

testUTF8RoundTrip = do
  utf8RoundTrip "1ByteLow"   0x000001 [0x01]
  utf8RoundTrip "1ByteHigh"  0x00007F [0x7F]
  utf8RoundTrip "2BytesLow"  0x000080 [0xC2, 0x80]
  utf8RoundTrip "2BytesHigh" 0x0007FF [0xDF, 0xBF]
  utf8RoundTrip "3BytesLow"  0x000800 [0xE0, 0xA0, 0x80]
  utf8RoundTrip "3BytesHigh" 0x00FFFF [0xEF, 0xBF, 0xBF]
  utf8RoundTrip "4BytesLow"  0x010000 [0xF0, 0x90, 0x80, 0x80]
  -- chr 0x10FFFF is the largest code-point Haskell currently allows
  utf8RoundTrip "4BytesHigh" 0x10FFFF [0xF4, 0x8F, 0xBF, 0xBF]

utf8RoundTrip :: String -> Int -> [Word8] -> IO ()
utf8RoundTrip msg unicode utf8 = do
  assertEqual ("testToUTF8-" ++ msg) utf8 (toUTF8 [chr unicode])
  assertEqual ("testFromUTF8-" ++ msg) [chr unicode] (fromUTF8 utf8)
  withUTF8String [chr unicode] $ \cstr -> do
    w8 <- peekArray0 0 (castPtr cstr)
    assertEqual ("testToUTF8Ptr-" ++ msg) utf8 w8
  withCString (map (chr . fromIntegral) utf8) $ \cstr -> do
    s <- peekUTF8String cstr
    assertEqual ("testFromUTF8Ptr-" ++ msg) [chr unicode] s

testUTF8StringLenRoundTrip = do
  utf8StringLenRoundTrip "1ByteLow"   [0x000001, 0x000001] [0x01, 0x01]
  utf8StringLenRoundTrip "2BytesLow"  [0x000080, 0x00007F] [0xC2, 0x80, 0x7F]
  utf8StringLenRoundTrip "3BytesLow"  [0x000800, 0x000001] [0xE0, 0xA0, 0x80, 0x01]
  utf8StringLenRoundTrip "4BytesLow"  [0x010000, 0x00007F] [0xF0, 0x90, 0x80, 0x80, 0x7F]
  utf8StringLenRoundTrip "4BytesHigh" [0x10FFFF, 0x00007F] [0xF4, 0x8F, 0xBF, 0xBF, 0x01]

utf8StringLenRoundTrip :: String -> [Int] -> [Word8] -> IO ()
utf8StringLenRoundTrip msg codepoints utf8 = do
  let unicode = map chr codepoints
  let expect = take (length unicode - 1) unicode
  -- convert our unicode code-points (Haskell String)
  -- into a UTF8 CStringLen.
  -- Pass this back to peekUTF8StringLen to convert back into Haskell String.
  -- Chop off the last char - make sure it's only one byte.
  withUTF8StringLen unicode $ \(cstr, clen) -> do
    assertEqual ("testUTF8StringLenRoundTrip-" ++ msg) (length utf8) clen
    s <- peekUTF8StringLen (cstr, clen - 1)
    assertEqual ("testUTF8StringLenRoundTrip-" ++ msg) expect s



--testUTF8StringLenRoundTrip = do

testFromUTF8Failure5Bytes = do
  let utf8 = [0xF8, 0x80, 0x80, 0x80, 0x80]
  let handler :: ErrorCall -> IO ()
      handler (ErrorCall msg) = assertEqual "testFromUTF8Failure5Bytes" "fromUTF8: illegal UTF-8 character 248" msg
  catch (do
    assertEqual "testFromUTF8Failure5Bytes" [chr 0x10FFFF] (fromUTF8 utf8)
    assertBool "testFromUTF8Failure5Bytes: no error raised" False
    ) handler

testFromUTF8Failure6Bytes = do
  let utf8 = [0xFD, 0xBF, 0xBF, 0xBF, 0xBF, 0xBF]
  let handler :: ErrorCall -> IO ()
      handler (ErrorCall msg) = assertEqual "testFromUTF8Failure6Bytes" "fromUTF8: illegal UTF-8 character 253" msg
  catch (do
    assertEqual "testFromUTF8Failure6Bytes" [chr 0x10FFFF] (fromUTF8 utf8)
    assertBool "testFromUTF8Failure6Bytes: no error raised" False
    ) handler
{-
testLargeFromUTF8 = do
  let sz = 1000000
  withCString (take sz (repeat 'a')) $ \cstr -> do
    s <- peekUTF8String cstr
    assertEqual "testLargeFromUTF8" sz (length s)
    s <- peekUTF8StringB cstr
    assertEqual "testLargeFromUTF8B" sz (length s)
    s <- peekUTF8StringLen (cstr, sz)
    assertEqual "testLargeFromUTF8Len" sz (length s)
    s <- peekUTF8StringLenB (cstr, sz)
    assertEqual "testLargeFromUTF8LenB" sz (length s)
-}

{-
testLargeFromUTF8Len = do
  let sz = 1000000
  withCStringLen (take sz (repeat 'a')) $ \(cstr, clen) -> do
    putStrLn "testLargeFromUTF8Len: call peekUTF8StringLen"
    s <- peekUTF8StringLen (cstr, clen)
    putStrLn "testLargeFromUTF8Len: assert length"
    assertEqual "testLargeFromUTF8Len" sz (length s)
-}

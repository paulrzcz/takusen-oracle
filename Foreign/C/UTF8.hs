
{-# LANGUAGE CPP #-}

-- |
-- Module      :  Foreign.C.UTF8
-- Copyright   :  (c) 2004 John Meacham, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  alistair@abayley.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Marshall Haskell Strings to and from UTF8-encoded CStrings.
-- This module's code is inspired by John Meacham's UTF8 en- & de-coders,
-- and also those found in the HXT library (module Text.XML.HXT.DOM.Unicode).
-- 
-- Note that the -Len functions all return the length in bytes,
-- not Chars (this is more useful, as you are most likely to want
-- to pass the length to an FFI function, which is most likely
-- expecting the length in bytes). If you want the length in Chars,
-- well, you have the original String, so...


-- This module has been reasonably optimised. You can check GHC's
-- simplifier output (for unboxing, mainly) with this:
--   ghc -O2 -c UTF8.hs -ddump-simpl > simpl.txt

module Foreign.C.UTF8
  ( peekUTF8String, peekUTF8StringLen
  , newUTF8String, withUTF8String, withUTF8StringLen
  , toUTF8String, fromUTF8String
  , lengthUTF8, fromUTF8, toUTF8
  ) where

import Control.Monad (when, liftM)
import Data.Bits
import Data.Char
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

#ifdef __GLASGOW_HASKELL__
import GHC.Base (unsafeChr)
#else
unsafeChr :: Int -> Char
unsafeChr i = chr i
#endif


nullCChar :: CChar
nullCChar = 0

nullByte :: Word8
nullByte = 0

-- | Analogous to peekCString. Converts UTF8 CString to String.
peekUTF8String :: CString -> IO String
peekUTF8String cs = fromUTF8Ptr0 (castPtr cs)
--peekUTF8String cs = peekArray0 nullByte (castPtr cs) >>= return . fromUTF8

-- | Analogous to peekCStringLen. Converts UTF8 CString to String.
-- The resulting String will end either when @len@ bytes
-- have been converted, or when a NULL is found.
peekUTF8StringLen :: CStringLen -> IO String
peekUTF8StringLen (cs, len) = fromUTF8Ptr (len-1) (castPtr cs) ""
--peekUTF8StringLen (cs, len) = peekArray len (castPtr cs) >>= return . fromUTF8

-- | Analogous to newCString. Creates UTF8 encoded CString.
newUTF8String :: String -> IO CString
newUTF8String hs = do
  p <- newArray0 nullByte (toUTF8 hs)
  return (castPtr p)

-- | Analogous to newCStringLen.
-- The length returned is in bytes (encoding units), not chars.
newUTF8StringLen :: String -> IO CStringLen
newUTF8StringLen hs = do
  let utf8 = toUTF8 hs
  p <- newArray0 nullByte utf8
  return (castPtr p, length utf8)

-- | Analogous to withCString. Creates UTF8 encoded CString.
withUTF8String :: String -> (CString -> IO a) -> IO a
withUTF8String s action =
  withUTF8StringLen s (\(cstr, _) -> action cstr)

-- | Analogous to withCStringLen.
-- The length returned is in bytes (encoding units), not chars.
withUTF8StringLen :: String -> (CStringLen -> IO a) -> IO a
withUTF8StringLen s action = do
  let utf8 = toUTF8 s
  withArray0 nullByte utf8
    (\arr -> action (castPtr arr, length utf8) )

-- | Convert a String that was marshalled from a CString without
-- any decoder applied. This might be useful if the client encoding
-- is unknown, and the user code must convert.
-- We assume that the UTF8 CString was marshalled as if Latin-1
-- i.e. all chars are in the range 0-255.
fromUTF8String :: String -> String
fromUTF8String = fromUTF8 . map charToWord8

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum

-- | Convert a Haskell String into a UTF8 String, where each UTF8 byte
-- is represented by its Char equivalent i.e. only chars 0-255 are used.
-- The resulting String can be marshalled to CString directly i.e. with
-- a Latin-1 encoding.
toUTF8String :: String -> String
toUTF8String = map word8ToChar . toUTF8

word8ToChar :: Word8 -> Char
word8ToChar = unsafeChr . fromIntegral


lengthUTF8 :: String -> Int
lengthUTF8 s = length (toUTF8 s)

{-
The codepoint-to-UTF8 rules:
0x00 - 0x7f: 7 bits: as is
0x80 - 0x07ff: 11 bits
  byte 1: 0xC0 OR ((x <<  6) AND 0x1F)  i.e. 0xC0 + bits  7-11 (bits 12-up are 0)
  byte 2: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
0x0800 - 0xFFFF: 16 bits
  byte 1: 0xE0 OR ((x << 12) AND 0x0F)  i.e. 0xE0 + bits 13-16
  byte 2: 0x80 OR ((x <<  6) AND 0x3F)  i.e. 0x80 + bits  7-12
  byte 3: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
0x00010000 - 0x001FFFFF: 21 bits
  byte 1: 0xF0 OR ((x << 18) AND 0x07)  i.e. 0xF0 + bits 19-21
  byte 2: 0x80 OR ((x << 12) AND 0x3F)  i.e. 0x80 + bits 13-18
  byte 3: 0x80 OR ((x <<  6) AND 0x3F)  i.e. 0x80 + bits  7-12
  byte 4: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
0x00200000 - 0x03FFFFFF: 26 bits
  byte 1: 0xF8 OR ((x << 24) AND 0x03)  i.e. 0xF8 + bits 25-26
  byte 2: 0x80 OR ((x << 18) AND 0x3F)  i.e. 0x80 + bits 19-24
  byte 3: 0x80 OR ((x << 12) AND 0x3F)  i.e. 0x80 + bits 13-18
  byte 4: 0x80 OR ((x <<  6) AND 0x3F)  i.e. 0x80 + bits  7-12
  byte 5: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
0x04000000 - 0x7FFFFFFF: 31 bits
  byte 1: 0xFC OR ((x << 30) AND 0x01)  i.e. 0xFC + bit  31
  byte 2: 0x80 OR ((x << 24) AND 0x3F)  i.e. 0x80 + bits 25-30
  byte 3: 0x80 OR ((x << 18) AND 0x3F)  i.e. 0x80 + bits 19-24
  byte 4: 0x80 OR ((x << 12) AND 0x3F)  i.e. 0x80 + bits 13-18
  byte 5: 0x80 OR ((x <<  6) AND 0x3F)  i.e. 0x80 + bits  7-12
  byte 6: 0x80 OR (x         AND 0x3F)  i.e. 0x80 + bits  1-6
-}

-- | Convert Unicode characters to UTF-8.
toUTF8 :: String -> [Word8]
toUTF8 [] = []
toUTF8 (x:xs) = toUTF8' (ord x) where
  toUTF8' x
      | x <= 0x0000007F = fromIntegral x : more
      | x <= 0x000007FF
        = w8 0xC0  6 : w8 0x80  0 : more
      | x <= 0x0000FFFF
        = w8 0xE0 12 : w8 0x80  6 : w8 0x80  0 : more
      -- If we want to encode chars > 1114111 then this test should be
      --   x <= 0x001FFFFF
      -- because that's the upper limit of the 4-byte encoding
      -- (and the 5- and 6-byte cases below might also be enabled).
      | x <= 0x0010FFFF
        = w8 0xF0 18 : w8 0x80 12 : w8 0x80  6 : w8 0x80  0 : more
      | otherwise = error ("toUTF8: codepoint " ++ show x
         ++ " is greater than the largest allowed (decimal 1114111, hex 0x10FFFF).")
      {-
      -- Potentially useful code, if Haskell ever supports codepoints > 0x0010FFFF.
      -- There are no tests for this, because we can't create Strings containing
      -- chars > 0x0010FFFF.
      | x <= 0x03FFFFFF
        = w8 0xF8 24 : w8 0x80 18 : w8 0x80 12 : w8 0x80  6 : w8 0x80  0 : more
      | x <= 0x7FFFFFFF
        = w8 0xFC 30 : w8 0x80 24 : w8 0x80 18 : w8 0x80 12 : w8 0x80  6 : w8 0x80  0 : more
      | otherwise = error ("toUTF8: codepoint " ++ show x ++ " is greater "
         ++ "then the largest that can be represented by UTF8 encoding"
         ++ "(decimal 2147483647, hex 0x7FFFFFFF).")
      -}
    where
    more = toUTF8 xs
    w8 :: Word8 -> Int -> Word8
    w8 base rshift = base .|. (fromIntegral (shiftR x rshift) .&. mask)
      where
      mask
        | base == 0x80 = 0x3F
        | base == 0xC0 = 0x1F
        | base == 0xE0 = 0x0F
        | base == 0xF0 = 0x07
        | base == 0xF8 = 0x03
        | base == 0xFC = 0x01

{-
And the rules for UTF8-to-codepoint:
examine first byte:
0x00-0x7F: 1 byte: as-is
0x80-0xBF: error (surrogate)
0xC0-0xDF: 2 bytes: b1 AND 0x1F + remaining
0xE0-0xEF: 3 bytes: b1 AND 0x0F + remaining
0xF0-0xF7: 4 bytes: b1 AND 0x07 + remaining
0xF8-0xFB: 5 bytes: b1 AND 0x03 + remaining
0xFC-0xFD: 6 bytes: b1 AND 0x01 + remaining
0xFE-0xFF: error
  (byte-order-mark indicators: UTF8 - EFBBBF, UTF16 - FEFF or FFFE)
remaining = lower 6 bits of each byte, concatenated
-}

-- | Convert UTF-8 to Unicode.
fromUTF8 :: [Word8] -> String
fromUTF8 [] = ""
fromUTF8 (x:xs)
      | x <= 0x7F = remaining 0 (fromIntegral x) xs
      | x <= 0xBF = err x
      | x <= 0xDF = remaining 1 (bAND x 0x1F) xs
      | x <= 0xEF = remaining 2 (bAND x 0x0F) xs
      | x <= 0xF7 = remaining 3 (bAND x 0x07) xs
      | otherwise = err x
      {-
      -- Again, only works for chars > 0x0010FFFF, which we can't test.
      | x <= 0xFB = remaining 4 (bAND x 0x03) xs
      | x <= 0xFD = remaining 5 (bAND x 0x01) xs
      | otherwise = err x
      -}
  where
    err x = error ("fromUTF8: illegal UTF-8 character " ++ show x)
    bAND :: Word8 -> Word8 -> Int
    bAND x m = fromIntegral (x .&. m)
    remaining :: Int -> Int -> [Word8] -> String
    remaining 0 x xs = chr x : fromUTF8 xs
    remaining n x [] = error "fromUTF8: incomplete UTF8 sequence"
    remaining n x (b:xs)
      | b == 0 = err x
      | otherwise = remaining (n-1) ((shiftL x 6) .|. (bAND b 0x3F)) xs

{-
This version of fromUTF8Ptr starts at the end of the array and works backwards.
This will allow us to create the result String with constant space
usage. Contrast this with creating the String by processing the array
from start to end: in this case we would probably use an accumulating
parameter, and reverse the list when we reach the end of the array.
This isn't so bad, if we expect reverse to work in constant space
(more or less).
-}

-- | Convert UTF-8 to Unicode, from a null-terminated C array of bytes.
-- This function is useful, in addition to 'fromUTF8' above,
-- because it doesn't create an intermediate @[Word8]@ list.
fromUTF8Ptr0 :: Ptr Word8 -> IO String
fromUTF8Ptr0 p = do
  len <- lengthArray0 nullByte p
  fromUTF8Ptr (len-1) p ""

-- | The bytes parameter should be len-1
-- i.e. if the CString has length 2, then you should pass bytes=1.
-- That's because we add bytes to the Ptr p to get the offset
-- for each byte; byte 1 is at p+0, byte 2 is at p+1, etc.
fromUTF8Ptr :: Int -> Ptr Word8 -> String -> IO String
fromUTF8Ptr bytes p acc
  | bytes `seq` p `seq` acc `seq` False = undefined
  | bytes < 0 = do
  if null acc then return acc
    else  -- BOM = chr 65279 ( EF BB BF )
    if head acc /= chr 65279 then return acc
    else return (tail acc)
  | otherwise = do
  x <- liftM fromIntegral (peekElemOff p bytes)
  case () of
    _ | x == 0 -> error ("fromUTF8Ptr: zero byte found in string as position " ++ show bytes)
      | x <= 0x7F -> fromUTF8Ptr (bytes-1) p (unsafeChr x:acc)
      | x <= 0xBF && bytes == 0 -> error "fromUTF8Ptr: surrogate at start of string"
      | x <= 0xBF -> fromUTF8Ptr (bytes-1) p acc
      | otherwise -> do
          c <- readUTF8Char x bytes p
          fromUTF8Ptr (bytes-1) p (c:acc)


readUTF8Char :: Int -> Int -> Ptr Word8 -> IO Char
readUTF8Char x offset p
  | x `seq` offset `seq` p `seq` False = undefined
  | otherwise =
  case () of
    _ | x == 0 -> err x
      | x <= 0x7F -> return (unsafeChr x)
      | x <= 0xBF -> err x
      | x <= 0xDF -> do
          x1 <- liftM fromIntegral (peekElemOff p (offset + 1))
          return (unsafeChr (
            ((x - 0xC0) * 64)
            + (x1 - 0x80)
            ))
      | x <= 0xEF -> do
          x1 <- liftM fromIntegral (peekElemOff p (offset + 1))
          x2 <- liftM fromIntegral (peekElemOff p (offset + 2))
          return (unsafeChr (
            ((x - 0xE0) * 4096)
            + ((x1 - 0x80) * 64)
            + (x2 - 0x80)
            ))
      | x <= 0xF7 -> do
          x1 <- liftM fromIntegral (peekElemOff p (offset + 1))
          x2 <- liftM fromIntegral (peekElemOff p (offset + 2))
          x3 <- liftM fromIntegral (peekElemOff p (offset + 3))
          return (unsafeChr (
            ((x - 0xF0) * 262144)
            + ((x1 - 0x80) * 4096)
            + ((x2 - 0x80) * 64)
            + (x3 - 0x80)
            ))
     | otherwise -> err x
  where
    err x = error ("readUTF8Char: illegal UTF-8 character " ++ show x)

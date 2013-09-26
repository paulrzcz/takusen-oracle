-- Converts .lhs (literary Haskell files) to .hs (plain Haskell files)
-- Keeps only the statements which are normally compiled, plus blank lines.

-- To use:
--    ghc --make lhs2hs.hs
-- to get an executable file lhs2hs.  
-- Then 
--    lhs2hs filename
-- will open filename.lhs and save the converted file in filename.hs

-- by Scot Drysdale on 7/28/07, based on SOE program on p. 241

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (unless)
import Control.Exception
import System.Environment 
import System.IO
import System.IO.Error (isEOFError)

-- Opens a file, given name and mode
openGivenFile :: String -> IOMode -> IO Handle
openGivenFile name mode 
  = catch (openFile name mode)
          (\(_ :: SomeException) -> error ("Cannot open " ++ name))
 
main :: IO ()
main = do args <- getArgs
          fromHandle <- openGivenFile (head args ++ ".lhs") ReadMode
          toHandle <- openGivenFile (head args ++ ".hs") WriteMode
          convertFile fromHandle toHandle
          hClose fromHandle
          hClose toHandle

-- Converts all the lines in a file
convertFile :: Handle -> Handle -> IO ()
convertFile fromHandle toHandle 
  = catch (do line <- hGetLine fromHandle
              case line of
                ('>' : ' ' : rest) -> hPutStrLn toHandle rest
                ('>' : rest)       -> hPutStrLn toHandle rest
                ('\n' : _)      -> hPutStrLn toHandle line
                ('\r' : _)      -> hPutStrLn toHandle line
                _                  -> hPutStrLn toHandle ("-- "++line)
              convertFile fromHandle toHandle)
           (\e -> unless (isEOFError e) $ ioError e)
       
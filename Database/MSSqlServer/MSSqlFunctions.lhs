
|
Module      :  Database.MSSqlServer.MSSqlFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Simple wrappers for MS Sql Server functions (FFI).
 
 > -L"C:\Program Files\Common Files\System\Ole DB" -lsqloledb
 
Don't forget to specify the header file location
(or wherever your Sql Server installation resides):
 
 > "-IC:\Program Files\Microsoft SQL Server\80\Tools\DevTools\Include"
 

> {-# OPTIONS -ffi #-}
> {-# OPTIONS -fglasgow-exts #-}

From C:\WINDOWS\hh.exe "C:\Program Files\Microsoft SQL Server\80\Tools\Books\SQL80.col"
 -- Compiling OLE DB Applications --
"Most applications use wide character strings to make OLE DB function calls.
If applications are using TCHAR variables, the application must include
#define UNICODE in the application. It converts the TCHAR variables to 
wide character strings."

> {-# OPTIONS -optc-DUNICODE #-}

Do we need this?
 {-# OPTIONS -#include "windows.h" #-}

> {-# OPTIONS -#include "sqloledb.h" #-}
> {-# OPTIONS -#include "oledberr.h" #-}

Don't need to explcitly include sqldb.h, as it's implied by the
FFI foreign import declarations e.g.
  > foreign import ccall "oledb.h dbinit" dbInit :: IO CString
implies
 {- # OPTIONS -#include "oledb.h" #-}


> module Database.MSSqlServer.MSSqlFunctions where

> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Data.Dynamic
> import Data.Int
> import Data.IORef



> data MSSqlException = MSSqlException Int String
>   deriving (Typeable, Eq)

> instance Show MSSqlException where
>   show (MSSqlException i s) = "MSSqlException " ++ (show i) ++ " " ++ s

> makeEx :: CInt -> String -> MSSqlException
> makeEx e msg = MSSqlException (fromIntegral e) msg

> catchMSSql :: IO a -> (MSSqlException -> IO a) -> IO a
> catchMSSql = catchDyn

> throwMSSql :: MSSqlException -> a
> throwMSSql = throwDyn

> throwEx :: CInt -> String -> IO a
> throwEx e msg = throwMSSql (makeEx e msg)

> testForError :: CInt -> String -> a -> IO a
> testForError rc msg retval = do
>   case () of
>     _ | rc == dbSUCCEED -> return retval
>       | otherwise -> throwEx rc msg

> errorOnNull :: Ptr b -> String -> a -> IO a
> errorOnNull ptr msg retval = do
>   if ptr == nullPtr
>     then throwEx (-1 ) msg
>     else return retval

> cStr :: CStringLen -> CString
> cStr = fst
> cStrLen :: CStringLen -> CInt
> cStrLen = fromIntegral . snd


 foreign import ccall "sqldb.h dbinit" dbInit :: IO CString

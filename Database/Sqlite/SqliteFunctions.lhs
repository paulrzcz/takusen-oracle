
|
Module      :  Database.Sqlite.SqliteFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Simple wrappers for Sqlite functions (FFI).


> {-# LANGUAGE ForeignFunctionInterface #-}

> module Database.Sqlite.SqliteFunctions where

> import Prelude hiding (catch)
> import Foreign.C.UTF8
> import Foreign
> import Foreign.C
> import Foreign.Ptr
> import Control.Monad
> import Control.Exception.Extensible
> import Data.Dynamic
> import Data.Int


> data DBHandleStruct = DBHandleStruct
> type DBHandle = Ptr DBHandleStruct
> data StmtStruct = StmtStruct
> type StmtHandle = Ptr StmtStruct
> type Blob = Ptr Word8

> type SqliteCallback a = FunPtr (Ptr a -> CInt -> Ptr CString -> Ptr CString -> IO Int)
> type FreeFunPtr = FunPtr ( Ptr Word8 -> IO () )

> data SqliteException = SqliteException Int String
>   deriving (Typeable)

> instance Show SqliteException where
>   show (SqliteException i s) = "SqliteException " ++ (show i) ++ " " ++ s

> catchSqlite :: IO a -> (SqliteException -> IO a) -> IO a
> throwSqlite :: SqliteException -> a
> instance Exception SqliteException
> catchSqlite = catch
> throwSqlite = throw


> sqliteOK :: CInt
> sqliteOK = 0
> sqliteERROR :: CInt
> sqliteERROR = 1
> sqliteROW :: CInt
> sqliteROW = 100
> sqliteDONE :: CInt
> sqliteDONE = 101

> cStr :: CStringLen -> CString
> cStr = fst
> cStrLen :: CStringLen -> CInt
> cStrLen = fromIntegral . snd

Apparently Sqlite's UTF16 is in "host native byte order",
whatever that means.

> type UTF16CString = CString
> type UTF8CString = CString


> foreign import ccall "sqlite.h sqlite3_open" sqliteOpen
>   :: UTF8CString -> Ptr DBHandle -> IO CInt

> foreign import ccall "sqlite.h sqlite3_close" sqliteClose
>   :: DBHandle -> IO CInt

> foreign import ccall "sqlite.h sqlite3_prepare" sqlitePrepare
>   :: DBHandle -> UTF8CString -> CInt -> Ptr StmtHandle -> Ptr CString -> IO CInt

> foreign import ccall "sqlite.h sqlite3_exec" sqliteExec
>   :: DBHandle -> UTF8CString -> SqliteCallback a -> Ptr a -> Ptr CString -> IO CInt

> foreign import ccall "sqlite.h sqlite3_column_count" sqliteColumnCount
>   :: StmtHandle -> IO CInt

> foreign import ccall "sqlite.h sqlite3_step" sqliteStep
>   :: StmtHandle -> IO CInt

> foreign import ccall "sqlite.h sqlite3_finalize" sqliteFinalise
>   :: StmtHandle -> IO CInt

> foreign import ccall "sqlite.h sqlite3_reset" sqliteReset
>   :: StmtHandle -> IO CInt

> foreign import ccall "sqlite.h sqlite3_changes" sqliteChanges
>   :: DBHandle -> IO CInt

> foreign import ccall "sqlite.h sqlite3_last_insert_rowid" sqliteLastInsertRowid
>   :: DBHandle -> IO CLLong

> foreign import ccall "sqlite.h sqlite3_free" sqliteFree
>   :: Ptr a -> IO ()

> foreign import ccall "sqlite.h sqlite3_errcode" sqliteErrcode
>   :: DBHandle -> IO CInt

> foreign import ccall "sqlite.h sqlite3_errmsg" sqliteErrmsg
>   :: DBHandle -> IO UTF8CString

column_bytes tells us how big a value is in the result set.
 * For blobs it's the blob size.
 * For strings, the string is converted to UTF-8 and then the size in bytes is given.
 * There's a "16" version which converts to UTF-16. The terminating Null isn't counted.
 * For ints and doubles the size of the result after conversion to string is returned
   (well, we already know how many bytes the raw value requires, don't we?)

> foreign import ccall "sqlite.h sqlite3_column_bytes" sqliteColumnBytes
>   :: StmtHandle -> CInt -> IO Int

> foreign import ccall "sqlite.h sqlite3_column_blob" sqliteColumnBlob
>   :: StmtHandle -> CInt -> IO Blob

> foreign import ccall "sqlite.h sqlite3_column_double" sqliteColumnDouble
>   :: StmtHandle -> CInt -> IO CDouble

> foreign import ccall "sqlite.h sqlite3_column_int" sqliteColumnInt
>   :: StmtHandle -> CInt -> IO CInt

> foreign import ccall "sqlite.h sqlite3_column_int64" sqliteColumnInt64
>   :: StmtHandle -> CInt -> IO CLLong

> foreign import ccall "sqlite.h sqlite3_column_text" sqliteColumnText
>   :: StmtHandle -> CInt -> IO UTF8CString

> foreign import ccall "sqlite.h sqlite3_column_text16" sqliteColumnText16
>   :: StmtHandle -> CInt -> IO UTF16CString




> foreign import ccall "sqlite.h sqlite3_bind_blob" sqliteBindBlob
>   :: StmtHandle -> CInt -> Blob -> CInt -> FreeFunPtr -> IO CInt

> foreign import ccall "sqlite.h sqlite3_bind_double" sqliteBindDouble
>   :: StmtHandle -> CInt -> CDouble -> IO CInt

> foreign import ccall "sqlite.h sqlite3_bind_int" sqliteBindInt
>   :: StmtHandle -> CInt -> CInt -> IO CInt

> foreign import ccall "sqlite.h sqlite3_bind_int64" sqliteBindInt64
>   :: StmtHandle -> CInt -> CLLong -> IO CInt

> foreign import ccall "sqlite.h sqlite3_bind_null" sqliteBindNull
>   :: StmtHandle -> CInt -> IO CInt

> foreign import ccall "sqlite.h sqlite3_bind_text" sqliteBindText
>   :: StmtHandle -> CInt -> UTF8CString -> CInt -> FreeFunPtr -> IO CInt

> foreign import ccall "sqlite.h sqlite3_bind_text16" sqliteBindText16
>   :: StmtHandle -> CInt -> UTF16CString -> CInt -> FreeFunPtr -> IO CInt


-------------------------------------------------------------------

> getError :: DBHandle -> IO SqliteException
> getError db = do
>   errcodec <- sqliteErrcode db
>   errmsgc <- sqliteErrmsg db
>   errmsg <- peekUTF8String errmsgc
>   return $ SqliteException (fromIntegral errcodec) errmsg

> getAndRaiseError :: Int -> DBHandle -> IO a
> getAndRaiseError rc db = do
>   ex@(SqliteException e m) <- getError db
>   if e == 0
>     then throwSqlite (SqliteException rc m)
>     else throwSqlite ex
>   return undefined

> errorTest :: DBHandle -> CInt -> IO a -> IO a
> errorTest db rc action = do
>   case () of
>     _ | rc == sqliteOK -> action
>       | rc == sqliteDONE -> action
>       | rc == sqliteROW -> action
>       | otherwise -> getAndRaiseError (fromIntegral rc) db


> testForError :: DBHandle -> CInt -> a -> IO a
> testForError db rc retval = errorTest db rc (return retval)

> testForErrorWithPtr :: (Storable a) => DBHandle -> CInt -> Ptr a -> IO a
> testForErrorWithPtr db rc ptr = errorTest db rc (peek ptr >>= return)


> openDb :: String -> IO DBHandle
> openDb dbName =
>   withUTF8String dbName $ \cstr ->
>   alloca $ \dbptr -> do
>   rc <- sqliteOpen cstr dbptr
>   if dbptr == nullPtr
>     then do
>       throwSqlite (SqliteException (fromIntegral rc) "Null handle returned when opening database")
>       return undefined
>     else do
>     db <- peek dbptr
>     if rc == sqliteOK
>       then return db
>       else do
>         ex <- getError db
>         _ <- sqliteClose db
>         throwSqlite ex
>         return undefined

> closeDb :: DBHandle -> IO ()
> closeDb db = do
>   rc <- sqliteClose db
>   testForError db rc ()


| This function is not used internally, so it's only provided
as a user convenience.

> stmtExec :: DBHandle -> String -> IO Int
> stmtExec db sqlText =
>   withUTF8String sqlText $ \cstr -> do
>     rc <- sqliteExec db cstr nullFunPtr nullPtr nullPtr
>     rows <- sqliteChanges db
>     testForError db rc (fromIntegral rows)

> stmtChanges :: DBHandle -> IO Int
> stmtChanges db = do
>   rows <- sqliteChanges db
>   return (fromIntegral rows)

> stmtPrepare :: DBHandle -> String -> IO StmtHandle
> stmtPrepare db sqlText =
>   withUTF8StringLen sqlText $ \(cstr, clen) ->
>   alloca $ \stmtptr ->
>   alloca $ \unusedptr -> do
>     rc <- sqlitePrepare db cstr (fromIntegral clen) stmtptr unusedptr
>     testForErrorWithPtr db rc stmtptr

> stmtFetch :: DBHandle -> StmtHandle -> IO CInt
> stmtFetch db stmt = do
>   rc <- sqliteStep stmt
>   testForError db rc rc

> stmtFinalise :: DBHandle -> StmtHandle -> IO ()
> stmtFinalise db stmt = do
>   rc <- sqliteFinalise stmt
>   testForError db rc ()

> stmtReset :: DBHandle -> StmtHandle -> IO ()
> stmtReset db stmt = do
>   rc <- sqliteReset stmt
>   testForError db rc ()

> checkColumnRange :: StmtHandle -> Int -> IO ()
> checkColumnRange stmt col = do
>   nc <- sqliteColumnCount stmt
>   if (fromIntegral nc < col) || col < 1
>     then throwSqlite (SqliteException (-1) ("Attempted fetch from invalid column number " ++ show col))
>     else return ()


|Column numbers are zero-indexed, so subtract one
from given index (we present a one-indexed interface).

> colValInt :: StmtHandle -> Int -> IO Int
> colValInt stmt colnum = do
>   checkColumnRange stmt colnum
>   cint <- sqliteColumnInt stmt (fromIntegral (colnum - 1))
>   return (fromIntegral cint)

> colValInt64 :: StmtHandle -> Int -> IO Int64
> colValInt64 stmt colnum = do
>   checkColumnRange stmt colnum
>   cllong <- sqliteColumnInt64 stmt (fromIntegral (colnum - 1))
>   return (fromIntegral cllong)

> colValDouble :: StmtHandle -> Int -> IO Double
> colValDouble stmt colnum = do
>   checkColumnRange stmt colnum
>   cdbl <- sqliteColumnDouble stmt (fromIntegral (colnum - 1))
>   return (realToFrac cdbl)

> colValString :: StmtHandle -> Int -> IO (Maybe String)
> colValString stmt colnum = do
>   checkColumnRange stmt colnum
>   cstrptr <- sqliteColumnText stmt (fromIntegral (colnum - 1))
>   if cstrptr == nullPtr
>     then return Nothing
>     else do
>       str <- peekUTF8String cstrptr
>       return (Just str)

> colValBlob :: StmtHandle -> Int -> IO (ForeignPtr Blob)
> colValBlob stmt colnum = do
>   checkColumnRange stmt colnum
>   let ccolnum = fromIntegral (colnum - 1)
>   bytes <- sqliteColumnBytes stmt ccolnum
>   src <- sqliteColumnBlob stmt ccolnum
>   buffer <- mallocForeignPtrBytes bytes
>   withForeignPtr buffer $ \dest -> copyBytes dest src bytes
>   return (castForeignPtr buffer)


Unlike column numbers, bind positions are 1-indexed,
so there's no need to subtract one from the given position.

> bindDouble :: DBHandle -> StmtHandle -> Int -> Double -> IO ()
> bindDouble db stmt pos value = do
>   rc <- sqliteBindDouble stmt (fromIntegral pos) (realToFrac value)
>   testForError db rc ()

> bindInt :: DBHandle -> StmtHandle -> Int -> Int -> IO ()
> bindInt db stmt pos value = do
>   rc <- sqliteBindInt stmt (fromIntegral pos) (fromIntegral value)
>   testForError db rc ()

> bindInt64 :: DBHandle -> StmtHandle -> Int -> Int64 -> IO ()
> bindInt64 db stmt pos value = do
>   rc <- sqliteBindInt64 stmt (fromIntegral pos) (fromIntegral value)
>   testForError db rc ()

> bindNull :: DBHandle -> StmtHandle -> Int -> IO ()
> bindNull db stmt pos = do
>   rc <- sqliteBindNull stmt (fromIntegral pos)
>   testForError db rc ()

> bindString :: DBHandle -> StmtHandle -> Int -> String -> IO ()
> bindString db stmt pos value =
>   withUTF8StringLen value $ \(cstr, clen) -> do
>     rc <- sqliteBindText stmt (fromIntegral pos) cstr (fromIntegral clen) nullFunPtr
>     testForError db rc ()

> bindBlob :: DBHandle -> StmtHandle -> Int -> Blob -> Int -> IO ()
> bindBlob db stmt pos value size = do
>   rc <- sqliteBindBlob stmt (fromIntegral pos) value (fromIntegral size) nullFunPtr
>   testForError db rc ()

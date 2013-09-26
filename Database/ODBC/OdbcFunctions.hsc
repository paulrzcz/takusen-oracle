
{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#ifdef mingw32_HOST_OS
#include <windows.h>
#endif
#include <sql.h>
#include <sqlext.h>

#ifdef mingw32_HOST_OS
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

{-
http://msdn.microsoft.com/library/default.asp?url=/library/en-us/odbc/htm/odbcodbc_api_reference.asp
http://www.dbmaker.com.tw/reference/manuals/odbc/odbc_chap_04.html
-}


-- |
-- Module      :  Database.ODBC.OdbcFunctions
-- Copyright   :  (c) 2007 Oleg Kiselyov, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  oleg@pobox.com, alistair@abayley.org
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- Wrappers for ODBC FFI functions, plus buffer marshaling.

module Database.ODBC.OdbcFunctions where

import Prelude hiding (catch)
import Control.Exception.Extensible
import Control.Monad
import Data.Dynamic
import Data.IORef
import Data.Time
import Database.Util
import Foreign
import Foreign.C
import Foreign.C.UTF8

--debugStmt s = putStrLn s
debugStmt s = return ()

data HandleObj = HandleObj
type Handle = Ptr HandleObj
data EnvObj = EnvObj
type EnvHandle = Ptr EnvObj
-- We store the dbms name name in the connection and stmt objects.
-- This allows us to customise behaviour based on the dbms we're
-- connected to. At present we use this when binding datetime
-- parameters for MS SQL Server.
data ConnObj = ConnObj
type ConnHdl = Ptr ConnObj
data ConnHandle = ConnHandle { connHdl :: ConnHdl, connDbms :: String, connCharEnc :: IORef CharEncoding }
data StmtObj = StmtObj
type StmtHdl = Ptr StmtObj
data StmtHandle = StmtHandle { stmtHdl :: StmtHdl, stmtDbms :: String, stmtCharEnc :: IORef CharEncoding }
type WindowHandle = Ptr ()
data Buffer = Buffer
type BufferFPtr = ForeignPtr Buffer
type SizeFPtr = ForeignPtr SqlLen


-- We need a way for user code to specify what charset is expected.
-- A bit limited, but it works for most DBMSs...
data CharEncoding = EncLatin1 | EncUTF8 | EncUTF16

-- Helper functions for String marshalling

withEncStringLen :: CharEncoding -> String -> (CStringLen -> IO a) -> IO a
withEncStringLen charenc str action = do
  case charenc of
    EncLatin1 -> withCAStringLen str action
    EncUTF8   -> withUTF8StringLen str action
    EncUTF16  -> withCWStringLen str (\(cwstr, clen) -> action (castPtr cwstr, clen))

peekEncString :: CharEncoding -> CString -> IO String
peekEncString charenc cstr = do
  case charenc of
    EncLatin1 -> peekCAString cstr
    EncUTF8   -> peekUTF8String cstr
    EncUTF16  -> peekCWString (castPtr cstr)

peekEncStringLen :: CharEncoding -> CStringLen -> IO String
peekEncStringLen charenc cstrlen = do
  case charenc of
    EncLatin1 -> peekCAStringLen cstrlen
    EncUTF8   -> peekUTF8StringLen cstrlen
    EncUTF16  -> peekCWStringLen (cstr2cwstr cstrlen)
  where
    cstr2cwstr :: CStringLen -> CWStringLen
    cstr2cwstr (cstr, clen) = (castPtr cstr, clen)



data BindBuffer = BindBuffer
  { bindBufPtr :: BufferFPtr
  , bindBufSzPtr :: SizeFPtr
  , bindBufSize :: SqlLen
  , bindBufCharEnc :: CharEncoding
  }

type SqlInteger = #{type SQLINTEGER}
type SqlUInteger = #{type SQLUINTEGER}
type SqlSmallInt = #{type SQLSMALLINT}
type SqlUSmallInt = #{type SQLUSMALLINT}
type SqlLen = #{type SQLLEN}
type SqlULen = #{type SQLULEN}
type SqlReturn = SqlSmallInt
type SqlHandleType = SqlSmallInt
type SqlDataType = SqlSmallInt
type SqlCDataType = SqlSmallInt
type SqlParamDirection = SqlSmallInt
type SqlInfoType = SqlUSmallInt

-- Return codes from API calls

(
  sqlRcInvalidHandle :
  sqlRcStillExecuting :
  sqlRcSuccess :
  sqlRcSuccessWithInfo :
  sqlRcError :
  sqlRcNeedData :
  sqlRcNoData :
--  []) = [-2,-1,0,1,2,99,100] :: [SqlReturn]
  []) =
  (
  #{const SQL_INVALID_HANDLE} :
  #{const SQL_STILL_EXECUTING} :
  #{const SQL_SUCCESS} :
  #{const SQL_SUCCESS_WITH_INFO} :
  #{const SQL_ERROR} :
  #{const SQL_NEED_DATA} :
  #{const SQL_NO_DATA} :
  []) :: [SqlReturn]

showReturnCode rc
  | rc == #{const SQL_INVALID_HANDLE} = "SQL_INVALID_HANDLE"
  | rc == #{const SQL_STILL_EXECUTING} = "SQL_STILL_EXECUTING"
  | rc == #{const SQL_SUCCESS} = "SQL_SUCCESS"
  | rc == #{const SQL_SUCCESS_WITH_INFO}  = "SQL_SUCCESS_WITH_INFO"
  | rc == #{const SQL_ERROR} = "SQL_ERROR"
  | rc == #{const SQL_NEED_DATA} = "SQL_NEED_DATA"
  | rc == #{const SQL_NO_DATA} = "SQL_NO_DATA"
  | otherwise = "UNKNOWN_RETURN_CODE"

-- There are only four handle types in ODBC.

(
  sqlHTypeEnv :
  sqlHTypeConn :
  sqlHTypeStmt :
  sqlHTypeDesc :
--  []) = [1..4] :: [SqlHandleType]
  []) =
  (
  #{const SQL_HANDLE_ENV} :
  #{const SQL_HANDLE_DBC} :
  #{const SQL_HANDLE_STMT} :
  #{const SQL_HANDLE_DESC} :
  []) :: [SqlHandleType]

sqlDriverNoPrompt :: SqlUSmallInt
sqlDriverNoPrompt = #{const SQL_DRIVER_NOPROMPT}

sqlNullTermedString :: SqlInteger
sqlNullTermedString = #{const SQL_NTS}

sqlNullData :: SqlLen
sqlNullData = #{const SQL_NULL_DATA}

sqlTransCommit :: SqlSmallInt
sqlTransCommit = #{const SQL_COMMIT}

sqlTransRollback :: SqlSmallInt
sqlTransRollback = #{const SQL_ROLLBACK}

sqlAutoCommitOn, sqlAutoCommitOff :: SqlInteger
sqlAutoCommitOn = #{const SQL_AUTOCOMMIT_ON}
sqlAutoCommitOff = #{const SQL_AUTOCOMMIT_OFF}

-- These are attribute types, which are passed as the second parameter
-- to sqlSetEnvAttr.

(
  sqlAttrOdbcVersion :
  sqlAttrAutoCommit :
  sqlAttrTxnIsolation :
  []) =
  (
  #{const SQL_ATTR_ODBC_VERSION} :
  #{const SQL_ATTR_AUTOCOMMIT} :
  #{const SQL_ATTR_TXN_ISOLATION} :
  []) :: [SqlInteger]

-- These are attribute values, which are passed as the third parameter
-- to sqlSetEnvAttr. Obviously that must accompany the relevant
-- attribute type.

(
  sqlOvOdbc3 :
  sqlTxnCapable :
  sqlDefaultTxnIsolation :
  sqlTxnIsolationOption :
  sqlTxnReadUncommitted :
  sqlTxnReadCommitted :
  sqlTxnRepeatableRead :
  sqlTxnSerializable :
  []) =
  (
  #{const SQL_OV_ODBC3} : -- 3 (UL)
  #{const SQL_TXN_CAPABLE} :  -- 46
  #{const SQL_DEFAULT_TXN_ISOLATION} :  -- 26
  #{const SQL_TXN_ISOLATION_OPTION} :  -- 72
  #{const SQL_TXN_READ_UNCOMMITTED} :  -- 1
  #{const SQL_TXN_READ_COMMITTED} :  -- 2
  #{const SQL_TXN_REPEATABLE_READ} :  -- 4
  #{const SQL_TXN_SERIALIZABLE} :  -- 8
  []) :: [SqlInteger]


-- ODBC SQL data types
(
  sqlDTypeChar :
  sqlDTypeVarchar :
  sqlDTypeInt :
  sqlDTypeBinary :
  sqlDTypeDouble :
  sqlDTypeDate :
  sqlDTypeTime :
  sqlDTypeTimestamp :
  sqlDTypeCursor :
  []) =
  (
  #{const SQL_CHAR} :  -- CHAR = 1, VARCHAR = 12, LONGVARCHAR = -1
  #{const SQL_VARCHAR} :  -- CHAR = 1, VARCHAR = 12, LONGVARCHAR = -1
  #{const SQL_INTEGER} :  -- 4
  #{const SQL_BINARY} :  -- -2
  #{const SQL_DOUBLE} :  -- 8
  #{const SQL_TYPE_DATE} :  -- 9
  #{const SQL_TYPE_TIME} :  -- 10
  #{const SQL_TYPE_TIMESTAMP} :  -- 11
  #{const SQL_CURSOR_TYPE} :  -- 6
  []) :: [SqlDataType]

-- host language (C) data types

(
  sqlCTypeString :
  sqlCTypeInt :
  sqlCTypeBinary :
  sqlCTypeDouble :
  sqlCTypeDate :
  sqlCTypeTime :
  sqlCTypeTimestamp :
  []) =
  (
  #{const SQL_C_CHAR} :
  #{const SQL_C_LONG} :
  #{const SQL_C_BINARY} :
  #{const SQL_C_DOUBLE} :
  #{const SQL_C_TYPE_DATE} :
  #{const SQL_C_TYPE_TIME} :
  #{const SQL_C_TYPE_TIMESTAMP} :
  []) :: [SqlCDataType]

{-
#define SQL_C_BINARY SQL_BINARY
#define SQL_C_BIT SQL_BIT
#define SQL_C_BOOKMARK SQL_C_ULONG
#define SQL_C_CHAR SQL_CHAR
#define SQL_C_DATE SQL_DATE
#define SQL_C_DEFAULT 99
#define SQL_C_DOUBLE SQL_DOUBLE
#define SQL_C_FLOAT SQL_REAL
#define SQL_C_LONG SQL_INTEGER
#define SQL_C_SHORT SQL_SMALLINT
#define SQL_C_SLONG (SQL_C_LONG+SQL_SIGNED_OFFSET)
#define SQL_C_SSHORT (SQL_C_SHORT+SQL_SIGNED_OFFSET)
#define SQL_C_STINYINT (SQL_TINYINT+SQL_SIGNED_OFFSET)
#define SQL_C_TIME SQL_TIME
#define SQL_C_TIMESTAMP SQL_TIMESTAMP
#define SQL_C_TINYINT SQL_TINYINT
#define SQL_C_ULONG (SQL_C_LONG+SQL_UNSIGNED_OFFSET)
#define SQL_C_USHORT (SQL_C_SHORT+SQL_UNSIGNED_OFFSET)
#define SQL_C_UTINYINT (SQL_TINYINT+SQL_UNSIGNED_OFFSET)
-}

-- Bind Parameter directions
(
  sqlParamInput :
  sqlParamInputOutput :
  sqlParamOutput :
  sqlParamDefault :
  sqlParamUnknown :
  [] ) =
  (
  #{const SQL_PARAM_INPUT} :
  #{const SQL_PARAM_INPUT_OUTPUT} :
  #{const SQL_PARAM_OUTPUT} :
  #{const SQL_PARAM_TYPE_DEFAULT} :
  #{const SQL_PARAM_TYPE_UNKNOWN} :
  [] ) :: [SqlParamDirection]


-- Information Types, for SQLGetInfo

(
  -- Driver info
  sqlInfoDriverHdbc :
  sqlInfoDriverHdesc :
  sqlInfoDriverHenv :
  sqlInfoDriverHlib :
  sqlInfoDriverHstmt :
  sqlInfoDriverName :
  sqlInfoDriverOdbcVer :
  sqlInfoDriverVer :
  sqlOdbcVer :
  -- DBMS product info
  sqlInfoDatabaseName :
  sqlInfoDbmsName :
  sqlInfoDbmsVer :
  [] ) =
  (
  #{const SQL_DRIVER_HDBC} :
  #{const SQL_DRIVER_HDESC} :
  #{const SQL_DRIVER_HENV} :
  #{const SQL_DRIVER_HLIB} :
  #{const SQL_DRIVER_HSTMT} :
  #{const SQL_DRIVER_NAME} :
  #{const SQL_DRIVER_ODBC_VER} :
  #{const SQL_DRIVER_VER} :
  #{const SQL_ODBC_VER} :
  #{const SQL_DATABASE_NAME} :
  #{const SQL_DBMS_NAME} :
  #{const SQL_DBMS_VER} :
  [] ) :: [SqlInfoType]


---------------------------------------------------------------------

data OdbcException = OdbcException Int String String [OdbcException]
  deriving (Typeable)

instance Show OdbcException where
  show (OdbcException i st s more) = "OdbcException "
    ++ (show i) ++ " " ++ st ++ " " ++ s
    -- Could print the entire chain of diag recs... is this a good idea?
    -- ++ (concat (map showOdbcExMsg more))
    --   where showOdbcExMsg (OdbcException i st s more) = s

catchOdbc :: IO a -> (OdbcException -> IO a) -> IO a
throwOdbc :: OdbcException -> a
instance Exception OdbcException
catchOdbc = catch
throwOdbc = throw


-- define SQL_SUCCEEDED(rc) (((rc)&(~1))==0)
sqlSucceeded rc = rc == sqlRcSuccess || rc == sqlRcSuccessWithInfo

getDiagRec :: SqlReturn -> SqlHandleType -> Handle -> SqlSmallInt -> IO [OdbcException]
getDiagRec retcode htype handle row =
  allocaBytes 6 $ \cstrState -> do
  alloca $ \errorNumPtr -> do
  allocaBytes 1025 $ \cstrMsg -> do
  alloca $ \msgLenPtr -> do
    rc <- sqlGetDiagRec htype handle row cstrState errorNumPtr cstrMsg 1024 msgLenPtr
    --debugStmt ("getDiagRec: rc=" ++ show rc)
    case () of
      _ | rc == sqlRcSuccess -> do
          errnum <- peek errorNumPtr
          state <- peekCStringLen (cstrState, 5)
          msglen <- peek msgLenPtr
          --debugStmt ("getDiagRec: msglen=" ++ show msglen)
          msg <- peekCStringLen (cstrMsg, fromIntegral msglen)
          --debugStmt ("getDiagRec: msg=" ++ msg)
          more <- getDiagRec retcode htype handle (row+1)
          return ((OdbcException (fromIntegral errnum) state msg []) : more)
        | rc == sqlRcNoData -> return []
        | otherwise -> return [OdbcException (fromIntegral rc) "01000" (showReturnCode retcode) []]

checkError :: SqlReturn -> SqlHandleType -> Handle -> IO ()
checkError rc htype handle =
  when (rc /= sqlRcSuccess && rc /= sqlRcSuccessWithInfo)
    (do
      exs <- getDiagRec rc htype handle 1
      if null exs
        then throwOdbc (OdbcException (fromIntegral rc) "01000"
          ("No error messages for return code " ++ show rc ++ " (" ++ showReturnCode rc ++ ")") [])
        else do
          let (OdbcException i st s _) = head exs
          throwOdbc (OdbcException i st s (tail exs))
    )

allocHdl :: (Storable a) => Handle -> SqlHandleType -> IO a
allocHdl h htype = do
  alloca $ \hptr -> do
    rc <- sqlAllocHandle htype h (castPtr hptr)
    checkError rc htype h
    peek hptr

allocEnv :: IO EnvHandle
allocEnv = allocHdl nullPtr sqlHTypeEnv

allocConn :: EnvHandle -> IO ConnHandle
allocConn env = do
  c <- allocHdl (castPtr env) sqlHTypeConn
  -- default char encoding is UTF8; change with setCharEncoding
  ior <- newIORef EncUTF8
  return (ConnHandle c "" ior)

setConnEncoding :: ConnHandle -> CharEncoding -> IO ()
setConnEncoding conn enc = writeIORef (connCharEnc conn) enc

setStmtEncoding :: StmtHandle -> CharEncoding -> IO ()
setStmtEncoding stmt enc = writeIORef (stmtCharEnc stmt) enc

allocStmt :: ConnHandle -> IO StmtHandle
allocStmt conn = do
  s <- allocHdl (castPtr (connHdl conn)) sqlHTypeStmt
  -- copy dbms name and char encoding from connection object
  enc <- readIORef (connCharEnc conn)
  ior <- newIORef enc
  return (StmtHandle s (connDbms conn) ior)

freeHandle :: SqlHandleType -> Handle -> IO ()
freeHandle htype h = do
  rc <- sqlFreeHandle htype h
  checkError rc htype h

freeEnv :: EnvHandle -> IO ()
freeEnv env = freeHandle sqlHTypeEnv (castPtr env)

freeConn :: ConnHandle -> IO ()
freeConn conn = freeHandle sqlHTypeConn (castPtr (connHdl conn))

freeStmt :: StmtHandle -> IO ()
freeStmt stmt = freeHandle sqlHTypeStmt (castPtr (stmtHdl stmt))

int2Ptr :: SqlInteger -> Ptr ()
int2Ptr i = plusPtr nullPtr (fromIntegral i)

setOdbcVer :: EnvHandle -> IO ()
setOdbcVer env = do
  rc <- sqlSetEnvAttr env sqlAttrOdbcVersion (int2Ptr sqlOvOdbc3) 0
  checkError rc sqlHTypeEnv (castPtr env)

connect :: ConnHandle -> String -> IO String
connect conn connstr = do
  withCStringLen connstr $ \(cstr, clen) -> do
  allocaBytes 1000 $ \outConnStr -> do
  alloca $ \ptrOutLen -> do
  rc <- sqlDriverConnect (connHdl conn) nullPtr cstr (fromIntegral clen)
    outConnStr 1000 ptrOutLen sqlDriverNoPrompt
  checkError rc sqlHTypeConn (castPtr (connHdl conn))
  outLen <- peek ptrOutLen
  peekCStringLen (outConnStr, fromIntegral outLen)

disconnect :: ConnHandle -> IO ()
disconnect conn = do
  rc <- sqlDisconnect (connHdl conn)
  checkError rc sqlHTypeConn (castPtr (connHdl conn))


prepareStmt :: StmtHandle -> String -> IO ()
prepareStmt stmt sqltext = do
  charenc <- readIORef (stmtCharEnc stmt)
  withEncStringLen charenc sqltext (\(cstr, clen) -> do
      rc <- sqlPrepare (stmtHdl stmt) cstr (fromIntegral clen)
      checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))
    )

executeStmt :: StmtHandle -> IO ()
executeStmt stmt = do
  rc <- sqlExecute (stmtHdl stmt)
  checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))

closeCursor :: StmtHandle -> IO ()
closeCursor stmt = do
  rc <- sqlCloseCursor (stmtHdl stmt)
  checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))

rowCount :: StmtHandle -> IO Int
rowCount stmt = do
  alloca $ \rcptr -> do
  rc <- sqlRowCount (stmtHdl stmt) rcptr
  checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))
  liftM fromIntegral (peek rcptr)

-- | Return 'True' if there are more rows, 'False' if end-of-data.
fetch :: StmtHandle -> IO Bool
fetch stmt = do
  rc <- sqlFetch (stmtHdl stmt)
  when (rc /= sqlRcNoData)
    (checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt)))
  return (rc /= sqlRcNoData)

-- | Return 'True' if there is another result-set to process.
-- Presumably the 'StmtHandle' is modified to reference the
-- new result-set.
moreResults :: StmtHandle -> IO Bool
moreResults stmt = do
  rc <- sqlMoreResults (stmtHdl stmt)
  when (rc /= sqlRcNoData)
    (checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt)))
  return (rc /= sqlRcNoData)


commit :: ConnHandle -> IO ()
commit conn = do
  rc <- sqlEndTran sqlHTypeConn (castPtr (connHdl conn)) sqlTransCommit
  checkError rc sqlHTypeConn (castPtr (connHdl conn))

rollback :: ConnHandle -> IO ()
rollback conn = do
  rc <- sqlEndTran sqlHTypeConn (castPtr (connHdl conn)) sqlTransRollback
  checkError rc sqlHTypeConn (castPtr (connHdl conn))

setAutoCommitOn :: ConnHandle -> IO ()
setAutoCommitOn conn = do
  rc <- sqlSetConnectAttr (connHdl conn) sqlAttrAutoCommit (int2Ptr sqlAutoCommitOn) 0
  checkError rc sqlHTypeConn (castPtr (connHdl conn))

setAutoCommitOff :: ConnHandle -> IO ()
setAutoCommitOff conn = do
  rc <- sqlSetConnectAttr (connHdl conn) sqlAttrAutoCommit (int2Ptr sqlAutoCommitOff) 0
  checkError rc sqlHTypeConn (castPtr (connHdl conn))

setTxnIsolation :: ConnHandle -> SqlInteger -> IO ()
setTxnIsolation conn level = do
  -- MS Access has transactions, but not isolation levels.
  -- Oracle & PostgreSQL don't do sqlTxnReadUncommitted or sqlTxnRepeatableRead;
  -- we get: "206 HY009 Illegal parameter value for SQL_TXN_ISOLATION"
  --     or: "  0 HYC00 [Oracle][ODBC]Optional feature not implemented."
  let doNothing = ("access" == connDbms conn)
        || (("postgresql" == connDbms conn)
          && (level == sqlTxnReadUncommitted || level == sqlTxnRepeatableRead) )
	|| (("oracle" == connDbms conn)
          && (level == sqlTxnReadUncommitted || level == sqlTxnRepeatableRead) )
  unless doNothing ( do
      rc <- sqlSetConnectAttr (connHdl conn) sqlAttrTxnIsolation (int2Ptr level) 0
      checkError rc sqlHTypeConn (castPtr (connHdl conn))
    )

getInfoString :: ConnHandle -> SqlInfoType -> IO String
getInfoString conn infotype = do
  let bufsize = 10000
  alloca $ \outsizeptr -> do
  allocaBytes bufsize $ \buffer -> do
  rc <- sqlGetInfo (connHdl conn) infotype buffer (fromIntegral bufsize) outsizeptr
  checkError rc sqlHTypeConn (castPtr (connHdl conn))
  outsize <- peek outsizeptr
  peekCStringLen (castPtr buffer, fromIntegral outsize)

getInfoDbmsName :: ConnHandle -> IO String
getInfoDbmsName conn = getInfoString conn sqlInfoDbmsName

getInfoDbmsVer :: ConnHandle -> IO String
getInfoDbmsVer conn = getInfoString conn sqlInfoDbmsVer

getInfoDatabaseName :: ConnHandle -> IO String
getInfoDatabaseName conn = getInfoString conn sqlInfoDatabaseName

getInfoDriverName :: ConnHandle -> IO String
getInfoDriverName conn = getInfoString conn sqlInfoDriverName

getInfoDriverVer :: ConnHandle -> IO String
getInfoDriverVer conn = getInfoString conn sqlInfoDriverVer

-- The ODBC spec allows you to use escape sequences
-- for product-specific functions i.e. stuff not in the SQL specs.
-- That is, it provides an abstraction over various SQL extensions.
-- getNativeSql shows you how these get translated into product-specific SQL.
-- Note that the MS Access, MS SQL Server, and PostgreSQL drivers seem to
-- return the original SQL text unchanged i.e. the ODBC escape sequences
-- are not translated, and are passed to the DBMS unchanged.
getNativeSql :: ConnHandle -> String -> IO String
getNativeSql conn sqltext = do
  let bufsize = 100000
  alloca $ \outsizeptr -> do
  allocaBytes bufsize $ \buffer -> do
  charenc <- readIORef (connCharEnc conn)
  withEncStringLen charenc sqltext $ \(cstr,clen) -> do
  rc <- sqlNativeSql (connHdl conn) cstr (fromIntegral clen) buffer (fromIntegral bufsize) outsizeptr
  checkError rc sqlHTypeConn (castPtr (connHdl conn))
  outsize <- peek outsizeptr
  peekEncStringLen charenc (castPtr buffer, fromIntegral outsize)
  


---------------------------------------------------------------------
-- Get column values with SQLGetData.
-- These functions will probably not be used much,
-- but we'll keep them in because the ODBC API has them
-- (i.e. for completeness).
-- The OdbcBindBuffer class has a getData function which uses these,
-- if you'd rather.

-- This function tests the null indicator first.
-- If the value is null, then there's no need to do the actual marshaling,
-- which is performed by the action passed in.
getMaybeFromBuffer :: Storable a => Ptr SqlLen -> Ptr a -> (Ptr a -> SqlLen -> IO b) -> IO (Maybe b)
getMaybeFromBuffer szptr bptr action = do
  len <- peek szptr
  if len < 0 then return Nothing
    else action bptr len >>= return . Just

getDataStorable :: Storable a => StmtHandle -> Int -> SqlDataType -> Int -> (a -> b) -> IO (Maybe b)
getDataStorable stmt pos sqltype buffersize convert = do
  allocaBytes buffersize $ \bptr -> do
  alloca $ \szptr -> do
  rc <- sqlGetData (stmtHdl stmt) (fromIntegral pos) sqltype (castPtr bptr) 0 szptr
  checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))
  getMaybeFromBuffer szptr bptr (\buffer len -> peek buffer >>= return . convert )

getDataUtcTime :: StmtHandle -> Int -> IO (Maybe UTCTime)
getDataUtcTime stmt pos = do
  allocaBytes #{size TIMESTAMP_STRUCT} $ \bptr -> do
  alloca $ \szptr -> do
  rc <- sqlGetData (stmtHdl stmt) (fromIntegral pos) sqlDTypeTimestamp (castPtr bptr) 50 szptr
  checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))
  getMaybeFromBuffer szptr bptr (\buffer len -> readUtcTimeFromMemory buffer >>= return )

getDataCStringLen :: StmtHandle -> Int -> IO (Maybe CStringLen)
getDataCStringLen stmt pos = do
  alloca $ \szptr -> do
  allocaBytes 16 $ \dummyptr -> do
  -- Call GetData with 0-sized buffer to get size information.
  -- We have to use sqlDTypeChar; sqlDTypeVarchar causes error "Program type out of range".
  rc <- sqlGetData (stmtHdl stmt) (fromIntegral pos) sqlDTypeChar (castPtr dummyptr) 0 szptr
  when (rc /= sqlRcSuccessWithInfo)
    (checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt)))
  bufSize <- peek szptr
  let bufSize' = 1 + if bufSize < 0 then 0 else bufSize
  -- Use size information to allocate perfectly-sized buffer.
  -- We have to use sqlDTypeChar; sqlDTypeVarchar causes error "Program type out of range".
  allocaBytes (fromIntegral bufSize') $ \bptr -> do
  rc <- sqlGetData (stmtHdl stmt) (fromIntegral pos) sqlDTypeChar (castPtr bptr) bufSize' szptr
  checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))
  len <- peek szptr
  if len < 0 then return Nothing
    else return (Just (castPtr bptr, fromIntegral len))

getDataString :: StmtHandle -> Int -> IO (Maybe String)
getDataString stmt pos = do
  mbcstrlen <- getDataCStringLen stmt pos
  case mbcstrlen of
    Nothing -> return Nothing
    Just cstrlen -> do
      charenc <- readIORef (stmtCharEnc stmt)
      peekEncStringLen charenc cstrlen >>= return . Just

{- from sqltypes.h. Struct size depends on size of SmallInt etc,
but is 16 bytes on a 32-bit platform. 32 bytes on 64-bit?
typedef struct tagTIMESTAMP_STRUCT {
    SQLSMALLINT year;
    SQLUSMALLINT month;
    SQLUSMALLINT day;
    SQLUSMALLINT hour;
    SQLUSMALLINT minute;
    SQLUSMALLINT second;
    SQLUINTEGER fraction;
} TIMESTAMP_STRUCT;
-}

peekSmallInt :: Ptr a -> Int -> IO SqlSmallInt
peekSmallInt buffer offset = peekByteOff buffer offset
peekUSmallInt :: Ptr a -> Int -> IO SqlUSmallInt
peekUSmallInt buffer offset = peekByteOff buffer offset
peekUInteger :: Ptr a -> Int -> IO SqlUInteger
peekUInteger buffer offset = peekByteOff buffer offset

-- We have to give the Ptr a concrete type, to keep the type-checker
-- happy, but it can be anything. It has to be a Storable type, though.

readUtcTimeFromMemory :: Ptr Word8 -> IO UTCTime
readUtcTimeFromMemory buffer = do
  year <- peekSmallInt buffer #{offset TIMESTAMP_STRUCT, year}
  month <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, month}
  day <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, day}
  hour <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, hour}
  minute <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, minute}
  second <- peekUSmallInt buffer #{offset TIMESTAMP_STRUCT, second}
  -- Fraction ranges from 0 - 999,999,999,
  frac <- peekUInteger buffer #{offset TIMESTAMP_STRUCT, fraction}
  let secs :: Double; secs = fromIntegral second + (fromIntegral frac / 1000000000.0)
  return (mkUTCTime (fromIntegral year) month day hour minute second)

---------------------------------------------------------------------
-- Return-set column binding.
-- Apparently this is faster than SQLGetData for larger result-sets,
-- because the output buffers do not need to be rebound with every call.
-- Dunno how much difference this'll make in practice. Suck 'n' see.

bindColumnBuffer :: StmtHandle -> Int -> SqlDataType -> SqlLen -> IO BindBuffer
bindColumnBuffer stmt pos dtype size = do
  alloca $ \colPtr -> do
  charenc <- readIORef (stmtCharEnc stmt)
  rc <- sqlNumResultCols (stmtHdl stmt) colPtr
  checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))
  nc <- peek colPtr
  if (fromIntegral nc) < pos || pos < 1
    then throwOdbc (OdbcException (-1) "01000"
      ("Attempted fetch from invalid column number " ++ show pos) [])
    else do
      buffer <- createEmptyBuffer (fromIntegral size) charenc
      withForeignPtr (bindBufPtr buffer) $ \bptr -> do
      withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
      rc <- sqlBindCol (stmtHdl stmt) (fromIntegral pos) dtype bptr size szptr
      checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))
      return buffer

createEmptyBuffer :: SqlLen -> CharEncoding -> IO BindBuffer
createEmptyBuffer size charenc = do
  szfptr <- mallocForeignPtr
  bfptr <- mallocForeignPtrBytes (fromIntegral size)
  return (BindBuffer bfptr szfptr size charenc)


testForNull :: BindBuffer -> (Ptr Buffer -> SqlLen -> IO a) -> IO (Maybe a)
testForNull buffer action = do
  withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
  len <- peek szptr
  -- sqlNullData = -1, so anthing less than zero could
  -- be treated as null.
  if len < 0 then return Nothing
    else withForeignPtr (bindBufPtr buffer) $ \bptr ->
      action bptr len >>= return . Just

getStorableFromBuffer :: Storable a => BindBuffer -> IO (Maybe a)
getStorableFromBuffer buffer =
  testForNull buffer (\bptr _ -> peek (castPtr bptr))

getStringFromBuffer :: BindBuffer -> IO (Maybe String)
getStringFromBuffer buffer =
  testForNull buffer (\ptr len -> peekEncStringLen (bindBufCharEnc buffer) (castPtr ptr, fromIntegral len))

getUtcTimeFromBuffer :: BindBuffer -> IO (Maybe UTCTime)
getUtcTimeFromBuffer bindbuffer =
  testForNull bindbuffer (\b _ -> readUtcTimeFromMemory (castPtr b))


---------------------------------------------------------------------
-- Parameter binding

-----------------------------------
-- First some helper functions...
-----------------------------------

-- Create a Bind Buffer for a value in the Storable class
-- e.g. Int, Double, Char
mkBindBufferForStorable :: Storable a => Maybe a -> IO BindBuffer
mkBindBufferForStorable mbv =
  case mbv of
    Nothing -> let zero :: Int; zero = 0; in mkBindBufferHelper zero sqlNullData (sizeOf zero)
    Just val -> mkBindBufferHelper val (fromIntegral (sizeOf val)) (sizeOf val)
  where 
    mkBindBufferHelper :: Storable a => a -> SqlLen -> Int -> IO BindBuffer
    mkBindBufferHelper val valsize bufsize = do
      bptr <- malloc
      poke bptr val
      mkBindBuffer bptr valsize bufsize EncUTF8

-- Create a Bind Buffer for a chunk of memory (so, anything at all).
-- Given:
--   * a Ptr to a buffer (already allocated and filled)
--   * the size (SqlLen) of the data in the buffer (not necessarily the same as the buffer size)
--   * the size of the buffer
--   * a character encoding for Strings
-- create a BindBuffer object.
-- The buffer is wrapped with a ForeignPtr so it will be freed when the buffer
-- is no longer required.
mkBindBuffer :: Ptr a -> SqlLen -> Int -> CharEncoding -> IO BindBuffer
mkBindBuffer valptr valsize bufsize charenc = do
  szfptr <- mallocForeignPtr
  withForeignPtr szfptr (\szptr -> poke szptr valsize)
  bfptr <- newForeignPtr finalizerFree valptr
  let size2 = if valsize > (fromIntegral bufsize) then valsize else (fromIntegral bufsize)
  return (BindBuffer (castForeignPtr bfptr) szfptr size2 charenc)

bindParam ::
  StmtHandle
  -> Int
  -> SqlParamDirection
  -> SqlCDataType
  -> SqlDataType
  -> SqlULen
  -> SqlSmallInt
  -> BindBuffer
  -> IO ()
bindParam stmt pos direction ctype sqltype precision scale buffer =
  withForeignPtr (bindBufPtr buffer) $ \bptr -> do
  withForeignPtr (bindBufSzPtr buffer) $ \valszptr -> do
  let bufsize = bindBufSize buffer
  rc <- sqlBindParameter (stmtHdl stmt) (fromIntegral pos) direction ctype sqltype precision scale bptr bufsize valszptr
  checkError rc sqlHTypeStmt (castPtr (stmtHdl stmt))


------------------------------------------------------
-- Now type-specific functions to bind values.
-- Each of these creates and returns a BindBuffer.
------------------------------------------------------

bindNull :: StmtHandle -> Int -> SqlParamDirection -> SqlCDataType -> SqlDataType -> IO BindBuffer
bindNull stmt pos direction ctype dtype = do
  let val :: Maybe Int; val = Nothing
  buffer <- mkBindBufferForStorable val
  bindParam stmt pos direction ctype dtype 0 0 buffer
  return buffer

bindParamString :: StmtHandle -> Int -> SqlParamDirection -> Maybe String -> Int -> IO BindBuffer
bindParamString stmt pos direction Nothing bufsize = do
  charenc <- readIORef (stmtCharEnc stmt)
  let bufsz = if bufsize < 8 then 8 else (fromIntegral bufsize)
  ptr <- mallocBytes bufsz
  buffer <- mkBindBuffer ptr sqlNullData bufsz charenc
  -- We have to use sqlDTypeVarchar; sqlDTypeChar doesn't marshal output values (fixed size).
  -- MS Access and SQL Server require buffer size in precision.
  bindParam stmt pos direction sqlCTypeString sqlDTypeVarchar (fromIntegral bufsz) 0 buffer
  return buffer
bindParamString stmt pos direction (Just s) bufsize = do
  charenc <- readIORef (stmtCharEnc stmt)
  (mem, clen, bufsz) <- withEncStringLen charenc s makeStringBuffer
  -- Now wrap the malloc'd area wth a ForeignPtr, which should ensure
  -- it is GC'd when no longer used.
  buffer <- mkBindBuffer mem (fromIntegral clen) bufsz charenc
  -- We have to use sqlDTypeVarchar; sqlDTypeChar doesn't marshal output values (fixed size).
  -- MS Access and SQL Server require buffer size in precision.
  bindParam stmt pos direction sqlCTypeString sqlDTypeVarchar (fromIntegral bufsz) 0 buffer
  return buffer
  where
    makeStringBuffer :: CStringLen -> IO (CString, Int, Int)
    makeStringBuffer (cstr, clen) = do
      -- The size we pass to bindParam is the max of string-length and buffer-size.
      -- This lets us be lazy and pass 0 as the buffer size in the input-only case.
      let bufsz = 1 + maximum [clen, bufsize]
      mem <- mallocBytes bufsz
      copyBytes mem cstr clen
      pokeByteOff mem clen (0 :: CChar)
      return (mem, clen, bufsz)


-- Fun with dates+times...

pokeSmallInt :: Ptr a -> Int -> SqlSmallInt -> IO ()
pokeSmallInt buffer offset val = pokeByteOff buffer offset val
pokeUSmallInt :: Ptr a -> Int -> SqlUSmallInt -> IO ()
pokeUSmallInt buffer offset val = pokeByteOff buffer offset val
pokeUInteger :: Ptr a -> Int -> SqlUInteger -> IO ()
pokeUInteger buffer offset val = pokeByteOff buffer offset val

writeUTCTimeToMemory :: Ptr Word8 -> UTCTime -> IO ()
writeUTCTimeToMemory buffer utc = do
  let
    (LocalTime ltday time) = utcToLocalTime (hoursToTimeZone 0) utc
    (TimeOfDay hour minute second) = time
    (year, month, day) = toGregorian ltday
  pokeSmallInt buffer #{offset TIMESTAMP_STRUCT, year} (fromIntegral year)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, month} (fromIntegral month)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, day} (fromIntegral day)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, hour} (fromIntegral hour)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, minute} (fromIntegral minute)
  -- what do we do with fraction? What sort of fraction is it?
  -- apparently can range from 0 - 999,999,999,
  -- but MS SQL Server only handles milliseconds (0 - 999) i.e. precision 3
  let (secs, frac) = properFraction second
  let fraction :: SqlUInteger; fraction = round (frac * 1000000000.0)
  pokeUSmallInt buffer #{offset TIMESTAMP_STRUCT, second} secs
  pokeUInteger buffer #{offset TIMESTAMP_STRUCT, fraction} fraction

-- writeUTCTimeToMemory and makeUtcTimeBuffer don't work with MS SQL Server;
-- it always returns 22008 "Datetime field overflow".
-- They do work with PostgreSQL and Oracle ODBC drivers.

makeUtcTimeBuffer :: UTCTime -> IO BindBuffer
makeUtcTimeBuffer utc = do
  mem <- mallocBytes #{size TIMESTAMP_STRUCT}
  writeUTCTimeToMemory (castPtr mem) utc
  mkBindBuffer mem #{size TIMESTAMP_STRUCT} #{size TIMESTAMP_STRUCT} EncUTF8

-- Marshal via String, for MS SQL Server.
-- We have to drop the last 6 chars (nnn+nn) from the ISO datetime,
-- because MS SQL Server can only cope with "yyyy-mm-dd hh:mi:ss.fff".

makeUtcTimeStringBuffer :: UTCTime -> IO BindBuffer
makeUtcTimeStringBuffer utc = do
  let buffersize = 32  -- Allow extra space for output values
  mem <- mallocBytes buffersize
  let s = take 23 (utcTimeToOdbcDatetime utc)
  withCStringLen s $ \(cstr, clen) -> do
    copyBytes mem cstr (fromIntegral clen)
    pokeByteOff mem (fromIntegral clen) (0 :: Word8)
    mkBindBuffer mem (fromIntegral clen) buffersize EncUTF8

bindParamUtcTime :: StmtHandle -> Int -> SqlParamDirection -> Maybe UTCTime -> IO BindBuffer
bindParamUtcTime stmt pos direction Nothing = do
  bindNull stmt pos direction sqlCTypeTimestamp sqlDTypeTimestamp
bindParamUtcTime stmt pos direction (Just utc) = do
  case (stmtDbms stmt) of
    "microsoft sql server" -> do
      -- I cannot get TIMESTAMP_STRUCT to work with MS SQL Server;
      -- it always returns 22008 "Datetime field overflow".
      -- We have to pass in a String, rather than a TIMESTAMP_STRUCT,
      -- and let the ODBC driver do the conversion.
      buffer <- makeUtcTimeStringBuffer utc
      withForeignPtr (bindBufSzPtr buffer) $ \szptr -> do
        -- 23 is the largest precision value allowed by MS SQL Server.
        -- That gives us "yyyy-mm-dd hh:mm:ss.fff"
        bindParam stmt pos direction sqlCTypeString sqlDTypeTimestamp 23 0 buffer
        return buffer
    otherwise -> do
      -- For TimeStamp:
      --   Size/Length should be 16 bytes.
      --   Precision should be 8 (or 16?).
      --   Scale is the number of digits in the fraction component
      --     (normally 9; SQL Server only allows 0-3).
      buffer <- makeUtcTimeBuffer utc
      bindParam stmt pos direction sqlCTypeTimestamp sqlDTypeTimestamp #{size TIMESTAMP_STRUCT} 9 buffer
      return buffer


---------------------------------------------------------------------
-- Binding with class...

sizeOfMaybe :: forall a. Storable a => Maybe a -> Int
--sizeOfMaybe :: Storable a => Maybe a -> Int
sizeOfMaybe _ = sizeOf ( undefined :: a )
-- H98 stylee...
--sizeOfMaybe v@Nothing = sizeOfMaybe (asTypeOf (Just undefined) v)
--sizeOfMaybe (Just v) = sizeOf v


newtype OutParam a = OutParam a
newtype InOutParam a = InOutParam a

{-
Separate out the two different types of binding: parameter and column.

Both types use the same BindBuffer object, but bind parameters
can send and receive values from the buffer, whereas
column binds only receive values.

So we have class OdbcBindBuffer for result set column binds
(data coming out only), and class OdbcBindParam for bind
parameters that can be both input and output.

We'll have instances for OdbcBindBuffer that are of the form (Maybe a),
where a is one of the normal database types e.g. Int, Double,
String, UTCTime.

The instances for OdbcBindParam will include the (Maybe a) set,
and also (OutParam (Maybe a)), and (InOutParam (Maybe a)),
to indicate Out and In/Out paramaters.

When we do a column buffer bind, we require a dummy value
of the column data type, so that we know which instance to use.
-}

class OdbcBindBuffer a where
  bindColBuffer
    :: StmtHandle  -- ^ stmt handle
    -> Int  -- ^ column position (1-indexed)
    -> Int  -- ^ size of result buffer (ignored when it can be inferred from type of a)
    -> a  -- ^ dummy value of the appropriate type (just to ensure we get the right class instance)
    -> IO BindBuffer  -- ^ returns a 'BindBuffer' object
  getFromBuffer :: BindBuffer -> IO a
  getData :: StmtHandle -> Int -> IO a

instance OdbcBindBuffer (Maybe Int) where
  bindColBuffer stmt pos size val = bindColumnBuffer stmt pos sqlDTypeInt (fromIntegral (sizeOfMaybe val))
  getFromBuffer buffer = getStorableFromBuffer buffer
  getData stmt pos = getDataStorable stmt pos sqlDTypeInt (sizeOf cint) convert
    where convert :: CInt -> Int; convert = fromIntegral
          cint :: CInt; cint = 0

instance OdbcBindBuffer (Maybe Double) where
  bindColBuffer stmt pos size val = bindColumnBuffer stmt pos sqlDTypeDouble (fromIntegral (sizeOfMaybe val))
  getFromBuffer buffer = getStorableFromBuffer buffer
  getData stmt pos = getDataStorable stmt pos sqlDTypeDouble (sizeOf cdbl) convert
    where convert :: CDouble -> Double; convert = realToFrac
          cdbl :: CDouble; cdbl = 0

instance OdbcBindBuffer (Maybe String) where
  -- We have to use sqlDTypeChar; sqlDTypeVarchar causes error "Program type out of range".
  bindColBuffer stmt pos size val = bindColumnBuffer stmt pos sqlDTypeChar (fromIntegral size)
  getFromBuffer buffer = getStringFromBuffer buffer
  getData stmt pos = getDataString stmt pos

instance OdbcBindBuffer (Maybe UTCTime) where
  bindColBuffer stmt pos size val = bindColumnBuffer stmt pos sqlDTypeTimestamp #{size TIMESTAMP_STRUCT}
  getFromBuffer buffer = getUtcTimeFromBuffer buffer
  getData stmt pos = getDataUtcTime stmt pos


class OdbcBindParam a where
  bindParamBuffer
    :: StmtHandle  -- ^ stmt handle
    -> Int  -- ^ parameter position (1-indexed)
    -> a  -- ^ value to write to buffer
    -> Int -- ^ size of buffer, for output.
           -- Value is ignored if input only (buffer will be sized to exactly hold input only)
           -- or size is fixed by type (e.g. Int, Double)
    -> IO BindBuffer  -- ^ returns a 'BindBuffer' object

bindParamStorable stmt pos val dir ctype dtype precn scale = do
  buffer <- mkBindBufferForStorable val
  bindParam stmt pos dir ctype dtype precn scale buffer
  return buffer

instance OdbcBindParam (Maybe Int) where
  bindParamBuffer stmt pos val _ =
    bindParamStorable stmt pos val sqlParamInput sqlCTypeInt sqlDTypeInt 30 0

instance OdbcBindParam (OutParam (Maybe Int)) where
  bindParamBuffer stmt pos (OutParam val) _ =
    bindParamStorable stmt pos val sqlParamOutput sqlCTypeInt sqlDTypeInt 30 0

instance OdbcBindParam (InOutParam (Maybe Int)) where
  bindParamBuffer stmt pos (InOutParam val) _ =
    bindParamStorable stmt pos val sqlParamInputOutput sqlCTypeInt sqlDTypeInt 30 0

instance OdbcBindParam (Maybe Double) where
  bindParamBuffer stmt pos val _ =
    bindParamStorable stmt pos val sqlParamInput sqlCTypeDouble sqlDTypeDouble 50 50

instance OdbcBindParam (OutParam (Maybe Double)) where
  bindParamBuffer stmt pos (OutParam val) _ =
    bindParamStorable stmt pos val sqlParamOutput sqlCTypeDouble sqlDTypeDouble 50 50

instance OdbcBindParam (InOutParam (Maybe Double)) where
  bindParamBuffer stmt pos (InOutParam val) _ =
    bindParamStorable stmt pos val sqlParamInputOutput sqlCTypeDouble sqlDTypeDouble 50 50

instance OdbcBindParam (Maybe String) where
  bindParamBuffer stmt pos val sz =
    bindParamString stmt pos sqlParamInput val 0

instance OdbcBindParam (OutParam (Maybe String)) where
  bindParamBuffer stmt pos (OutParam val) sz =
    bindParamString stmt pos sqlParamOutput val sz

instance OdbcBindParam (InOutParam (Maybe String)) where
  bindParamBuffer stmt pos (InOutParam val) sz =
    bindParamString stmt pos sqlParamInputOutput val sz

instance OdbcBindParam (Maybe UTCTime) where
  bindParamBuffer stmt pos val _ =
    bindParamUtcTime stmt pos sqlParamInput val

instance OdbcBindParam (OutParam (Maybe UTCTime)) where
  bindParamBuffer stmt pos (OutParam val) _ =
    bindParamUtcTime stmt pos sqlParamOutput val

instance OdbcBindParam (InOutParam (Maybe UTCTime)) where
  bindParamBuffer stmt pos (InOutParam val) _ =
    bindParamUtcTime stmt pos sqlParamInputOutput val


---------------------------------------------------------------------
-- FFI

-- From sql.h:
-- SQLRETURN SQL_API SQLAllocHandle(SQLSMALLINT,SQLHANDLE,SQLHANDLE*);
foreign import #{CALLCONV} unsafe "sql.h SQLAllocHandle" sqlAllocHandle ::
  SqlHandleType -> Handle -> Ptr Handle -> IO SqlReturn

-- SQLRETURN SQL_API SQLFreeHandle(SQLSMALLINT,SQLHANDLE);
foreign import #{CALLCONV} unsafe "sql.h SQLFreeHandle" sqlFreeHandle ::
  SqlSmallInt -> Handle -> IO SqlReturn

-- SQLRETURN SQL_API SQLGetDiagRec(SQLSMALLINT,SQLHANDLE,SQLSMALLINT,SQLCHAR*,SQLINTEGER*,SQLCHAR*,SQLSMALLINT,SQLSMALLINT*); 
foreign import #{CALLCONV} unsafe "sql.h SQLGetDiagRec" sqlGetDiagRec ::
  SqlHandleType  -- ^ enum: which handle type is the next parameter?
  -> Handle  -- ^ generic handle ptr
  -> SqlSmallInt  -- ^ row (or message) number
  -> CString  -- ^ OUT: state
  -> Ptr SqlInteger  -- ^ OUT: error number
  -> CString  -- ^ OUT: error message
  -> SqlSmallInt  -- ^ IN: message buffer size
  -> Ptr SqlSmallInt  -- ^ OUT: message length
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLDriverConnect(SQLHDBC,SQLHWND,SQLCHAR*,SQLSMALLINT,SQLCHAR*,SQLSMALLINT,SQLSMALLINT*,SQLUSMALLINT);
foreign import #{CALLCONV} unsafe "sql.h SQLDriverConnect" sqlDriverConnect ::
  ConnHdl
  -> WindowHandle  -- ^ just pass nullPtr
  -> CString  -- ^ connection string
  -> SqlSmallInt  -- ^ connection string size
  -> CString  -- ^ OUT: buffer for normalised connection string
  -> SqlSmallInt  -- ^ buffer size
  -> Ptr SqlSmallInt  -- ^ OUT: length of returned string
  -> SqlUSmallInt  -- ^ enum: should driver prompt user for missing info?
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLDisconnect(SQLHDBC);
foreign import #{CALLCONV} unsafe "sql.h SQLDisconnect" sqlDisconnect ::
  ConnHdl -> IO SqlReturn

-- SQLRETURN SQL_API SQLSetEnvAttr(SQLHENV,SQLINTEGER,SQLPOINTER,SQLINTEGER);
foreign import #{CALLCONV} unsafe "sql.h SQLSetEnvAttr" sqlSetEnvAttr ::
  EnvHandle  -- ^ Env Handle
  -> SqlInteger  -- ^ Attribute (enumeration)
  -> Ptr ()  -- ^ value (cast to void*)
  -> SqlInteger  -- ^ ? - set to 0
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLSetConnectAttr(SQLHDBC,SQLINTEGER,SQLPOINTER,SQLINTEGER);
foreign import #{CALLCONV} unsafe "sql.h SQLSetConnectAttr" sqlSetConnectAttr ::
  ConnHdl  -- ^ Connection Handle
  -> SqlInteger  -- ^ Attribute (enumeration)
  -> Ptr ()  -- ^ value (cast to void*)
  -> SqlInteger  -- ^ ? - set to 0
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLPrepare(SQLHSTMT,SQLCHAR*,SQLINTEGER);
foreign import #{CALLCONV} unsafe "sql.h SQLPrepare" sqlPrepare ::
  StmtHdl -> CString -> SqlInteger -> IO SqlReturn

-- SQLRETURN SQL_API SQLBindParameter(SQLHSTMT,SQLUSMALLINT,SQLSMALLINT,SQLSMALLINT,SQLSMALLINT,SQLULEN,SQLSMALLINT,SQLPOINTER,SQLLEN,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLBindParameter" sqlBindParameter ::
  StmtHdl
  -> SqlUSmallInt  -- ^ position, 1-indexed
  -> SqlParamDirection  -- ^ direction: IN, OUT
  -> SqlCDataType  -- ^ C data type: char, int, long, float, etc
  -> SqlDataType  -- ^ SQL data type: string, int, long, date, etc
  -> SqlULen  -- ^ col size (precision)
  -> SqlSmallInt  -- ^ decimal digits (scale)
  -> Ptr Buffer  -- ^ input+output buffer
  -> SqlLen  -- ^ buffer size
  -> Ptr SqlLen -- ^ input+output data size, SQL_NTS (-3), SQL_NULL_DATA (-1), or SQL_NO_TOTAL (-4)
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLExecute(SQLHSTMT);
foreign import #{CALLCONV} unsafe "sql.h SQLExecute" sqlExecute ::
  StmtHdl -> IO SqlReturn

-- SQLRETURN SQL_API SQLNumResultCols(SQLHSTMT,SQLSMALLINT*);
foreign import #{CALLCONV} unsafe "sql.h SQLNumResultCols" sqlNumResultCols ::
  StmtHdl -> Ptr SqlSmallInt -> IO SqlReturn

-- SQLRETURN SQL_API SQLRowCount(SQLHSTMT,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLRowCount" sqlRowCount ::
  StmtHdl -> Ptr SqlLen -> IO SqlReturn

-- SQLRETURN SQL_API SQLDescribeCol(SQLHSTMT,SQLUSMALLINT,SQLCHAR*,SQLSMALLINT,SQLSMALLINT*,SQLSMALLINT*,SQLULEN*,SQLSMALLINT*,SQLSMALLINT*);
foreign import #{CALLCONV} unsafe "sql.h SQLDescribeCol" sqlDescribeCol ::
  StmtHdl
  -> SqlUSmallInt  -- ^ position, 1-indexed
  -> CString  -- ^ buffer for column name
  -> SqlSmallInt  -- ^ size of column name buffer
  -> Ptr SqlSmallInt  -- ^ size of column name output string
  -> Ptr SqlDataType  -- ^ SQL data type: string, int, long, date, etc
  -> Ptr SqlULen  -- ^ col size (precision)
  -> Ptr SqlSmallInt  -- ^ decimal digits (scale)
  -> Ptr SqlSmallInt  -- ^ nullable: SQL_NO_NULLS, SQL_NULLABLE, or SQL_NULLABLE_UNKNOWN
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLBindCol(SQLHSTMT,SQLUSMALLINT,SQLSMALLINT,SQLPOINTER,SQLLEN,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLBindCol" sqlBindCol ::
  StmtHdl
  -> SqlUSmallInt  -- ^ column position, 1-indexed
  -> SqlDataType  -- ^ SQL data type: string, int, long, date, etc
  -> Ptr Buffer  -- ^ output buffer
  -> SqlLen  -- ^ output buffer size
  -> Ptr SqlLen -- ^ output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLFetch(SQLHSTMT);
foreign import #{CALLCONV} unsafe "sql.h SQLFetch" sqlFetch ::
  StmtHdl -> IO SqlReturn

-- SQLRETURN SQL_API SQLGetData(SQLHSTMT,SQLUSMALLINT,SQLSMALLINT,SQLPOINTER,SQLLEN,SQLLEN*);
foreign import #{CALLCONV} unsafe "sql.h SQLGetData" sqlGetData ::
  StmtHdl
  -> SqlUSmallInt  -- ^ column position, 1-indexed
  -> SqlDataType  -- ^ SQL data type: string, int, long, date, etc
  -> Ptr Buffer  -- ^ output buffer
  -> SqlLen  -- ^ output buffer size
  -> Ptr SqlLen -- ^ output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLCloseCursor(SQLHSTMT); 
foreign import #{CALLCONV} unsafe "sql.h SQLCloseCursor" sqlCloseCursor ::
  StmtHdl -> IO SqlReturn

-- SQLRETURN SQL_API SQLMoreResults(SQLHSTMT);
foreign import #{CALLCONV} unsafe "sql.h SQLMoreResults" sqlMoreResults ::
  StmtHdl -> IO SqlReturn

-- There's no beginTrans - transactions are started implicitly.
-- SQLRETURN SQL_API SQLEndTran(SQLSMALLINT,SQLHANDLE,SQLSMALLINT);
foreign import #{CALLCONV} unsafe "sql.h SQLEndTran" sqlEndTran ::
  SqlSmallInt -> Handle -> SqlSmallInt -> IO SqlReturn

-- SQLRETURN SQL_API SQLGetInfo(SQLHDBC,SQLUSMALLINT,SQLPOINTER,SQLSMALLINT,SQLSMALLINT*);
foreign import #{CALLCONV} unsafe "sql.h SQLGetInfo" sqlGetInfo ::
  ConnHdl
  -> SqlInfoType  -- ^ information type
  -> Ptr Buffer  -- ^ output buffer
  -> SqlSmallInt  -- ^ output buffer size
  -> Ptr SqlSmallInt -- ^ output data size, or -1 (SQL_NULL_DATA) for null
  -> IO SqlReturn

-- SQLRETURN SQL_API SQLNativeSql(SQLHDBC,SQLCHAR*,SQLINTEGER,SQLCHAR*,SQLINTEGER,SQLINTEGER*);
foreign import #{CALLCONV} unsafe "sql.h SQLNativeSql" sqlNativeSql ::
  ConnHdl
  -> CString  -- ^ sql text in
  -> SqlInteger  -- ^ size of sql text
  -> CString  -- ^ buffer for output text
  -> SqlInteger  -- ^ size of output buffer
  -> Ptr SqlInteger  -- ^ size of text in output buffer
  -> IO SqlReturn

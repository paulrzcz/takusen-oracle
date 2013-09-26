
|
Module      :  Database.Sqlite.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Sqlite implementation of Database.Enumerator.


> {-# OPTIONS -fglasgow-exts #-}

> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE IncoherentInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeSynonymInstances #-}


> module Database.Sqlite.Enumerator
>   ( Session, connect
>   , prepareStmt, preparePrefetch
>   , prepareQuery, prepareLargeQuery, prepareCommand
>   , sql, sqlbind, prefetch, cmdbind
>   , LastInsertRowid(..)
>   , module Database.Enumerator
>   )
> where


> import Data.Int ( Int64 )
> import Database.Enumerator
> import Database.InternalEnumerator
> import Database.Util
> import Foreign.C
> import Control.Monad
> import Control.Exception.Extensible
> import Database.Sqlite.SqliteFunctions
>   (DBHandle, StmtHandle, SqliteException(..), catchSqlite, throwSqlite)
> import qualified Database.Sqlite.SqliteFunctions as DBAPI
> import Control.Monad.Trans
> import Control.Monad.Reader
> import Data.Dynamic
> import Data.IORef
> import Data.Int
> import System.Time
> import Data.Time


> {-# DEPRECATED prepareStmt "Use prepareQuery or prepareCommand instead" #-}
> {-# DEPRECATED preparePrefetch "Use prepareLargeQuery instead" #-}

--------------------------------------------------------------------
-- ** API Wrappers
--------------------------------------------------------------------

|These wrappers ensure that only DBExceptions are thrown,
and never SqliteExceptions.
We don't need wrappers for the colValXxx functions
because they never throw exceptions.


> convertAndRethrow :: SqliteException -> IO a
> convertAndRethrow (SqliteException e m) = do
>   let
>     s@(ssc,sssc) = errorSqlState e
>     ec = case ssc of
>       "XX" -> DBFatal
>       "58" -> DBFatal
>       "57" -> DBFatal
>       "54" -> DBFatal
>       "53" -> DBFatal
>       "08" -> DBFatal
>       _ -> DBError
>   throwDB (ec s e m)

Below are pretty much all of the errors that Sqlite can throw.

> errorSqlState :: Int -> (String, String)
> errorSqlState 0  = ("00", "000")
> errorSqlState 1  = ("42", "000") -- sql error or missing database
> errorSqlState 2  = ("XX", "000") -- internal error
> errorSqlState 3  = ("42", "501") -- insufficient privileges/permission denied
> errorSqlState 4  = ("38", "000") -- callback requested abort
> errorSqlState 5  = ("58", "030") -- database file locked
> errorSqlState 6  = ("55", "006") -- table locked
> errorSqlState 7  = ("53", "200") -- malloc failed
> errorSqlState 8  = ("25", "006") -- can't write readonly database
> errorSqlState 9  = ("57", "014") -- query cancelled (interrupt)
> errorSqlState 10 = ("58", "030") -- io error
> errorSqlState 11 = ("58", "030") -- corrupt file
> errorSqlState 12 = ("42", "704") -- internal: object not found
> errorSqlState 13 = ("53", "100") -- database full
> errorSqlState 14 = ("58", "030") -- can't open database file
> errorSqlState 15 = ("55", "000") -- lock protocol error
> errorSqlState 16 = ("22", "000") -- internal: empty table
> errorSqlState 17 = ("42", "000") -- schema changed
> errorSqlState 18 = ("54", "000") -- row too big
> errorSqlState 19 = ("23", "000") -- constraint violation
> errorSqlState 20 = ("42", "804") -- data type mismatch
> errorSqlState 21 = ("39", "000") -- library used incorrectly
> errorSqlState 22 = ("58", "030") -- unsupported OS feature on host
> errorSqlState 23 = ("42", "501") -- authorisation denied
> errorSqlState 100 = ("00", "000") -- next row ready
> errorSqlState 101 = ("00", "000") -- end of fetch
> errorSqlState _  = ("01", "000") -- unspecified error


|Common case: wrap an action with a convertAndRethrow.

> convertEx :: IO a -> IO a
> convertEx action = catchSqlite action convertAndRethrow

> stmtPrepare :: DBHandle -> String -> IO StmtHandle
> stmtPrepare db sqltext = convertEx $ DBAPI.stmtPrepare db sqltext

> fetchRow :: DBHandle -> StmtHandle -> IO CInt
> fetchRow db stmt = convertEx $ DBAPI.stmtFetch db stmt

> resetStmt :: DBHandle -> StmtHandle -> IO ()
> resetStmt db stmt = convertEx $ DBAPI.stmtReset db stmt

> finaliseStmt :: DBHandle -> StmtHandle -> IO ()
> finaliseStmt db stmt = convertEx $ DBAPI.stmtFinalise db stmt

> openDb dbname = convertEx $ DBAPI.openDb dbname
> closeDb handle = convertEx $ DBAPI.closeDb handle

--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------

We don't need much in an Sqlite Session record.
Session objects are created by 'connect'.

> newtype Session = Session { dbHandle :: DBHandle } deriving Typeable

> connect :: String -> ConnectA Session
> connect dbname = ConnectA $ do
>   db <- openDb dbname
>   return (Session db)

> lastInsertRowid :: Session -> IO Int64
> lastInsertRowid sess =
>   liftM fromIntegral $! DBAPI.sqliteLastInsertRowid (dbHandle sess)

--------------------------------------------------------------------
-- Statements and Commands
--------------------------------------------------------------------

> newtype QueryString = QueryString String

> sql :: String -> QueryString
> sql str = QueryString str

> instance Command QueryString Session where
>   executeCommand sess (QueryString str) = executeCommand sess str

> instance Command String Session where
>   executeCommand sess str = do
>     bracket 
>       (stmtPrepare (dbHandle sess) str)
>       (finaliseStmt (dbHandle sess))
>       (\stmt -> do
>         fetchRow (dbHandle sess) stmt
>         liftM fromIntegral (DBAPI.stmtChanges (dbHandle sess))
>       )

> instance Command BoundStmt Session where
>   executeCommand sess (BoundStmt pstmt) = do
>     fetchRow (dbHandle sess) (stmtHandle pstmt)
>     n <- DBAPI.stmtChanges (dbHandle sess)
>     -- reset, so prepared stmt can be reused
>     resetStmt (dbHandle sess) (stmtHandle pstmt)
>     return (fromIntegral n)

> instance Command StmtBind Session where
>   executeCommand sess (StmtBind sqltext bas) = do
>     bracket
>       (let (PreparationA action) = prepareStmt' sqltext False in action sess)
>       (finaliseStmt (dbHandle sess) . stmtHandle)
>       (\pstmt -> do
>         sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>         fetchRow (dbHandle sess) (stmtHandle pstmt)
>         liftM fromIntegral (DBAPI.stmtChanges (dbHandle sess))
>       )


> data LastInsertRowid = LastInsertRowid

> instance EnvInquiry LastInsertRowid Session Int64 where
>   inquire LastInsertRowid sess =
>     liftM fromIntegral (DBAPI.sqliteLastInsertRowid (dbHandle sess))


> instance ISession Session where
>   disconnect sess = closeDb (dbHandle sess)
>   beginTransaction sess isolation =
>     executeCommand sess "begin;" >> return ()
>   commit sess = executeCommand sess "commit;" >> return ()
>   rollback sess = executeCommand sess "rollback;" >> return ()

About stmtFreeWithQuery:

We need to keep track of the scope of the PreparedStmtObj
i.e. should it be freed when the Query (result-set) is freed,
or does it have a longer lifetime?
PreparedStmts created by prepareStmt have a lifetime possibly
longer than the result-set; users should use withPreparedStatement
to manage these.

PreparedStmts can also be created internally by various instances
of makeQuery (in class Statement), and these will usually have the
same lifetime/scope as that of the Query (result-set).

This lifetime distinction should probably be handled by having
separate types for the two types of prepared statement...

> data PreparedStmtObj = PreparedStmtObj
>   { stmtHandle :: StmtHandle
>   , stmtFreeWithQuery :: Bool
>   }

> prepareStmt :: QueryString -> PreparationA Session PreparedStmtObj
> prepareStmt (QueryString sqltext) = prepareStmt' sqltext False

> prepareQuery :: QueryString -> PreparationA Session PreparedStmtObj
> prepareQuery (QueryString sqltext) = prepareStmt' sqltext False

> prepareLargeQuery :: Int -> QueryString -> PreparationA Session PreparedStmtObj
> prepareLargeQuery _ (QueryString sqltext) = prepareStmt' sqltext False

> prepareCommand :: QueryString -> PreparationA Session PreparedStmtObj
> prepareCommand (QueryString sqltext) = prepareStmt' sqltext False


preparePrefetch is just here for interface consistency
with Oracle and PostgreSQL.

> preparePrefetch :: Int -> QueryString -> PreparationA Session PreparedStmtObj
> preparePrefetch count (QueryString sqltext) =
>   prepareStmt' sqltext False

> prepareStmt' sqltext free =
>   PreparationA (\sess -> do
>     stmt <- stmtPrepare (dbHandle sess) sqltext
>     return (PreparedStmtObj stmt free))

--------------------------------------------------------------------
-- ** Binding
--------------------------------------------------------------------

> newtype BoundStmt = BoundStmt { boundStmt :: PreparedStmtObj }
> type BindObj = Int -> IO ()

> instance IPrepared PreparedStmtObj Session BoundStmt BindObj where
>   bindRun sess stmt bas action = do
>     sequence_ (zipWith (\i (BindA ba) -> ba sess stmt i) [1..] bas)
>     action (BoundStmt stmt)
>   destroyStmt sess stmt = finaliseStmt (dbHandle sess) (stmtHandle stmt)

> instance DBBind (Maybe String) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Int) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Int64) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Double) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe CalendarTime) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe UTCTime) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe a) Session PreparedStmtObj BindObj
>     => DBBind a Session PreparedStmtObj BindObj where
>   bindP x = bindP (Just x)

The default instance, uses generic Show

> instance (Show a) => DBBind (Maybe a) Session PreparedStmtObj BindObj where
>   bindP (Just x) = bindP (Just (show x))
>   bindP Nothing = bindP (Nothing `asTypeOf` Just "")

> makeBindAction x = BindA (\ses st -> bindMaybe (dbHandle ses) (stmtHandle st) x)


> class SqliteBind a where
>   stmtBind :: DBHandle -> StmtHandle -> Int -> a -> IO ()

> instance SqliteBind Int where stmtBind = DBAPI.bindInt
> instance SqliteBind Int64 where stmtBind = DBAPI.bindInt64
> instance SqliteBind String where stmtBind = DBAPI.bindString
> instance SqliteBind Double where stmtBind = DBAPI.bindDouble
> instance SqliteBind CalendarTime where
>   stmtBind db stmt pos val =
>     DBAPI.bindInt64 db stmt pos (calTimeToInt64 val)
> instance SqliteBind UTCTime where
>   stmtBind db stmt pos val =
>     DBAPI.bindInt64 db stmt pos (utcTimeToInt64 val)

> bindMaybe :: (SqliteBind a)
>   => DBHandle -> StmtHandle -> Maybe a -> Int -> IO ()
> bindMaybe db stmt mval pos = convertEx $
>   case mval of
>     Nothing -> DBAPI.bindNull db stmt pos
>     Just val -> stmtBind db stmt pos val



--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

> data Query = Query
>   { queryStmt :: PreparedStmtObj
>   , querySess :: Session
>   , queryCount :: IORef Int
>   }

> data StmtBind = StmtBind String [BindA Session PreparedStmtObj BindObj]

> sqlbind :: String -> [BindA Session PreparedStmtObj BindObj] -> StmtBind
> sqlbind sql bas = StmtBind sql bas

> cmdbind :: String -> [BindA Session PreparedStmtObj BindObj] -> StmtBind
> cmdbind sql bas = StmtBind sql bas

> prefetch :: Int -> String -> [BindA Session PreparedStmtObj BindObj] -> StmtBind
> prefetch n sql bas = StmtBind sql bas


> instance Statement BoundStmt Session Query where
>   makeQuery sess bstmt = do
>     n <- newIORef 0
>     return (Query (boundStmt bstmt) sess n)

> instance Statement PreparedStmtObj Session Query where
>   makeQuery sess pstmt = do
>     n <- newIORef 0
>     return (Query pstmt sess n)

> instance Statement StmtBind Session Query where
>   makeQuery sess (StmtBind sqltext bas) = do
>     let (PreparationA action) = prepareStmt' sqltext True
>     pstmt <- action sess
>     sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>     n <- newIORef 0
>     return (Query pstmt sess n)

> instance Statement QueryString Session Query where
>   makeQuery sess (QueryString sqltext) = makeQuery sess sqltext

> instance Statement String Session Query where
>   makeQuery sess sqltext = do
>     let (PreparationA action) = prepareStmt' sqltext True
>     pstmt <- action sess
>     n <- newIORef 0
>     return (Query pstmt sess n)


> instance IQuery Query Session ColumnBuffer where
>   destroyQuery query =
>     if (stmtFreeWithQuery (queryStmt query))
>       then finaliseStmt (dbHandle (querySess query)) (stmtHandle (queryStmt query))
>       else resetStmt (dbHandle (querySess query)) (stmtHandle (queryStmt query))
>   fetchOneRow query = do
>     rc <- fetchRow (dbHandle (querySess query)) (stmtHandle (queryStmt query))
>     modifyIORef (queryCount query) (+1)
>     return (rc /= DBAPI.sqliteDONE)
>   currentRowNum q = readIORef (queryCount q)
>   freeBuffer q buffer = return ()


> nullIf :: Bool -> a -> Maybe a
> nullIf test v = if test then Nothing else Just v

> bufferToString query buffer =
>   DBAPI.colValString (stmtHandle (queryStmt query)) (colPos buffer)

> bufferToInt query buffer = do
>   v <- DBAPI.colValInt (stmtHandle (queryStmt query)) (colPos buffer)
>   return (Just v)

> bufferToInt64 query buffer = do
>   v <- DBAPI.colValInt64 (stmtHandle (queryStmt query)) (colPos buffer)
>   return (Just v)

> bufferToDouble query buffer = do
>   v <- DBAPI.colValDouble (stmtHandle (queryStmt query)) (colPos buffer)
>   return (Just v)

> nullDatetimeInt64 :: Int64
> nullDatetimeInt64 = 99999999999999

> bufferToCalTime query buffer = do
>   v <- DBAPI.colValInt64 (stmtHandle (queryStmt query)) (colPos buffer)
>   return (nullIf (v == 0 || v == nullDatetimeInt64) (int64ToCalTime v))

> bufferToUTCTime query buffer = do
>   v <- DBAPI.colValInt64 (stmtHandle (queryStmt query)) (colPos buffer)
>   return (nullIf (v == 0 || v == nullDatetimeInt64) (int64ToUTCTime v))


|There aren't really Buffers to speak of with Sqlite,
so we just record the position of each column.
We also keep a reference to the Query which owns the buffer,
as we need it to get column values.

> data ColumnBuffer = ColumnBuffer
>   { colPos :: Int
>   , colQuery :: Query
>   }

> allocBuffer q colpos = return $ ColumnBuffer { colPos = colpos, colQuery = q }

> buffer_pos q buffer = do
>   row <- currentRowNum q
>   return (row,colPos buffer)


> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol q buffer = bufferToString q buffer

> instance DBType (Maybe Int) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol q buffer = bufferToInt q buffer

> instance DBType (Maybe Int64) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol q buffer = bufferToInt64 q buffer

> instance DBType (Maybe Double) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol q buffer = bufferToDouble q buffer

> instance DBType (Maybe CalendarTime) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer undefined n
>   fetchCol q buffer = bufferToCalTime q buffer

> instance DBType (Maybe UTCTime) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer undefined n
>   fetchCol q buffer = bufferToUTCTime q buffer


|This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor _ q n = allocBufferFor (undefined::Maybe a) q n
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) (fetchCol q buffer)


|This polymorphic instance assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer undefined n
>   fetchCol q buffer = do
>     v <- bufferToString q buffer
>     case v of
>       Just s -> if s == "" then return Nothing else return (Just (read s))
>       Nothing -> return Nothing

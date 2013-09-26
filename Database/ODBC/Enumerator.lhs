
|
Module      :  Database.ODBC.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

ODBC implementation of Database.Enumerator.


Notes on ODBC query processing:
Query rebinding (i.e. prepared-stmt reuse) is awkward
(at least for MS SQL Server):

 * For a normal "select", when you are done fetching,
   you must call closeCursor before you attempt to rebind.

 * For a procedure call returning result-sets,
   again (I think) you must call closeCursor before rebinding.
   Don't call it until you've processed all result-sets,
   because it will close all of them, including any unprocessed.

 * For a procedure call that does not return a result-set,
   closeCursor will raise an error.

 * If you call closeCursor after preparing the stmt,
   but before binding and executing, this will cause an error.
   This means we can't simply call closeCursor before every bind.

So when do we call closeCursor?
Before binding, but only if the prepared-stmt object
indicates that some rows have already been fetched.


> {-# OPTIONS -fglasgow-exts #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE UndecidableInstances #-}

> module Database.ODBC.Enumerator
>   ( Session, connect
>   , prepareStmt
>   , prepareQuery, prepareLargeQuery, prepareCommand
>   , sql, sqlbind, prefetch, cmdbind
>   , Out(..), InfoDbmsName(..)
>   , CharEncoding(..), setStringEnc
>   , module Database.Enumerator
>   )
> where


> import Database.Enumerator
> import Database.InternalEnumerator
> import Database.Util
> import Foreign.C
> import Foreign.Storable (sizeOf)
> import Control.Monad
> import Control.Exception.Extensible (bracket)
> import Database.ODBC.OdbcFunctions
>   (EnvHandle, ConnHandle, StmtHandle, CharEncoding(..), OdbcException(..), catchOdbc, throwOdbc)
> import qualified Database.ODBC.OdbcFunctions as DBAPI
> import Control.Monad.Trans
> import Control.Monad.Reader
> import Data.Char (toLower)
> import Data.Dynamic
> import Data.IORef
> import Data.Int
> import Data.List (isPrefixOf)
> import System.Time
> import Data.Time


> --debugStmt s = putStrLn s
> debugStmt s = return ()

--------------------------------------------------------------------
-- ** API Wrappers
--------------------------------------------------------------------

|These wrappers ensure that only DBExceptions are thrown,
and never SqliteExceptions.
We don't need wrappers for the colValXxx functions
because they never throw exceptions.


> convertAndRethrow :: OdbcException -> IO a
> convertAndRethrow (OdbcException e st m exs) = do
>   let
>     statepair@(stateclass, statesubclass) = (take 2 st, drop 2 st)
>     ec = case stateclass of
>       "XX" -> DBFatal
>       "58" -> DBFatal
>       "57" -> DBFatal
>       "54" -> DBFatal
>       "53" -> DBFatal
>       "08" -> DBFatal
>       _ -> DBError
>   throwDB (ec statepair e m)


|Common case: wrap an action with a convertAndRethrow.

> convertEx :: IO a -> IO a
> convertEx action = catchOdbc action convertAndRethrow

> stmtPrepare :: ConnHandle -> String -> IO StmtHandle
> stmtPrepare conn sqltext = convertEx $ do
>   stmt <- DBAPI.allocStmt conn
>   DBAPI.prepareStmt stmt sqltext
>   return stmt

> stmtExecute :: StmtHandle -> IO ()
> stmtExecute stmt = convertEx (DBAPI.executeStmt stmt)

> closeCursor :: StmtHandle -> IO ()
> closeCursor stmt = debugStmt "closeCursor" >> convertEx (DBAPI.closeCursor stmt)

> fetchRow :: StmtHandle -> IO Bool
> fetchRow stmt = convertEx (DBAPI.fetch stmt)

> rowCount :: StmtHandle -> IO Int
> rowCount stmt = convertEx (DBAPI.rowCount stmt)

> freeStmt stmt = debugStmt "freeStmt" >> convertEx (DBAPI.freeStmt stmt)
> freeConn conn = convertEx (DBAPI.freeConn conn)
> freeEnv env = convertEx (DBAPI.freeEnv env)

> connectDb connstr = convertEx $ do
>   env <- DBAPI.allocEnv
>   DBAPI.setOdbcVer env
>   conn <- DBAPI.allocConn env
>   connstr <- DBAPI.connect conn connstr
>   dbms <- DBAPI.getInfoDbmsName conn
>   DBAPI.setAutoCommitOff conn
>   return (env, conn { DBAPI.connDbms = map toLower dbms } )

> disconnectDb conn = convertEx (DBAPI.disconnect conn)

> commitTrans conn = convertEx (DBAPI.commit conn)
> rollbackTrans conn = convertEx (DBAPI.rollback conn)

> setTransLevel conn level = convertEx (DBAPI.setTxnIsolation conn level)

--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------

We don't need much in a Session record.
Session objects are created by 'connect'.

> data Session = Session
>   { envHandle :: EnvHandle
>   , connHandle :: ConnHandle }
>   deriving Typeable

> connect :: String -> ConnectA Session
> connect connstr = ConnectA $ do
>   (env, conn) <- connectDb connstr
>   return (Session env conn)


We need a way to set the char encoding for String marshaling.
EnvInquiry.inquire gives us a way to run arbitrary actions
in the DBM monad, so make an instance for it.

> instance EnvInquiry CharEncoding Session () where
>   inquire charenc sess = DBAPI.setConnEncoding (connHandle sess) charenc

Also make a synonym for inquire, because "inquire EncUTF8" will look odd,
and non-obvious.

> setStringEnc enc = Database.Enumerator.inquire enc

Note: the PostgreSQL and Oracle ODBC drivers only support ReadCommitted
and Serializable. You get an error on other values.
It'd be nicer if they just silently upgraded, but c'est la vie...

Apparently MS SQL Server upgrades RepeatableRead to Serializable.
Presumably the other modes are still supported.

> isolationLevel ReadUncommitted = DBAPI.sqlTxnReadUncommitted
> isolationLevel ReadCommitted = DBAPI.sqlTxnReadCommitted
> isolationLevel RepeatableRead = DBAPI.sqlTxnRepeatableRead
> isolationLevel Serialisable = DBAPI.sqlTxnSerializable
> isolationLevel Serializable = DBAPI.sqlTxnSerializable

> instance ISession Session where
>   disconnect sess = do
>     disconnectDb (connHandle sess)
>     freeConn (connHandle sess)
>     freeEnv (envHandle sess)
>   -- With ODBC, transactions are implicitly started,
>   -- so all we do is commit the previous transaction
>   -- (probably done already) and set isolation level.
>   beginTransaction sess isolation = do
>     commitTrans (connHandle sess)
>     setTransLevel (connHandle sess) (isolationLevel isolation)
>   commit sess = commitTrans (connHandle sess)
>   rollback sess = rollbackTrans (connHandle sess)

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
>       (stmtPrepare (connHandle sess) str)
>       (freeStmt)
>       ( \stmt -> do
>         stmtExecute stmt
>         liftM fromIntegral (rowCount stmt)
>       )

> instance Command StmtBind Session where
>   executeCommand sess (StmtBind sqltext bas) = do
>     let (PreparationA action) = prepareStmt' sqltext FreeManually
>     bracket
>       (action sess)
>       (freeStmt . stmtHandle)
>       (\pstmt -> do
>         freeBindBuffers pstmt
>         sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>         stmtExecute (stmtHandle pstmt)
>         rowCount (stmtHandle pstmt)
>       )

> instance Command BoundStmt Session where
>   executeCommand sess (BoundStmt pstmt) =
>     rowCount (stmtHandle pstmt)


> data InfoDbmsName = InfoDbmsName

> instance EnvInquiry InfoDbmsName Session String where
>   inquire InfoDbmsName sess =
>     --liftM (map toLower) (DBAPI.getInfoDbmsName (connHandle sess))
>     return (DBAPI.connDbms (connHandle sess))


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

> data StmtLifetime = FreeWithQuery | FreeManually
>   deriving (Eq)

> data PreparedStmtObj = PreparedStmtObj
>   { stmtHandle :: StmtHandle
>   , stmtLifetime :: StmtLifetime
>   , stmtBuffers :: IORef [ColumnBuffer]
>   -- We package output bind buffers to look like ColumnBuffers
>   -- (as created by allocBuffer) so that we can use them
>   -- like ColumnBuffers.
>   , stmtOutputBuffers :: IORef [ColumnBuffer]
>   , stmtFetched :: IORef Bool
>   }

> prepareStmt :: QueryString -> PreparationA Session PreparedStmtObj
> prepareStmt (QueryString sqltext) = prepareStmt' sqltext FreeManually

> prepareQuery :: QueryString -> PreparationA Session PreparedStmtObj
> prepareQuery q = prepareStmt q

> prepareLargeQuery :: Int -> QueryString -> PreparationA Session PreparedStmtObj
> prepareLargeQuery _ q = prepareStmt q

> prepareCommand :: QueryString -> PreparationA Session PreparedStmtObj
> prepareCommand q = prepareStmt q

> prepareStmt' sqltext lifetime =
>   PreparationA (\sess -> do
>     stmt <- stmtPrepare (connHandle sess) sqltext
>     newPreparedStmt stmt lifetime
>     )

> newPreparedStmt stmt lifetime = do
>   b <- newIORef []
>   ob <- newIORef []
>   f <- newIORef False
>   return (PreparedStmtObj stmt lifetime b ob f)

> freeBindBuffers stmt = do
>   writeIORef (stmtBuffers stmt) []
>   writeIORef (stmtOutputBuffers stmt) []

--------------------------------------------------------------------
-- ** Binding
--------------------------------------------------------------------

> newtype BoundStmt = BoundStmt { boundStmt :: PreparedStmtObj }
> type BindObj = Int -> IO DBAPI.BindBuffer
> newtype Out a = Out a

> instance IPrepared PreparedStmtObj Session BoundStmt BindObj where
>   bindRun sess pstmt bas action = do
>     fetched <- readIORef (stmtFetched pstmt)
>     when (fetched)
>       (closeCursor (stmtHandle pstmt) >> writeIORef (stmtFetched pstmt) False)
>     freeBindBuffers pstmt
>     sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>     stmtExecute (stmtHandle pstmt)
>     action (BoundStmt pstmt)
>   destroyStmt sess pstmt = do
>     -- Could free output bind buffers here, but for now we don't bother.
>     -- They are ForeignPtrs, so we expect them to be GC'd.
>     freeStmt (stmtHandle pstmt)


> instance DBBind (Maybe String) Session PreparedStmtObj BindObj where
>   bindP val = makeBindAction val DBAPI.bindParamBuffer 0

> instance DBBind (Out (Maybe String)) Session PreparedStmtObj BindObj where
>   -- SQL Server will raise an error if buffer size > 7999
>   bindP (Out val) = makeOutputBindAction (DBAPI.InOutParam val) DBAPI.bindParamBuffer 7999


> instance DBBind (Maybe Int) Session PreparedStmtObj BindObj where
>   bindP val = makeBindAction val DBAPI.bindParamBuffer 0

> instance DBBind (Out (Maybe Int)) Session PreparedStmtObj BindObj where
>   bindP (Out val) = makeOutputBindAction (DBAPI.InOutParam val) DBAPI.bindParamBuffer 0


> instance DBBind (Maybe Double) Session PreparedStmtObj BindObj where
>   bindP val = makeBindAction val DBAPI.bindParamBuffer 0

> instance DBBind (Out (Maybe Double)) Session PreparedStmtObj BindObj where
>   bindP (Out val) = makeOutputBindAction (DBAPI.InOutParam val) DBAPI.bindParamBuffer 0


> instance DBBind (Maybe UTCTime) Session PreparedStmtObj BindObj where
>   bindP val = makeBindAction val DBAPI.bindParamBuffer 0

> instance DBBind (Out (Maybe UTCTime)) Session PreparedStmtObj BindObj where
>   bindP (Out val) = makeOutputBindAction (DBAPI.InOutParam val) DBAPI.bindParamBuffer 0


> instance DBBind (Maybe a) Session PreparedStmtObj BindObj
>     => DBBind a Session PreparedStmtObj BindObj where
>   bindP x = bindP (Just x)

> instance DBBind (Out (Maybe a)) Session PreparedStmtObj BindObj
>     => DBBind (Out a) Session PreparedStmtObj BindObj where
>   bindP (Out x) = bindP (Out (Just x))


The default instance, uses generic Show

> instance (Show a) => DBBind (Maybe a) Session PreparedStmtObj BindObj where
>   bindP (Just x) = bindP (Just (show x))
>   bindP Nothing = bindP (Nothing `asTypeOf` Just "")

> instance (Show a) => DBBind (Out (Maybe a)) Session PreparedStmtObj BindObj where
>   bindP (Out (Just x)) = bindP (Out (Just (show x)))
>   bindP (Out Nothing) = bindP (Out (Nothing `asTypeOf` Just ""))

> makeBindAction val binder size = BindA (\sess stmt pos -> do
>   convertEx (do
>     buf <- binder (stmtHandle stmt) pos val size
>     appendBindBuffer stmt buf
>     ))

> makeOutputBindAction val binder size = BindA (\sess stmt pos -> do
>   convertEx (do
>     buf <- binder (stmtHandle stmt) pos val size
>     appendBindBuffer stmt buf
>     appendOutputBindBuffer stmt buf
>     ))

> appendBindBuffer stmt buffer = do
>   buffers <- readIORef (stmtBuffers stmt)
>   let colbuf = ColumnBuffer (1 + length buffers) buffer
>   modifyIORef (stmtBuffers stmt) (++ [colbuf])
>   return buffer

> appendOutputBindBuffer stmt buffer = do
>   buffers <- readIORef (stmtOutputBuffers stmt)
>   let colbuf = ColumnBuffer (1 + length buffers) buffer
>   modifyIORef (stmtOutputBuffers stmt) (++ [colbuf])
>   return buffer


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
>     stmtExecute (stmtHandle pstmt)
>     n <- newIORef 0
>     return (Query pstmt sess n)

> instance Statement StmtBind Session Query where
>   makeQuery sess (StmtBind sqltext bas) = do
>     let (PreparationA action) = prepareStmt' sqltext FreeWithQuery
>     pstmt <- action sess
>     freeBindBuffers pstmt
>     sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>     stmtExecute (stmtHandle pstmt)
>     n <- newIORef 0
>     return (Query pstmt sess n)

> instance Statement QueryString Session Query where
>   makeQuery sess (QueryString sqltext) = makeQuery sess sqltext

> instance Statement String Session Query where
>   makeQuery sess sqltext = makeQuery sess (StmtBind sqltext [])

> instance Statement (NextResultSet mark PreparedStmtObj) Session Query where
>   makeQuery sess (NextResultSet (PreparedStmt pstmt)) = do
>     -- If stmt buffers are present, then the first doQuery
>     -- will have processed its results from there.
>     -- So for the next query, we want to clear the buffer list
>     -- and start fetching from the stmt handle.
>     -- This allows us to call stored procedures that return
>     -- both output parameters and (multiple) result-sets.
>     buffers <- readIORef (stmtOutputBuffers pstmt)
>     if null buffers
>       then do
>         more <- DBAPI.moreResults (stmtHandle pstmt)
>         when (not more) (throwDB (DBError ("02", "000") (-1)
>           "NextResultSet was used, but there are no more result-sets to process"))
>         n <- newIORef 0
>         return (Query pstmt sess n)
>       else do
>         writeIORef (stmtBuffers pstmt) []
>         n <- newIORef 0
>         return (Query pstmt sess n)

> instance IQuery Query Session ColumnBuffer where
>   destroyQuery query = do
>     let pstmt = queryStmt query
>     when (stmtLifetime pstmt == FreeWithQuery)
>       (freeBindBuffers pstmt >> freeStmt (stmtHandle pstmt))
>   fetchOneRow query = do
>     let pstmt = queryStmt query
>     -- Only call fetchRow if there are no bind output buffers
>     -- If there are bind output buffers then assume this was a
>     -- procedure call.
>     -- In this case you will always get the same row over and over,
>     -- so you'd better be careful with your iteratees
>     buffers <- readIORef (stmtOutputBuffers pstmt)
>     if not (null buffers) then return True
>       else do
>         moreRows <- fetchRow (stmtHandle pstmt)
>         writeIORef (stmtFetched pstmt) True
>         modifyIORef (queryCount query) (+1)
>         return moreRows
>   currentRowNum q = readIORef (queryCount q)
>   freeBuffer q buffer = return ()


> data ColumnBuffer = ColumnBuffer
>   { colPos :: Int
>   , colBuffer :: DBAPI.BindBuffer
>   }

In allocBuffer we decide to either
 - create a new buffer - this is a real column buffer for a query - , or
 - return an already-allocated output bind-buffer.

> allocBuffer q pos size val = do
>   let stmt = queryStmt q
>   buffers <- readIORef (stmtOutputBuffers stmt)
>   if null buffers
>     then do
>       bindbuffer <- convertEx (DBAPI.bindColBuffer (stmtHandle stmt) pos size val)
>       return (ColumnBuffer pos bindbuffer)
>     else do
>       if length buffers >= pos
>         then return (buffers !! (pos - 1))
>         else
>           throwDB (DBError ("02", "000") (-1) ( "There are " ++ show (length buffers)
>             ++ " output buffers, but you have asked for buffer " ++ show pos ))

> buffer_pos q buffer = do
>   row <- currentRowNum q
>   return (row,colPos buffer)


> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 32000 v
>   fetchCol q buffer = convertEx (DBAPI.getFromBuffer (colBuffer buffer))

> instance DBType (Maybe Int) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 0 v
>   fetchCol q buffer = convertEx (DBAPI.getFromBuffer (colBuffer buffer))

> instance DBType (Maybe Double) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 0 v
>   fetchCol q buffer = convertEx (DBAPI.getFromBuffer (colBuffer buffer))

> instance DBType (Maybe UTCTime) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 32 v
>   fetchCol q buffer = convertEx (DBAPI.getFromBuffer (colBuffer buffer))


|This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor v q n = allocBufferFor (Just v) q n
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) (fetchCol q buffer)


|This polymorphic instance assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor v q n = allocBuffer q n 32000 (Just "")
>   fetchCol q buffer = do
>     v <- convertEx (DBAPI.getFromBuffer (colBuffer buffer))
>     case v of
>       Just s -> if s == "" then return Nothing else return (Just (read s))
>       Nothing -> return Nothing

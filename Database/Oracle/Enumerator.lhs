
|
Module      :  Database.Oracle.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Oracle OCI implementation of Database.Enumerator.


> {-# OPTIONS -fglasgow-exts #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE UndecidableInstances #-}

> module Database.Oracle.Enumerator
>   ( Session, connect
>   , prepareQuery, prepareLargeQuery, prepareCommand, prepareLargeCommand
>   , sql, sqlbind, prefetch, cmdbind
>   , StmtHandle, Out(..)
>   , module Database.Enumerator
>   )
> where


> import Database.Enumerator
> import Database.InternalEnumerator
> import Database.Oracle.OCIConstants
> import qualified Database.Oracle.OCIFunctions as OCI
> import Database.Oracle.OCIFunctions
>   ( OCIHandle, EnvHandle, ErrorHandle, ServerHandle, ConnHandle, SessHandle, StmtHandle
>   , OCIException (..), catchOCI)
> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception.Extensible
> import Control.Monad.Trans
> import Control.Monad.Reader
> import Data.Char (toLower)
> import Data.Dynamic
> import Data.List (isPrefixOf)
> import Data.IORef
> import Data.Int
> import Data.Time
> import System.Time
> import System.IO (hPutStrLn, stderr)


--------------------------------------------------------------------
-- ** Error handling
--------------------------------------------------------------------

> nullAction :: IO ()
> nullAction = return ()


> between i (l, u) = i >= l && i <= u

Where did I find these mappings?...

> errorSqlState :: Int -> (String, String)
> errorSqlState 0 = ("00", "000")
> -- 02 - no data
> errorSqlState 1403 = ("02", "000")
> errorSqlState 1095 = ("02", "000")
> -- 23 - integrity violation
> errorSqlState 1 = ("23", "000")
> errorSqlState e | e >= 2290 && e <= 2299 = ("23", "000")
> -- 42 - syntax error or access rule violation
> errorSqlState 22 = ("42", "000")
> errorSqlState 251 = ("42", "000")
> errorSqlState e | e `between` (900, 999) = ("42", "000")
> errorSqlState 1031 = ("42", "000")
> errorSqlState e | e `between` (1490, 1493) = ("42", "000")
> errorSqlState e | e `between` (1700, 1799) = ("42", "000")
> errorSqlState e | e `between` (1900, 2099) = ("42", "000")
> errorSqlState e | e `between` (2140, 2289) = ("42", "000")
> errorSqlState e | e `between` (2420, 2424) = ("42", "000")
> errorSqlState e | e `between` (2450, 2499) = ("42", "000")
> errorSqlState e | e `between` (3276, 3299) = ("42", "000")
> errorSqlState e | e `between` (4040, 4059) = ("42", "000")
> errorSqlState e | e `between` (4070, 4099) = ("42", "000")
> -- 08 - connection errors
> errorSqlState 12154 = ("08", "001") -- TNS: can't resolve service name
> -- unspecified error
> errorSqlState _ = ("01", "000")

> throwSqlError e m = do
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


|rethrow converts an OCIException to a DBException.
The third parameter is an IO action that you can use to clean up any handles.
First we get the error message from the Env or ErrorHandle,
and then we run the cleanup action to free any allocated handles.
(Obviously, we must extract the error message _before_ we free the handles.)
If there's no cleanup action required then simply pass nullAction.

> class OCIExceptionHandler a where
>   rethrow :: a -> OCIException -> IO () -> IO b

> instance OCIExceptionHandler ErrorHandle where
>   rethrow err ex finaliser = do
>     (e, m) <- OCI.formatErrorMsg ex err
>     finaliser
>     throwSqlError e m

> instance OCIExceptionHandler EnvHandle where
>   rethrow env ex finaliser = do
>     (e, m) <- OCI.formatEnvMsg ex env
>     finaliser
>     throwSqlError e m


|What do we do if creating the first handle (Environment) fails?
There's no Env- or ErrorHandle to get the error message from,
so do the best we can by constructing a message with formatErrorCodeDesc.
Still throws DBException.

> reportOCIExc :: OCIException -> IO a
> reportOCIExc (OCIException e m) = do
>   let s = OCI.formatErrorCodeDesc e m
>   printError s
>   throwDB (DBError (errorSqlState 0) 0 s)
>   return undefined

> printError :: String -> IO ()
> printError s = hPutStrLn stderr s

--------------------------------------------------------------------
-- ** OCI Function Wrappers
--------------------------------------------------------------------

These wrappers ensure that only DBExceptions are thrown,
and never OCIExceptions.

> data Session = Session 
>   { envHandle :: EnvHandle
>   , errorHandle :: ErrorHandle
>   , connHandle :: ConnHandle
>   }
>   deriving Typeable

> class FreeHandle ht where dispose :: ht -> IO ()

> instance FreeHandle EnvHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_ENV
> instance FreeHandle ErrorHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_ERROR
> instance FreeHandle ServerHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_SERVER
> instance FreeHandle ConnHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_SVCCTX
> instance FreeHandle SessHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_SESSION
> instance FreeHandle StmtHandle where dispose h = freeHandle (castPtr h) oci_HTYPE_STMT


|Reports and ignores any errors when freeing handles.
Will catch attempts to free invalid (already freed?) handles.

> freeHandle :: OCIHandle -> CInt -> IO ()
> freeHandle ocihandle handleType = catchOCI ( do
>     OCI.handleFree handleType ocihandle
>   ) (\(OCIException e m) -> do
>     let s = OCI.formatErrorCodeDesc e m
>     printError s
>   )

|Assumes that if an exception is raised,
the Env and Error handles should be freed.

> inOCI :: EnvHandle -> ErrorHandle -> IO a -> IO a
> inOCI env err action = catchOCI action $ \e -> do
>   rethrow err e $ do
>     dispose err
>     dispose env


|Does not free handles when exception raised.

> inSession :: Session -> (EnvHandle -> ErrorHandle -> ConnHandle -> IO a) -> IO () -> IO a
> inSession session action finaliser = do
>   let
>     env = envHandle session
>     err = errorHandle session
>     conn = connHandle session
>   catchOCI (action env err conn) (\e -> rethrow err e finaliser)


> getEnv :: IO EnvHandle
> getEnv = catchOCI OCI.envCreate reportOCIExc

> getErr :: EnvHandle -> IO ErrorHandle
> getErr env = catchOCI ( do
>     err <- OCI.handleAlloc oci_HTYPE_ERROR (castPtr env)
>     return (castPtr err)
>   ) (\e -> rethrow env e (dispose env))


> getServer :: EnvHandle -> ErrorHandle -> IO ServerHandle
> getServer env err = inOCI env err $ do
>     server <- OCI.handleAlloc oci_HTYPE_SERVER (castPtr env)
>     return (castPtr server)


> getConnection :: EnvHandle -> ErrorHandle -> IO ConnHandle
> getConnection env err = inOCI env err $ do
>     conn <- OCI.handleAlloc oci_HTYPE_SVCCTX (castPtr env)
>     return (castPtr conn)


> getSessionHandle :: EnvHandle -> ErrorHandle -> IO SessHandle
> getSessionHandle env err = inOCI env err $ do
>     session <- OCI.handleAlloc oci_HTYPE_SESSION (castPtr env)
>     return (castPtr session)


|The idea with multiple logons is to first connect to the server.
Then you create a connection and a session, set the user id details,
and begin the session.
When finished, you end the session,
detach from the server, and free the handles.
So we should have, globally, one EnvHandle and one ErrorHandle,
and then, per session, one ServerHandle, one ConnHandle, and one SessHandle.
Also, for each server (or Instance, in Oracle-speak), we could share
the ServerHandle among the many ConnHandles and SessHandles.
At the moment we're being lazy,
and not reusing the Env and ErrorHandles for new connections.

> startServerSession :: String -> String -> EnvHandle -> ErrorHandle -> ServerHandle -> IO ConnHandle
> startServerSession user pswd env err server = do
>     conn <- getConnection env err
>     -- the connection holds a reference to the server in one of its attributes
>     OCI.setHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX (castPtr server) oci_ATTR_SERVER
>     session <- getSessionHandle env err
>     if (user == "")
>       then do
>         OCI.sessionBegin err conn session oci_CRED_EXT
>       else do
>         OCI.setHandleAttrString err (castPtr session) oci_HTYPE_SESSION user oci_ATTR_USERNAME
>         OCI.setHandleAttrString err (castPtr session) oci_HTYPE_SESSION pswd oci_ATTR_PASSWORD
>         OCI.sessionBegin err conn session oci_CRED_RDBMS
>     -- the connection also holds a reference to the session in one of its attributes
>     OCI.setHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX (castPtr session) oci_ATTR_SESSION
>     -- and we need to create a valid transaction handle for the connection, too.
>     trans <- OCI.handleAlloc oci_HTYPE_TRANS (castPtr env)
>     OCI.setHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX (castPtr trans) oci_ATTR_TRANS
>     return conn


> logon :: String -> String -> String -> EnvHandle -> ErrorHandle -> IO ConnHandle
> logon user pswd dbname env err = inOCI env err $ do
>     server <- getServer env err
>     OCI.serverAttach err server dbname
>     startServerSession user pswd env err server


> logoff :: ErrorHandle -> ConnHandle -> IO ()  
> logoff err conn = catchOCI (do
>     session <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SESSION
>     server <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SERVER
>     OCI.sessionEnd err conn session
>     OCI.serverDetach err server
>     dispose session
>     dispose conn
>     dispose server
>   ) (\e -> rethrow err e nullAction)




> dbConnect :: String -> String -> String -> IO Session
> dbConnect user pswd dbname = do
>   env <- getEnv
>   err <- getErr env
>   conn <- logon user pswd dbname env err
>   return (Session env err conn)



> dbDisconnect :: Session -> IO ()
> dbDisconnect session = do
>   let
>     env = envHandle session
>     err = errorHandle session
>     conn = connHandle session
>   logoff err conn
>   dispose err
>   dispose env
>   OCI.terminate


|Oracle only supports ReadCommitted and Serialisable.
If you ask for RepeatableRead, we must go one better and choose Serialisable
(ReadCommitted is no good because you can get non-reapeatable reads).
Oracle has a ReadOnly mode which will give you RepeatableRead,
but you can't do any updates.

Oracle's default (and weakest) behaviour is ReadCommitted;
there's no equivalent for ReadUncommitted.

> beginTrans :: Session -> IsolationLevel -> IO ()
> beginTrans session isolation = inSession session 
>   (\_ err conn -> do
>       case isolation of
>         ReadUncommitted -> OCI.beginTrans err conn oci_TRANS_READWRITE
>         ReadCommitted -> OCI.beginTrans err conn oci_TRANS_READWRITE
>         RepeatableRead -> OCI.beginTrans err conn oci_TRANS_SERIALIZABLE
>         Serialisable -> OCI.beginTrans err conn oci_TRANS_SERIALIZABLE
>         Serializable -> OCI.beginTrans err conn oci_TRANS_SERIALIZABLE
>   ) nullAction



> commitTrans :: Session -> IO ()
> commitTrans session = inSession session
>   (\_ err conn -> OCI.commitTrans err conn)
>   nullAction

> rollbackTrans :: Session -> IO ()
> rollbackTrans session = inSession session
>   (\_ err conn -> OCI.rollbackTrans err conn)
>   nullAction



> getStmt :: Session -> IO StmtHandle
> getStmt session = inSession session
>   (\ env err _ -> do
>       stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       return (castPtr stmt)
>   ) nullAction


> closeStmt :: Session -> StmtHandle -> IO ()
> closeStmt _ stmt = dispose stmt


FIXME stmt should not be closed by these functions,
because they don't create it.
stmt should be closed in function that creates it.

> setPrefetchCount :: Session -> StmtHandle -> Int -> IO ()
> setPrefetchCount session stmt count = inSession session
>   (\_ err _ -> with count $ \countPtr ->
>         OCI.setHandleAttr err (castPtr stmt) oci_HTYPE_STMT countPtr oci_ATTR_PREFETCH_ROWS
>   ) (closeStmt session stmt)


> stmtPrepare :: Session -> StmtHandle -> String -> IO ()
> stmtPrepare session stmt sql = inSession session
>   (\_ err _ -> OCI.stmtPrepare err stmt sql
>   ) (closeStmt session stmt)


> word32ToInt :: Word32 -> Int
> word32ToInt n = fromIntegral n

> getRowCount :: Session -> StmtHandle -> IO Int
> getRowCount session stmt = inSession session
>   (\_ err _ -> do
>       rc <- OCI.getHandleAttr err (castPtr stmt) oci_HTYPE_STMT oci_ATTR_ROW_COUNT
>       return (word32ToInt rc)
>   ) (closeStmt session stmt)


> execute :: Session -> StmtHandle -> Int -> IO Int
> execute session stmt iterations = inSession session
>   (\_ err conn -> do
>       OCI.stmtExecute err conn stmt iterations
>       getRowCount session stmt
>   ) (closeStmt session stmt)


> fetchRow :: Session -> PreparedStmtObj -> IO CInt
> fetchRow session stmt = inSession session
>   (\_ err _ -> OCI.stmtFetch err (stmtHandle stmt))
>   nullAction  -- cleanup handled by doQuery1Maker


> defineCol :: Session -> PreparedStmtObj -> Int -> Int -> CInt -> IO OCI.ColumnInfo
> defineCol session stmt posn bufsize sqldatatype = inSession session
>   (\_ err _ -> OCI.defineByPos err (stmtHandle stmt) posn bufsize sqldatatype)
>   (closeStmt session (stmtHandle stmt))

> bindByPos :: Session -> PreparedStmtObj -> Int -> CShort -> OCI.BufferPtr -> Int -> CInt -> IO ()
> bindByPos session stmt posn nullind val bufsize sqldatatype = inSession session
>   (\_ err _ -> OCI.bindByPos err (stmtHandle stmt) posn nullind val bufsize sqldatatype)
>   (closeStmt session (stmtHandle stmt))

> bindOutputByPos :: Session -> PreparedStmtObj -> Int -> OCI.BindBuffer -> Int -> CInt -> IO OCI.BindHandle
> bindOutputByPos session stmt posn buffer bufsize sqldatatype = inSession session
>   (\_ err _ -> OCI.bindOutputByPos err (stmtHandle stmt) posn buffer bufsize sqldatatype)
>   (closeStmt session (stmtHandle stmt))

--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------

> connect :: String -> String -> String -> ConnectA Session
> connect user pswd dbname = ConnectA (dbConnect user pswd dbname)

--------------------------------------------------------------------
-- Statements and Commands
--------------------------------------------------------------------

==== A wrapper for SQL statements ====

When we execute queries with the OCI,
we need to specify how many times the statement should be executed.
For select statements it is zero (!?), while everything else is one
(>1 executions is for array binds).

For a select, if you have defined output buffers then the iterations
parameter for 'execute'
says how many rows to put in the buffers immediately after the execute
(so there's no need for a call to OCIStmtFetch).
We don't use this, and set up the buffers after the execute.

So there's a distinction between queries and commands.
Note that PL/SQL blocks which populate output bind buffers
are treated like queries by our library,
so we need some way of distinguishing between queries and commands.

> newtype QueryString = QueryString String

> sql :: String -> QueryString
> sql str = QueryString str


> instance Command QueryString Session where
>   executeCommand sess (QueryString str) = doCommand sess str

> doCommand sess str = do
>   stmt <- getStmt sess
>   -- stmtPrepare and execute both close the stmt if an exception is thrown,
>   -- so there should be no need for bracket here.
>   stmtPrepare sess stmt str
>   rc <- execute sess stmt 1
>   closeStmt sess stmt
>   return rc


> instance Command String Session where
>   executeCommand sess str = executeCommand sess (sql str)

> data CommandBind = CommandBind String [BindA Session PreparedStmtObj BindObj]

> cmdbind :: String -> [BindA Session PreparedStmtObj BindObj] -> CommandBind
> cmdbind sql parms = CommandBind sql parms

> instance Command CommandBind Session where
>   executeCommand sess (CommandBind sqltext bas) = do
>     let (PreparationA pa) = prepareStmt' 0 sqltext FreeWithQuery CommandType
>     ps <- pa sess
>     bindRun sess ps bas (\(BoundStmt bs) -> getRowCount sess (stmtHandle bs))

> instance Command BoundStmt Session where
>   executeCommand s (BoundStmt pstmt) =
>     getRowCount s (stmtHandle pstmt)


> instance ISession Session where
>   disconnect sess = dbDisconnect sess
>   beginTransaction sess isolation = beginTrans sess isolation
>   commit sess = commitTrans sess
>   rollback sess = rollbackTrans sess


We need to keep track of the scope of the PreparedStmtObj
i.e. should it be freed when the Query (result-set) is freed,
or does it have a longer lifetime?
PreparedStmtObjs created by prepareStmt have a lifetime possibly
longer than the result-set; users should use withPreparedStatement
to manage these.

PreparedStmtObjs can also be created internally by various instances
of makeQuery (in class Statement), and these will usually have the
same lifetime/scope as that of the Query (result-set).

> data StmtLifetime = FreeWithQuery | FreeManually

We also need to note if the statement is a query (select)
or some sort of command. This influences subsequent behaviour in two ways:
  1. when execute is done, we specify either 0 or 1 iterations,
     for selects or or commands, respectively (see bindRun)
  2. when fetchRow is called by doQuery, for selects we call OCI stmtFetch,
     but for commands we ignore the call (do nothing), because the
     output buffers will already have been filled.

> data StmtType = SelectType | CommandType

> data PreparedStmtObj = PreparedStmtObj
>       { stmtLifetime :: StmtLifetime
>       , stmtType :: StmtType
>       , stmtHandle :: StmtHandle
>       , stmtSession :: Session
>       , stmtCursors :: IORef [RefCursor StmtHandle]
>       -- stmtBuffers are actually output bind buffers.
>       -- we package them to look like ColumnBuffers
>       -- (as created by allocBuffer) so that we can use them
>       -- like ColumnBuffers.
>       , stmtBuffers :: IORef [ColumnBuffer]
>       }


> beginsWithSelect "" = False
> beginsWithSelect text = isPrefixOf "select" . map toLower $ text
> inferStmtType text = if beginsWithSelect text then SelectType else CommandType

> prepareQuery :: QueryString -> PreparationA Session PreparedStmtObj
> prepareQuery (QueryString sqltext) =
>   prepareStmt' (prefetchRowCount defaultResourceUsage) sqltext FreeManually SelectType

> prepareLargeQuery :: Int -> QueryString -> PreparationA Session PreparedStmtObj
> prepareLargeQuery count (QueryString sqltext) =
>   prepareStmt' count sqltext FreeManually SelectType

> prepareCommand :: QueryString -> PreparationA Session PreparedStmtObj
> prepareCommand (QueryString sqltext) =
>   prepareStmt' 0 sqltext FreeManually CommandType

| Seems like an odd alternative to 'prepareCommand' (what is a large command?)
but is actually useful for when the outer query it a procedure call that
returns one or more cursors. The prefetch count for the inner cursors is
inherited from the outer statement, which in this case is a command, rather
than a select. Normally prefetch would be irrelevant (and indeed it is for
the outer command), but we also save it in the statement so that it can be
reused for the child cursors.

> prepareLargeCommand :: Int -> QueryString -> PreparationA Session PreparedStmtObj
> prepareLargeCommand n (QueryString sqltext) =
>   prepareStmt' n sqltext FreeManually CommandType

> prepareStmt' count sqltext lifetime stmtype =
>   PreparationA (\sess -> do
>     stmt <- getStmt sess
>     stmtPrepare sess stmt (OCI.substituteBindPlaceHolders sqltext)
>     setPrefetchCount sess stmt count
>     newPreparedStmt lifetime stmtype sess stmt
>     )

> newPreparedStmt lifetime iteration sess stmt = do
>   c <- newIORef []
>   b <- newIORef []

>   return (PreparedStmtObj lifetime iteration stmt sess c b)

--------------------------------------------------------------------
-- ** Binding
--------------------------------------------------------------------

> newtype BoundStmt = BoundStmt { boundStmt :: PreparedStmtObj }

> type BindObj = Int -> IO ()
> newtype Out a = Out a

> instance IPrepared PreparedStmtObj Session BoundStmt BindObj where
>   bindRun sess stmt bas action = do
>     sequence_ (zipWith (\i (BindA ba) -> ba sess stmt i) [1..] bas)
>     let iteration = case (stmtType stmt) of
>           SelectType -> 0
>           CommandType -> 1
>     execute sess (stmtHandle stmt) iteration
>     writeIORef (stmtCursors stmt) []
>     action (BoundStmt stmt)
>   destroyStmt sess pstmt = closeStmt sess (stmtHandle pstmt)

> instance DBBind (Maybe String) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Out (Maybe String)) Session PreparedStmtObj BindObj where
>   bindP (Out v) = makeOutputBindAction v

> instance DBBind (Maybe Int) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Out (Maybe Int)) Session PreparedStmtObj BindObj where
>   bindP (Out v) = makeOutputBindAction v

I don't think Oracle supports int64 in v8i's OCI API...

 instance DBBind (Maybe Int64) Session PreparedStmtObj BindObj where
   bindP = makeBindAction

> instance DBBind (Maybe Double) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Out (Maybe Double)) Session PreparedStmtObj BindObj where
>   bindP (Out v) = makeOutputBindAction v

> instance DBBind (Maybe CalendarTime) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe UTCTime) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Out (Maybe UTCTime)) Session PreparedStmtObj BindObj where
>   bindP (Out v) = makeOutputBindAction v

StmtHandles (i.e. RefCursors) are output only, I think
(altough you have to pass a valid one in, or it'll hurl).
We create the StmtHandle here, so the user doesn't have to
(is this a bad idea?...)

> instance DBBind (Out (Maybe StmtHandle)) Session PreparedStmtObj BindObj where
>   bindP (Out v) = BindA (\sess stmt pos -> do
>       stmt2 <- getStmt sess
>       bindOutputMaybe sess stmt (Just stmt2) pos
>     )



Instances for non-Maybe types i.e. bare Int, Double, String, etc.

> instance DBBind (Maybe a) Session PreparedStmtObj BindObj
>     => DBBind a Session PreparedStmtObj BindObj where
>   bindP x = bindP (Just x)

> instance DBBind (Out (Maybe a)) Session PreparedStmtObj BindObj
>     => DBBind (Out a) Session PreparedStmtObj BindObj where
>   bindP (Out x) = bindP (Out (Just x))

Default instances, using generic Show.

> instance (Show a) => DBBind (Maybe a) Session PreparedStmtObj BindObj where
>   bindP (Just x) = bindP (Just (show x))
>   bindP Nothing = bindP (Nothing `asTypeOf` Just "")

> instance (Show a) => DBBind (Out (Maybe a)) Session PreparedStmtObj BindObj where
>   bindP (Out (Just x)) = bindP (Out (Just (show x)))
>   bindP (Out Nothing) = bindP (Out (Nothing `asTypeOf` Just ""))


> makeBindAction x = BindA (\ses st -> bindMaybe ses st x)

> bindMaybe :: (OracleBind a)
>   => Session -> PreparedStmtObj -> Maybe a -> Int -> IO ()
> bindMaybe sess stmt v pos =
>   bindWithValue v $ \ptrv -> do
>     bindByPos sess stmt pos (bindNullInd v) (castPtr ptrv) (bindDataSize v) (bindType v)

Note currying... we don't provide the column-position;
it's provided when bindRun is invoked.

> makeOutputBindAction v = BindA (\sess stmt -> bindOutputMaybe sess stmt v)

> bindOutputMaybe :: (OracleBind a)
>   => Session -> PreparedStmtObj -> Maybe a -> Int -> IO ()
> bindOutputMaybe sess stmt v pos = do
>       buffer <- mallocForeignPtrBytes (bindBufferSize v)
>       nullind <- mallocForeignPtr
>       sizeind <- mallocForeignPtr
>       withForeignPtr buffer $ \bufptr -> do
>       withForeignPtr nullind $ \indptr -> do
>       withForeignPtr sizeind $ \szeptr -> do
>         poke (castPtr indptr) (bindNullInd v)
>         poke (castPtr szeptr) (bindDataSize v)  -- input size
>         bindWriteBuffer (castPtr bufptr) v
>       bindOutputByPos sess stmt pos (nullind, buffer, sizeind) (bindBufferSize v) (bindType v)
>       let
>         colbuf = ColumnBuffer
>           { colBufBufferFPtr = buffer
>           , colBufNullFPtr = nullind
>           , colBufSizeFPtr = sizeind
>           , colBufColPos = 0
>           , colBufSqlType = (bindType v)
>           }
>       appendOutputBindBuffer stmt colbuf

If the bind values are output, then we save them in a list.
We can then use a doQuery to fetch the values,
just like the Postgres technique of returning values
as a set of tuples.
I guess there'll only ever be a single row to fetch
in the Oracle case, though.

> appendOutputBindBuffer stmt buffer = do
>   buffers <- readIORef (stmtBuffers stmt)
>   modifyIORef (stmtBuffers stmt) (++ [buffer { colBufColPos = 1 + length buffers }])




> class OracleBind a where
>   bindWithValue :: a -> (Ptr Word8 -> IO ()) -> IO ()
>   bindWriteBuffer :: Ptr Word8 -> a -> IO ()
>   bindDataSize :: a -> Int
>   bindBufferSize :: a -> Int
>   bindBufferSize v = bindDataSize v
>   bindNullInd :: a -> CShort
>   -- non-Maybe types aren't nullable, so null-ind is always 0
>   -- for values in these types.
>   bindNullInd _ = 0
>   bindType :: a -> CInt

> instance OracleBind a => OracleBind (Maybe a) where
>   bindWithValue (Just v) a = bindWithValue v a
>   bindWithValue Nothing a = return ()
>   bindWriteBuffer b (Just v) = bindWriteBuffer b v
>   bindWriteBuffer b Nothing = return ()
>   bindDataSize (Just v) = bindDataSize v
>   bindDataSize Nothing = 0
>   bindBufferSize (Just v) = bindBufferSize v
>   bindBufferSize x@Nothing =
>     let (Just v) = (Just undefined) `asTypeOf` x in bindBufferSize v
>   bindNullInd (Just v) = 0
>   bindNullInd Nothing = -1
>   bindType (Just v) = bindType v
>   bindType Nothing = bindType (undefined :: a)

> instance OracleBind String where
>   -- FIXME  should these be withUTF8String{Len} ?
>   bindWithValue v a = withCString v (\p -> a (castPtr p))
>   bindWriteBuffer b s = withCStringLen s (\(p,l) -> 
>     copyBytes (castPtr b) p (1+l))
>   bindDataSize s = fromIntegral (length s)
>   bindBufferSize _ = 32000
>   bindType _ = oci_SQLT_CHR

> instance OracleBind Int where
>   bindWithValue v a = withBinaryValue toCInt v (\p v -> poke (castPtr p) v) a
>   bindWriteBuffer b v = poke (castPtr b) v
>   bindDataSize _ = (sizeOf (toCInt 0))
>   bindType _ = oci_SQLT_INT

> instance OracleBind Double where
>   bindWithValue v a = withBinaryValue toCDouble v (\p v -> poke (castPtr p) v) a
>   bindWriteBuffer b v = poke (castPtr b) v
>   bindDataSize _ = (sizeOf (toCDouble 0.0))
>   bindType _ = oci_SQLT_FLT

> instance OracleBind CalendarTime where
>   bindWithValue v a = withBinaryValue id v (\p dt -> calTimeToBuffer (castPtr p) dt) a
>   bindWriteBuffer b v = calTimeToBuffer (castPtr b) v
>   bindDataSize _ = 7
>   bindType _ = oci_SQLT_DAT

> instance OracleBind UTCTime where
>   bindWithValue v a = withBinaryValue id v (\p dt -> utcTimeToBuffer (castPtr p) dt) a
>   bindWriteBuffer b v = utcTimeToBuffer (castPtr b) v
>   bindDataSize _ = 7
>   bindType _ = oci_SQLT_DAT

> instance OracleBind StmtHandle where
>   bindWithValue v a = alloca (\p -> poke p v >> a (castPtr p))
>   bindWriteBuffer b v = poke (castPtr b) v
>   bindDataSize _ = sizeOf nullPtr
>   bindType _ = oci_SQLT_RSET

> withBinaryValue :: (OracleBind b) =>
>   (b -> a)  -- ^ convert Haskell value to C value
>   -> b      -- ^ value to convert (we call bindSize on this value to get buffer size)
>   -> (Ptr Word8 -> a -> IO ())  -- ^ action to place converted value into buffer
>   -> (Ptr Word8 -> IO ())       -- ^ action to run over buffer; buffer will be freed on completion
>   -> IO ()
> withBinaryValue fn v pok action =
>   -- FIXME  is bindBufferSize better here?
>   --allocaBytes (bindBufferSize v) $ \p -> do
>   allocaBytes (bindDataSize v) $ \p -> do
>   pok p (fn v)
>   action (castPtr p)

> clength = fromIntegral . length

> toCInt :: Int -> CInt; toCInt = fromIntegral
> fromCInt :: CInt -> Int; fromCInt = fromIntegral
> toCChar :: Char -> CChar; toCChar = toEnum . fromEnum
> fromCChar :: CChar -> Char; fromCChar = toEnum . fromEnum
> toCDouble :: Double -> CDouble; toCDouble = realToFrac
> fromCDouble :: CDouble -> Double; fromCDouble = realToFrac
> toCFloat :: Float -> CFloat; toCFloat = realToFrac
> fromCFloat :: CFloat -> Float; fromCFloat = realToFrac


--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

We save a reference to the parent PreparedStmtObj.
In a lot of cases (the simple ones) the parent is that same
as the PreparedStmtObj.
It only differs when we use the NextResultSet instance of makeQuery.
It is only Nothing when we are processing a RefCursor;
in this case we don't want to save any nested cursors returned
by the query.

> data Query = Query
>   { queryStmt :: PreparedStmtObj
>   , querySess :: Session
>   , queryParent :: Maybe PreparedStmtObj
>   }


> data QueryResourceUsage = QueryResourceUsage { prefetchRowCount :: Int }

> defaultResourceUsage :: QueryResourceUsage
> defaultResourceUsage = QueryResourceUsage 100  -- sensible default?

> data QueryStringTuned = QueryStringTuned QueryResourceUsage String [BindA Session PreparedStmtObj BindObj]

> sqlbind :: String -> [BindA Session PreparedStmtObj BindObj] -> QueryStringTuned
> sqlbind sql parms = QueryStringTuned defaultResourceUsage sql parms

> prefetch :: Int -> String -> [BindA Session PreparedStmtObj BindObj] -> QueryStringTuned
> prefetch count sql parms = QueryStringTuned (QueryResourceUsage count) sql parms

> instance Statement BoundStmt Session Query where
>   makeQuery sess bstmt = return (Query (boundStmt bstmt) sess (Just (boundStmt bstmt)))

> instance Statement PreparedStmtObj Session Query where
>   makeQuery sess pstmt = return (Query pstmt sess (Just pstmt))

> instance Statement QueryString Session Query where
>   makeQuery sess (QueryString sqltext) = makeQuery sess sqltext

> instance Statement String Session Query where
>   makeQuery sess sqltext = makeQuery sess (QueryStringTuned defaultResourceUsage sqltext [])

Important to use FreeManually here...
If we destroy the StmtHandle when the query is done,
it does not allow us to re-use the StmtHandle,
which is vital for nested cursors
(mainly because I haven't figured out how to
marshal StmtHandles back to Haskell-land).
However, this means we have to process StmtHandles
immediately; we can't save them up and process them later,
after the parent query is done.

Contrast this with the Postgres back-end, where the RefCursor
just contains a String, which is the cursor name.
It's no trouble to marshal a String back to Haskell-land.

> instance Statement (RefCursor StmtHandle) Session Query where
>   makeQuery sess (RefCursor stmt) = do
>     pstmt <- newPreparedStmt FreeManually SelectType sess stmt
>     return (Query pstmt sess Nothing)

For NextResultSet, we call makeQuery passing (RefCursor StmtHandle).
This creates a query with no parent statement.
All other instances of Statement make a statement its own parent.

> instance Statement (NextResultSet mark PreparedStmtObj) Session Query where
>   makeQuery sess (NextResultSet (PreparedStmt pstmt)) = do
>     cursors <- readIORef (stmtCursors pstmt)
>     if null cursors then throwDB (DBError ("02", "000") (-1) "No more result sets to process.") else return ()
>     writeIORef (stmtCursors pstmt) (tail cursors)
>     makeQuery sess (head cursors)

> instance Statement QueryStringTuned Session Query where
>   makeQuery sess (QueryStringTuned resUsage sqltext bas) = do
>     let
>      (PreparationA action) =
>         prepareStmt' (prefetchRowCount resUsage) sqltext FreeWithQuery SelectType
>     pstmt <- action sess
>     sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>     execute sess (stmtHandle pstmt) 0
>     return (Query pstmt sess (Just pstmt))

> instance Statement CommandBind Session Query where
>   makeQuery sess (CommandBind sqltext bas) = do
>     let
>      (PreparationA action) =
>         prepareStmt' 1 sqltext FreeWithQuery CommandType
>     pstmt <- action sess
>     sequence_ (zipWith (\i (BindA ba) -> ba sess pstmt i) [1..] bas)
>     execute sess (stmtHandle pstmt) 1
>     return (Query pstmt sess (Just pstmt))


> data ColumnBuffer = ColumnBuffer 
>    { colBufBufferFPtr :: OCI.ColumnResultBuffer
>    , colBufNullFPtr :: ForeignPtr CShort
>    , colBufSizeFPtr :: ForeignPtr CUShort
>    , colBufColPos :: Int
>    , colBufSqlType :: CInt
>    }

> instance IQuery Query Session ColumnBuffer where
>   destroyQuery query = do
>     let pstmt = queryStmt query
>     case stmtLifetime pstmt of
>       FreeWithQuery -> closeStmt (stmtSession pstmt) (stmtHandle pstmt)
>       _ -> return ()
>   fetchOneRow query = do
>     let pstmt = queryStmt query
>     -- Only call fetchRow if there are no bind output buffers
>     -- If there are bind output buffers then assume this was a
>     -- procedure call.
>     -- In this case you will always get the same row over and over,
>     -- so you'd better be careful with your iteratees
>     buffers <- readIORef (stmtBuffers pstmt)
>     if not (null buffers) then return True
>       else do
>         rc <- fetchRow (querySess query) pstmt
>         return (rc /= oci_NO_DATA)
>   currentRowNum query =
>     getRowCount (querySess query) (stmtHandle (queryStmt query))
>   freeBuffer q buffer = return ()


This is where we differ the behaviour depending on the
type of statement: is it a regular query,
where we just create column buffers, or do we have
bind output buffers, in which case we return those
as the column buffers?

> allocBuffer query (bufsize, ociBufferType) colpos = do
>   buffers <- readIORef (stmtBuffers (queryStmt query))
>   if null buffers
>     then do
>       (_, buf, nullptr, sizeptr) <- liftIO $ defineCol (querySess query) (queryStmt query) colpos bufsize ociBufferType
>       return $ ColumnBuffer
>         { colBufBufferFPtr = buf
>         , colBufNullFPtr = nullptr
>         , colBufSizeFPtr = sizeptr
>         , colBufColPos = colpos
>         , colBufSqlType = ociBufferType
>         }
>     else do
>       if length buffers >= colpos
>         then return (buffers !! (colpos - 1))
>         else
>           throwDB (DBError ("02", "000") (-1) ( "There are " ++ show (length buffers)
>             ++ " output buffers, but you have asked for buffer " ++ show colpos ))

When you allocate a StmtHandle define buffer (as opposed to a bind buffer)
you have to populate it with a valid StmtHandle before you call fetch.
FIXME  when is this freed? When is the StmtHandle destroyed?

> allocStmtBuffer query colpos = do
>   colbuf <- allocBuffer query (sizeOf nullPtr, oci_SQLT_RSET) colpos
>   buffers <- readIORef (stmtBuffers (queryStmt query))
>   -- if buffers is null then assume this is a define buffer,
>   -- rather than a bind buffer.
>   -- Bind buffers are stuffed into the stmtBuffers list
>   -- when they are created.
>   if null buffers
>     then do
>       -- If this is a define buffer (as opposed to bind buffer)
>       -- then shove a valid StmtHandle into it.
>       stmt <- getStmt (querySess query)
>       withForeignPtr (colBufBufferFPtr colbuf) $ \p -> poke (castPtr p) stmt
>     else return ()
>   return colbuf


> bufferToString :: ColumnBuffer -> IO (Maybe String)
> bufferToString buffer = OCI.bufferToString (undefined, colBufBufferFPtr buffer, colBufNullFPtr buffer, colBufSizeFPtr buffer)

> bufferToCaltime :: ColumnBuffer -> IO (Maybe CalendarTime)
> bufferToCaltime buffer = OCI.bufferToCaltime (colBufNullFPtr buffer) (colBufBufferFPtr buffer)

> bufferToUTCTime :: ColumnBuffer -> IO (Maybe UTCTime)
> bufferToUTCTime buffer = OCI.bufferToUTCTime (colBufNullFPtr buffer) (colBufBufferFPtr buffer)

> calTimeToBuffer :: OCI.BufferPtr -> CalendarTime -> IO ()
> calTimeToBuffer buf ct = OCI.calTimeToBuffer buf ct

> utcTimeToBuffer :: OCI.BufferPtr -> UTCTime -> IO ()
> utcTimeToBuffer buf utc = OCI.utcTimeToBuffer buf utc

> bufferToInt :: ColumnBuffer -> IO (Maybe Int)
> bufferToInt buffer = OCI.bufferToInt (colBufNullFPtr buffer) (colBufBufferFPtr buffer)

> bufferToDouble :: ColumnBuffer -> IO (Maybe Double)
> bufferToDouble buffer = OCI.bufferToDouble (colBufNullFPtr buffer) (colBufBufferFPtr buffer)

> bufferToStmtHandle :: ColumnBuffer -> IO (RefCursor StmtHandle)
> bufferToStmtHandle buffer = do
>   v <- OCI.bufferToStmtHandle (colBufBufferFPtr buffer)
>   return (RefCursor v)


Right now the StmtHandle in the buffer is updated with a new
cursor on each fetch.
If we call defineByPos before each fetch then we can provide
a fresh StmtHandle for each fetch (thus preserving the handles
from previous fetches), but this will require support from
Database.Enumerator i.e. we need to add a function, say reallocBuffers,
which is called before each row is fetched,
which will give us an opportunity to reallocate memory for buffers
if it is required.
This might be a bad idea though, because it is likely to lead
to space leaks. Perhaps the current approach of reusing the
StmtHandle is better from a memory management point-of-view.
That said, the Oracle DBMS has a fairly low limit (on the order
of 100 or so) on the number of open cursors, so perhaps space
leaks aren't as likely as I think.

> instance DBType (RefCursor StmtHandle) Query ColumnBuffer where
>   allocBufferFor _ q n = allocStmtBuffer q n
>   fetchCol q buffer = do
>     rawstmt <- OCI.bufferToStmtHandle (colBufBufferFPtr buffer)
>     appendRefCursor q (RefCursor rawstmt)

> appendRefCursor query refc = do
>   case queryParent query of
>     -- no parent stmt? => probably just a doQuery over a RefCursor.
>     -- Don't bother saving returned RefCursors.
>     Nothing -> return ()
>     Just pstmt -> modifyIORef (stmtCursors pstmt) (++ [refc])
>   return refc


> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (16000, oci_SQLT_CHR) n
>   fetchCol q buffer = bufferToString buffer

> instance DBType (Maybe Int) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (4, oci_SQLT_INT) n
>   fetchCol q buffer = bufferToInt buffer

> instance DBType (Maybe Double) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (8, oci_SQLT_FLT) n
>   fetchCol q buffer = bufferToDouble buffer

> instance DBType (Maybe UTCTime) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (7, oci_SQLT_DAT) n
>   fetchCol q buffer = bufferToUTCTime buffer

> instance DBType (Maybe CalendarTime) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (7, oci_SQLT_DAT) n
>   fetchCol q buffer = bufferToCaltime buffer

|This single polymorphic instance covers all of the
type-specific non-Maybe instances e.g. String, Int, Double, etc.

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor v q n = allocBufferFor (Just v) q n
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) (fetchCol q buffer)

> buffer_pos q buffer = do
>   row <- currentRowNum q
>   return (row, colBufColPos buffer)


|A polymorphic instance which assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q (16000, oci_SQLT_CHR) n
>   fetchCol q buffer = do
>     v <- bufferToString buffer
>     case v of
>       Just s -> if s == "" then return Nothing else return (Just (read s))
>       Nothing -> return Nothing


|
Module      :  Database.PostgreSQL.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

PostgreSQL implementation of Database.Enumerator.


> {-# OPTIONS -fglasgow-exts #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE UndecidableInstances #-}

> module Database.PostgreSQL.Enumerator
>   ( Session, connect, ConnectAttr(..)
>   , prepareStmt, preparePrefetch
>   , prepareQuery, prepareLargeQuery, prepareCommand
>   , sql, sqlbind, prefetch, cmdbind
>   , bindType, DBAPI.byteaEsc, DBAPI.byteaUnesc
>   , DBAPI.UUID, DBAPI.string2uuid, DBAPI.uuid2string
>   , module Database.Enumerator
>   )
> where


> import Database.Enumerator
> import Database.InternalEnumerator
> import Foreign.C
> import Control.Monad
> import qualified Database.PostgreSQL.PGFunctions as DBAPI
> import Data.Char
> import Data.Dynamic
> import Data.IORef
> import Data.Int
> import Data.List
> import Data.Time
> import Data.Word
> import System.Time

> {-# DEPRECATED prepareStmt "Use prepareQuery or prepareCommand instead" #-}
> {-# DEPRECATED preparePrefetch "Use prepareLargeQuery instead" #-}

--------------------------------------------------------------------
-- ** API Wrappers
--------------------------------------------------------------------

|These wrappers ensure that only DBExceptions are thrown,
and never PGExceptions.
We don't need wrappers for the colValXxx functions
because they never throw exceptions.


> convertAndRethrow :: DBAPI.PGException -> IO a
> convertAndRethrow (DBAPI.PGException e m) = do
>   let
>     sqlstate@(ssc,sssc) = errorSqlState m
>     errorConstructor = case ssc of
>       "XX" -> DBFatal
>       "58" -> DBFatal
>       "57" -> DBFatal
>       "54" -> DBFatal
>       "53" -> DBFatal
>       "08" -> DBFatal
>       _ -> DBError
>   throwDB (errorConstructor sqlstate e m)

Postgres error messages (in Verbose mode) start with
ERROR:  42P01: relation "blahblahblah" does not exist.

> errorSqlState :: String -> (String, String)
> errorSqlState msg = let s = dropPrefix "ERROR:  " msg in (take 2 s, take 3 (drop 2 s))

> dropPrefix p s = if isPrefixOf p s then drop (length p) s else s

|Common case: wrap an action with a convertAndRethrow.

> convertEx :: IO a -> IO a
> convertEx action = DBAPI.catchPG action convertAndRethrow


--------------------------------------------------------------------
-- ** Sessions
--------------------------------------------------------------------

We don't need much in an PostgreSQL Session record.
Session objects are created by 'connect'.
Deriving Typeable allows us to store Session objects in a
HashMap (say) of Dynamic objects e.g. for a connection pool.

> newtype Session = Session { dbHandle :: DBAPI.DBHandle }
>   deriving Typeable

| Specify connection options to 'connect'.
You only need to use whatever subset is relevant for your connection.

> data ConnectAttr = 
>    CAhost String
>  | CAhostaddr String
>  | CAport String
>  | CAdbname String
>  | CAuser String
>  | CApassword String
>  | CAconnect_timeout Int
>  | CAoptions String
>  | CAsslmode String
>  | CAservice String
>           
> connect :: [ConnectAttr] -> ConnectA Session
> connect attrs = ConnectA $ convertEx $ do
>   db <- DBAPI.openDb (unwords $ map encode attrs)
>   DBAPI.disableNoticeReporting db
>   -- Use Verbose to return the SqlState in the error message.
>   -- There doesn't seem to be a libpq API function to get it...
>   DBAPI.setErrorVerbosity db DBAPI.ePQERRORS_VERBOSE
>   --DBAPI.setErrorVerbosity db DBAPI.ePQERRORS_DEFAULT
>   --DBAPI.setErrorVerbosity db DBAPI.ePQERRORS_TERSE
>   return (Session db)
>  where 
>   -- process attributes into a string name=value
>   encode (CAhost s)            = "host=" ++ enc s
>   encode (CAhostaddr s)        = "hostaddr=" ++ enc s
>   encode (CAport s)            = "port=" ++ enc s
>   encode (CAdbname s)          = "dbname=" ++ enc s
>   encode (CAuser s)            = "user=" ++ enc s
>   encode (CApassword s)        = "password=" ++ enc s
>   encode (CAconnect_timeout i) = "connect_timeout=" ++ show i
>   encode (CAoptions s)         = "options=" ++ enc s
>   encode (CAsslmode s)         = "sslmode=" ++ enc s
>   encode (CAservice s)         = "service=" ++ enc s
>   enc s = "'" ++ qu  s ++ "'"
>   qu s = case break ( \c -> c == '\'' || c == '"' ) s of
>           (s,"") -> s
>           (s,(c:t))  -> s ++ ('\\' : c : qu t)


> isolationLevelText ReadUncommitted = "read uncommitted"
> isolationLevelText ReadCommitted = "read committed"
> isolationLevelText RepeatableRead = "repeatable read"
> isolationLevelText Serialisable = "serializable"
> isolationLevelText Serializable = "serializable" 


> instance ISession Session where
>   disconnect sess = convertEx $ DBAPI.closeDb (dbHandle sess)
>   beginTransaction sess isolation = do
>     executeCommand sess (sql ("begin isolation level " ++ isolationLevelText isolation)) >> return ()
>   commit sess   = executeCommand sess (sql "commit") >> return ()
>   rollback sess = executeCommand sess (sql "rollback") >> return ()


--------------------------------------------------------------------
-- Statements and Commands
--------------------------------------------------------------------


> newtype QueryString = QueryString String

|The simplest kind of a statement: no tuning parameters,
all default, little overhead.

> sql :: String -> QueryString
> sql str = QueryString str

> instance Command QueryString Session where
>   executeCommand s (QueryString str) = executeCommand s str

> instance Command String Session where
>   executeCommand s str = do
>     (_,ntuple_str,_) <- convertEx $ DBAPI.nqExec (dbHandle s) str
>     return $ if ntuple_str == "" then 0 else read ntuple_str

> instance Command BoundStmt Session where
>   executeCommand s bs = return (boundCount bs)

> data CommandBind = CommandBind String [BindA Session PreparedStmtObj BindObj]

> cmdbind :: String -> [BindA Session PreparedStmtObj BindObj] -> CommandBind
> cmdbind sql parms = CommandBind sql parms

> instance Command CommandBind Session where
>   executeCommand sess (CommandBind sqltext bas) = do
>     -- tricky - can't prepare statement without list of bind types,
>     -- but to construct list of bind types we need to evaluate the bind
>     -- actions and then get the bind-type out of the resulting PGBindVal object.
>     -- The bind action normally requires a valid session and stmt,
>     -- so we have a chicken-and-egg problem.
>     -- Good thing the sess and stmt that we pass to the bind action are
>     -- not used (see makeBindAction below), so we can pass undefined.
>     let params = map (\(BindA ba) -> ba sess undefined) bas
>     let bindtypes = DBAPI.bindTypes params
>     let (PreparationA pa) = prepareCommand "" (QueryString sqltext) bindtypes
>     pstmt <- pa sess
>     writeIORef (stmtCursors pstmt) []
>     (_, countstr, _) <- convertEx $ DBAPI.execPreparedCommand (dbHandle sess) (stmtName pstmt) params
>     return (read countstr)


|At present the only resource tuning we support is the number of rows
prefetched by the FFI library.
We use a record to (hopefully) make it easy to add other 
tuning parameters later.

> data QueryResourceUsage = QueryResourceUsage { prefetchRowCount :: Int }

> defaultResourceUsage :: QueryResourceUsage
> defaultResourceUsage = QueryResourceUsage 0  -- get it all at once


> data StmtType = SelectType | CommandType

Simple prepared statement: the analogue of QueryString. It is useful
for DDL and DML statements, and for simple queries (that is, queries
that do not need cursors and result in a small enough dataset -- because
it will be fetched entirely in one shot).

The data constructor is not exported.

> data PreparedStmtObj = PreparedStmtObj
>   { stmtName :: String
>   , stmtType :: StmtType
>   , stmtPrefetch :: Int
>   , stmtCursors :: IORef [RefCursor String]
>   }

> beginsWithSelect "" = False
> beginsWithSelect text = isPrefixOf "select" . map toLower $ text
> inferStmtType text = if beginsWithSelect text then SelectType else CommandType

> prepareStmt ::
>   String -> QueryString -> [DBAPI.Oid] -> PreparationA Session PreparedStmtObj
> prepareStmt [] _ _ = error "Prepared statement name must be non-empty"
> prepareStmt name (QueryString str) types = 
>   PreparationA (\sess -> do
>     psname <- convertEx $ DBAPI.stmtPrepare (dbHandle sess) name (DBAPI.substituteBindPlaceHolders str) types
>     c <- newIORef []
>     return (PreparedStmtObj psname (inferStmtType str) 0 c)
>     )

> prepareQuery ::
>   String -> QueryString -> [DBAPI.Oid] -> PreparationA Session PreparedStmtObj
> prepareQuery name (QueryString str) types = 
>   PreparationA (\sess -> do
>     psname <- convertEx $ DBAPI.stmtPrepare (dbHandle sess) name (DBAPI.substituteBindPlaceHolders str) types
>     c <- newIORef []
>     return (PreparedStmtObj psname SelectType 0 c)
>     )

Here we use the same name for both the cursor name and the prepared statement name.
This isn't necessary, but it saves the user having to provide two names...

> preparePrefetch ::
>   Int -> String -> QueryString -> [DBAPI.Oid] -> PreparationA Session PreparedStmtObj
> preparePrefetch count name (QueryString sqltext) types =
>   PreparationA (\sess -> do
>     let q = "DECLARE \"" ++ name ++ "\" NO SCROLL CURSOR FOR " ++ sqltext
>     psname <- convertEx $ DBAPI.stmtPrepare (dbHandle sess) name (DBAPI.substituteBindPlaceHolders q) types
>     c <- newIORef []
>     return (PreparedStmtObj psname CommandType count c)
>     )

> prepareLargeQuery ::
>   Int -> String -> QueryString -> [DBAPI.Oid] -> PreparationA Session PreparedStmtObj
> prepareLargeQuery = preparePrefetch

> prepareCommand ::
>   String -> QueryString -> [DBAPI.Oid] -> PreparationA Session PreparedStmtObj
> prepareCommand name (QueryString sqltext) types =
>   PreparationA (\sess -> do
>     psname <- convertEx $ DBAPI.stmtPrepare (dbHandle sess) name (DBAPI.substituteBindPlaceHolders sqltext) types
>     c <- newIORef []
>     return (PreparedStmtObj psname CommandType 0 c)
>     )


|bindType is useful when constructing the list of Oids for stmtPrepare.
You don't need to pass the actual bind values, just dummy values
of the same type (the value isn't used, so 'Prelude.undefined' is OK here).

> bindType :: DBAPI.PGType a => a -> DBAPI.Oid
> bindType v = DBAPI.pgTypeOid v

We store the parent prepared-statement in BoundStmt.
This is used by the BoundStmt instance of Statement to set up
the parent PreparedStmtObj object in the Query object.

Note two different types of BoundStmt (prefetch and non-prefetch).
The prefetch version requires different behaviour both when
binding-and-running (it doesn't return a result-set, so we have to
execute it as a command), and when processing the result-set
(the non-prefetch version has all of the data available,
whereas the prefetch version must use the advance+cleanup actions).

> data BoundStmt =
>   BoundStmtQuery
>     { boundHandle :: DBAPI.ResultSetHandle
>     , boundCount :: Int
>     , boundParentStmt :: PreparedStmtObj
>     }
>   | BoundStmtCommand
>     { boundParentStmt :: PreparedStmtObj
>     , boundCount :: Int
>     }


The bindRun method returns a BoundStmt,
which contains just the result-set (and row count).

> type BindObj = DBAPI.PGBindVal

> instance IPrepared PreparedStmtObj Session BoundStmt BindObj where
>   bindRun sess stmt bas action = do
>     let params = map (\(BindA ba) -> ba sess stmt) bas
>     writeIORef (stmtCursors stmt) []
>     case stmtType stmt of
>       CommandType -> do
>         (_, countstr, _) <- convertEx $ DBAPI.execPreparedCommand (dbHandle sess) (stmtName stmt) params
>         action (BoundStmtCommand stmt (read countstr))
>       SelectType -> do
>         (rs, count) <- convertEx $ DBAPI.stmtExec (dbHandle sess) (stmtName stmt) params
>         action (BoundStmtQuery rs count stmt)
>   destroyStmt sess stmt = deallocateStmt sess (stmtName stmt)

> deallocateStmt sess name =
>   when (not (null name)) $ do
>     executeCommand sess ("deallocate \"" ++ name ++ "\"")
>     return ()

-- Serialization (binding)

> makeBindAction x = BindA (\_ _ -> DBAPI.newBindVal x)

> instance DBBind (Maybe String) Session PreparedStmtObj BindObj where
>   bindP (Just s) = makeBindAction (Just s)
>   bindP Nothing = makeBindAction (Nothing `asTypeOf` Just "")

> instance DBBind (Maybe UTCTime) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Int) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Bool) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Int64) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Float) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe Double) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe [Word8]) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe DBAPI.UUID) Session PreparedStmtObj BindObj where
>   bindP = makeBindAction

> instance DBBind (Maybe a) Session PreparedStmtObj BindObj
>     => DBBind a Session PreparedStmtObj BindObj where
>   bindP x = bindP (Just x)

The default instance, uses generic Show

> instance (Show a) => DBBind (Maybe a) Session PreparedStmtObj BindObj where
>   bindP (Just x) = bindP (Just (show x))
>   bindP Nothing = bindP (Nothing `asTypeOf` Just "")

--------------------------------------------------------------------
-- ** Queries
--------------------------------------------------------------------

> data SubQuery = SubQuery
>   { stmtHandle :: DBAPI.ResultSetHandle
>   , ntuples  :: Int  -- number of tuples to process in this subquery
>   , curr'row :: Int  -- current row, one-based. Should increment before use
>   }


> data Query = Query
>   { subquery :: IORef SubQuery
>   , advance'action :: Maybe (IO (DBAPI.ResultSetHandle, Int))
>   , cleanup'action :: Maybe (IO ())
>   , querySession :: Session
>   , queryStmt :: Maybe PreparedStmtObj
>   }



The following function creates the Query record. It has a few
decisions to make:
 -- should we prepare a statement or do execImm?
 -- should we use a cursor (better for large queries) or obtain
    all data in one shot?
    The use of cursor means we must be in a transaction.
 -- what is the name of the prepared statement? 
    Potentially, we should be able to execute a previously prepared
    statement...

Currently, if prefetchRowCount is 0 or the query is
not tuned, we use execImm. Otherwise,
we open a cursor and advance it by prefetchRowCount step.
We use anonymous prepared statement name.

The function commence'query also fetches some data (even if it turns
out 0 rows) so that later on, we could determine the type of data in columns
and prepare the buffers accordingly. We don't do that at the moment.


> instance Statement String Session Query where
>   makeQuery sess sqltext = makeQuery sess (QueryString sqltext)

> instance Statement QueryString Session Query where
>   makeQuery sess (QueryString sqltext) = commence'query'simple sess sqltext []


Simple prepared statements.
Query has been executed and result-set is in handle.

> instance Statement BoundStmt Session Query where
>   makeQuery sess bs@(BoundStmtQuery _ _ _) = do
>     sqr <- newIORef $ SubQuery (boundHandle bs) (boundCount bs) 0
>     return (Query sqr Nothing Nothing sess (Just (boundParentStmt bs)))
>   makeQuery sess bs@(BoundStmtCommand _ _) = do
>     let pstmt = boundParentStmt bs
>     let ru = QueryResourceUsage (stmtPrefetch pstmt)
>     -- prefix "ff_" to prepared-stmt/cursor name to create unique statement
>     -- name for fetch-forward statement.
>     commencePreparedQuery sess ru (Just pstmt) (stmtName pstmt) ("ff_" ++ (stmtName pstmt))

Here we need to pop the next cursor name from the head of the list,
and use it to open 

> instance Statement (NextResultSet mark PreparedStmtObj) Session Query where
>   makeQuery sess (NextResultSet (PreparedStmt pstmt)) = do
>     cursors <- readIORef (stmtCursors pstmt)
>     if null cursors then throwDB (DBError ("02", "000") (-1) "No more result sets to process.") else return ()
>     let (RefCursor cursor) = head cursors
>     writeIORef (stmtCursors pstmt) (tail cursors)
>     let ru = QueryResourceUsage (stmtPrefetch pstmt)
>     commencePreparedQuery sess ru (Just pstmt) cursor cursor


> instance Statement (RefCursor String) Session Query where
>   makeQuery sess (RefCursor cursor) = do
>     let ru = QueryResourceUsage 0
>     commencePreparedQuery sess ru Nothing cursor cursor


Statements with resource usage

> data QueryStringTuned = QueryStringTuned QueryResourceUsage String [BindA Session PreparedStmtObj BindObj]

> sqlbind :: String -> [BindA Session PreparedStmtObj BindObj] -> QueryStringTuned
> sqlbind sql parms = QueryStringTuned defaultResourceUsage sql parms

> prefetch :: Int -> String -> [BindA Session PreparedStmtObj BindObj] -> QueryStringTuned
> prefetch count sql parms = QueryStringTuned (QueryResourceUsage count) sql parms

> instance Statement QueryStringTuned Session Query where
>   makeQuery sess (QueryStringTuned resource_usage sqltext bas) = do
>     let params = map (\(BindA ba) -> ba sess undefined) bas
>     commence'query sess resource_usage sqltext params



> commence'query'simple sess sqltext params = do
>   subq <- create'subq sess $
>     convertEx $ DBAPI.stmtExecImm (dbHandle sess) (DBAPI.substituteBindPlaceHolders sqltext) params
>   sqr <- newIORef subq
>   return (Query sqr Nothing Nothing sess Nothing)

Now, prepare and open the cursor

> commence'query sess resourceUsage sqltext params
>     | QueryResourceUsage{prefetchRowCount = 0} <- resourceUsage
>              = commence'query'simple sess sqltext params

> commence'query sess resourceUsage sqltext params = do
>   let prepared'statement'name = "" -- meaning anonymous
>   let default'cursor'name = "takusenp"
>   let q = "DECLARE \"" ++ default'cursor'name ++ "\" NO SCROLL CURSOR FOR " ++ sqltext
>   psname <- convertEx $ DBAPI.stmtPrepare (dbHandle sess) prepared'statement'name
>     (DBAPI.substituteBindPlaceHolders q) (DBAPI.bindTypes params)
>   convertEx $ DBAPI.execPreparedCommand (dbHandle sess) psname params
>   commencePreparedQuery sess resourceUsage Nothing default'cursor'name default'cursor'name



commencePreparedQuery assumes that the DECLARE CURSOR statement
has been prepared and executed.
Note there are two statement names (and therefore two statements) in scope here.
The first is the name we give to the DECLARE CURSOR statement;
currently this is just "" (passed in by commence'query).
The second is the name given to the FETCH FORWARD statement, so that it can
be re-executed quickly and easily.
This is also passed in by commence'query, but if you look at commence'query,
you'll see that it is the same as the cursor name (currently "takusenp").

> commencePreparedQuery sess resourceUsage parentStmt cursorName prefetchStmtName = do
>   let countStr = if (prefetchRowCount resourceUsage) <= 0 then "ALL" else (show $ prefetchRowCount resourceUsage)
>   let fetchq = "FETCH FORWARD " ++ countStr ++ " FROM \"" ++ cursorName ++ "\""
>   sn <- convertEx $ DBAPI.stmtPrepare (dbHandle sess) prefetchStmtName fetchq []
>   let
>     advanceA = convertEx (DBAPI.stmtExec0t (dbHandle sess) sn)
>     cleanupA = do
>       executeCommand sess ("CLOSE \"" ++ cursorName ++ "\"")
>       deallocateStmt sess prefetchStmtName
>   subq <- create'subq sess advanceA
>   sqr <- newIORef subq
>   return (Query sqr (Just advanceA) (Just cleanupA) sess parentStmt)

> create'subq sess action = do
>     (stmt,ntuples) <- action
>     return $ SubQuery stmt ntuples 0
> destroy'subq sess subq = convertEx $ DBAPI.stmtFinalise (stmtHandle subq)


> appendRefCursor query cname = do
>   case queryStmt query of
>     -- no parent stmt => probably just a doQuery over a RefCursor.
>     -- Don't bother saving returned RefCursors.
>     Nothing -> return ()
>     Just pstmt -> modifyIORef (stmtCursors pstmt) (++ [cname])

> instance IQuery Query Session ColumnBuffer where
>
>   destroyQuery query = do
>     subq <- readIORef (subquery query)
>     destroy'subq (querySession query) subq
>     maybe (return ()) id (cleanup'action query)
>
>   fetchOneRow query = do
>     let sess = querySession query
>     subq'  <- readIORef (subquery query)
>     let subq = subq' { curr'row = succ (curr'row subq') }
>     if ntuples subq == 0
>        then return False
>        else if curr'row subq > ntuples subq 
>        then maybe
>              (return False)                -- no advance action: we're done
>              (\action -> destroy'subq sess subq   >>
>                          create'subq sess action >>=
>                          writeIORef (subquery query) >>
>                          fetchOneRow query
>              )
>              (advance'action query)
>        else writeIORef (subquery query) subq >> return True
>
>   -- want to add a counter, so we can support this properly
>   currentRowNum q = readIORef (subquery q) >>= return . curr'row
>
>   freeBuffer q buffer = return ()

--------------------------------------------------------------------
-- result-set data buffers implementation
--------------------------------------------------------------------

|There aren't really Buffers to speak of with PostgreSQL,
so we just record the position of each column.

> data ColumnBuffer = ColumnBuffer { colPos :: Int }

> buffer_pos q buffer = do 
>   row <- currentRowNum q
>   return (row, colPos buffer)

An auxiliary function: buffer allocation

> allocBuffer q colpos = return $ ColumnBuffer { colPos = colpos }

> bufferToAny fn query buffer = do
>   subq <- readIORef (subquery query)
>   ind <- DBAPI.colValNull (stmtHandle subq) (curr'row subq) (colPos buffer)
>   if ind then return Nothing
>     else 
>       fn (stmtHandle subq) (curr'row subq) (colPos buffer)
>         >>= return . Just

> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValString

> instance DBType (RefCursor String) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol query buffer = do
>     (Just v) <- bufferToAny DBAPI.colValString query buffer
>     appendRefCursor query (RefCursor v)
>     return (RefCursor v)

> instance DBType (Maybe UTCTime) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValUTCTime

> instance DBType (Maybe CalendarTime) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValCalTime

> instance DBType (Maybe Bool) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValBool

> instance DBType (Maybe Int) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValInt

> instance DBType (Maybe Double) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValDouble

> instance DBType (Maybe Float) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValFloat

> instance DBType (Maybe Int64) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValInt64

> instance DBType (Maybe [Word8]) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValBytea

> instance DBType (Maybe DBAPI.UUID) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q n
>   fetchCol = bufferToAny DBAPI.colValUUID

|This single polymorphic instance replaces all of the type-specific non-Maybe instances
e.g. String, Int, Double, etc.

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) $ fetchCol q buffer


|This polymorphic instance assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor _  = allocBufferFor (undefined::String)
>   fetchCol q buffer = do
>     v <- bufferToAny DBAPI.colValString q buffer
>     case v of
>       Just s -> if s == "" then return Nothing else return (Just (read s))
>       Nothing -> return Nothing


|
Module      :  Database.Stub.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Stub implementation of Database.Enumerator.
Useful for people who can't or won't install a DBMS,
so that they can try out the Enumerator interface.

Currently last last row of any fetch will have a null in its Int columns
(this makes it easier to test handling of nulls and DBUnexpectedNull).
See fetchIntVal.


> {-# OPTIONS -fglasgow-exts #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE IncoherentInstances #-}

> module Database.Stub.Enumerator
>   -- Only the type constructor of Session is exported
>   -- (so the end user could write type signatures). 
>   (  Session, ConnParm(..), connect, sql, prefetch
>    , QueryResourceUsage(..)
>   )
> where


> import Database.InternalEnumerator
> import Foreign
> import Foreign.C
> import Foreign.C.Types
> import Control.Monad
> import System.Time
> import Data.IORef
> import Data.Dynamic



> data ConnParm = ConnParm{ user, pswd, dbname :: String }


> data Session = Session
> data StmtHandle = StmtHandle
> data QueryString = QueryString String
> data QueryStringTuned = QueryStringTuned QueryResourceUsage String

 data PreparedStatement = PreparedStatement
   { stmtSession :: Session, stmtHandle :: StmtHandle }

> data Query = Query
>   { querySess :: Session
>   , queryStmt :: StmtHandle
>   , queryCounter :: IORef (IORef Int)
>   }

> data DBColumnType =
>     DBTypeInt
>   | DBTypeString
>   | DBTypeDouble
>   | DBTypeDatetime

> type BufferSize = Int

|At present the only resource tuning we support is the number of rows
prefetched by the FFI library.
We use a record to (hopefully) make it easy to add other 
tuning parameters later.

> data QueryResourceUsage = QueryResourceUsage { prefetchRowCount :: Int }

> defaultResourceUsage :: QueryResourceUsage
> defaultResourceUsage = QueryResourceUsage 100


--------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------

> connect :: ConnParm -> ConnectA Session
> connect connparm = ConnectA (return Session)

> instance ISession Session where
>   disconnect sess = return ()
>   beginTransaction sess isol  = return ()
>   commit sess = return ()
>   rollback sess = return ()

--------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------

-- Simple statements: just a string

> sql :: String -> QueryString
> sql str = QueryString str

> instance Command QueryString Session where
>   executeCommand s q  = return 0

> instance Statement QueryString Session Query where
>   makeQuery sess stmt = do
>       -- Leave one counter in to ensure the fetch terminates
>     counter <- newIORef numberOfRowsToPretendToFetch
>     refc <- newIORef counter
>     return (Query sess StmtHandle refc)

-- Statements with resource usage

> prefetch :: Int -> String -> QueryStringTuned
> prefetch count str = QueryStringTuned (QueryResourceUsage count) str

> instance Command QueryStringTuned Session where
>   executeCommand s (QueryStringTuned _ str) = executeCommand s (QueryString str)

> instance Statement QueryStringTuned Session Query where
>   -- Currently just ignore the tuning parameter. This is the stub
>   -- anyway. We only wish to test different types of statements
>   makeQuery s (QueryStringTuned _ str) = makeQuery s (QueryString str)


--------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------

See makeQuery below for use of this:

> numberOfRowsToPretendToFetch :: Int
> numberOfRowsToPretendToFetch = 3

See fetchIntVal below for use of this:
Note that rows are counted down from numberOfRowsToPretendToFetch,
so this will throw on the last row.

> throwNullIntOnRow :: Int
> throwNullIntOnRow = 1


> instance IQuery Query Session ColumnBuffer
>   where
>
>   fetchOneRow q = do
>     -- We'll pretend that we're going to fetch a finite number of rows.
>     refCounter <- readIORef (queryCounter q)
>     counter <- readIORef refCounter
>     if counter > 0
>       then (modifyIORef refCounter pred >> return True)
>       else return False
>
>
>   currentRowNum q = do
>     refCounter <- readIORef (queryCounter q)
>     counter <- readIORef refCounter
>     return counter
>
>   freeBuffer q buffer = return ()
>   destroyQuery q      = return ()


--------------------------------------------------------------------
-- result-set data buffers implementation
--------------------------------------------------------------------

> data ColumnBuffer = ColumnBuffer
>   { colPos :: Int
>   }

> buffer_pos q buffer = 
>     do 
>     let col = colPos buffer
>     row <- currentRowNum q
>     return (row,col)

An auxiliary function: buffer allocation

> allocBuffer q bufsize buftype colpos = do
>     return $ ColumnBuffer
>       { colPos = colpos
>       }

> bufferToString :: ColumnBuffer -> IO (Maybe String)
> bufferToString buffer = return $ Just "boo"

> bufferToDatetime :: ColumnBuffer -> IO (Maybe CalendarTime)
> bufferToDatetime colbuf = do
>       return $ Just $ CalendarTime
>         { ctYear = 1971
>         , ctMonth = toEnum 6
>         , ctDay = 1
>         , ctHour = 12
>         , ctMin = 1
>         , ctSec = 1
>         , ctPicosec = 0
>         , ctWDay = Sunday
>         , ctYDay = -1
>         , ctTZName = "UTC"
>         , ctTZ = 0
>         , ctIsDST = False
>         }

> bufferToInt :: ColumnBuffer -> IO (Maybe Int)
> bufferToInt buffer = return $ Just 1

> bufferToDouble :: ColumnBuffer -> IO (Maybe Double)
> bufferToDouble buffer = return $ Just 1.1

> {-
> instance DBBind (Maybe a) SessionM PreparedStatement
>     => DBBind a SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance DBBind (Maybe String) SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance DBBind (Maybe Int) SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance DBBind (Maybe Double) SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance DBBind (Maybe CalendarTime) SessionM PreparedStatement where
>   bindPos v q p = return ()

> instance (Show a, Read a) => DBBind (Maybe a) SessionM PreparedStatement where
>   bindPos v q p = return ()
> -}

> instance DBType (Maybe a) Query ColumnBuffer
>     => DBType a Query ColumnBuffer where
>   allocBufferFor _ = allocBufferFor (undefined::Maybe a)
>   fetchCol q buffer = throwIfDBNull (buffer_pos q buffer) $ fetchCol q buffer

> instance DBType (Maybe String) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q 4000 DBTypeString n
>   fetchCol q buffer = bufferToString buffer

> instance DBType (Maybe Int) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q 4 DBTypeInt n
>   fetchCol query buffer = do
>     refCounter <- readIORef (queryCounter query)
>     counter <- readIORef refCounter
>     -- last row returns null rather than 1
>     if counter == throwNullIntOnRow
>       then return Nothing
>       else bufferToInt buffer

> instance DBType (Maybe Double) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q 8 DBTypeDouble n
>   fetchCol q buffer = bufferToDouble buffer

> instance DBType (Maybe CalendarTime) Query ColumnBuffer where
>   allocBufferFor _ q n = allocBuffer q 8 DBTypeDatetime n
>   fetchCol q buffer = bufferToDatetime buffer

A polymorphic instance which assumes that the value is in a String column,
and uses Read to convert the String to a Haskell data value.

> instance (Show a, Read a) => DBType (Maybe a) Query ColumnBuffer where
>   allocBufferFor _  = allocBufferFor (undefined::String)
>   fetchCol q buffer = do
>     v <- bufferToString buffer
>     case v of
>       Just s -> if s == "" then return Nothing else return (Just (read s))
>       Nothing -> return Nothing

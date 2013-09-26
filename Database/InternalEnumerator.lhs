
|
Module      :  Database.InternalEnumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

This is the interface between the middle Enumerator layer and the
low-level, Database-specific layer. This file is not exported to the end user.

Only the programmer for a new back-end needs to consult this file.


> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}

> module Database.InternalEnumerator
>   (
>     -- * Session object.
>       ISession(..), ConnectA(..)
>     , Statement(..), Command(..), EnvInquiry(..)
>     , PreparationA(..), IPrepared(..)
>     , PreparedStmt(..)
>     , BindA(..), DBBind(..)
>     , IsolationLevel(..)
>     , Position
>     , IQuery(..)
>     , DBType(..)
>     , throwIfDBNull
>     -- * Exceptions and handlers
>     , DBException(..)
>     , throwDB
>     , ColNum, RowNum
>     , SqlState, SqlStateClass, SqlStateSubClass
>   ) where

> import Data.Typeable
> import Control.Exception.Extensible (throw, Exception)
> import qualified Control.Exception (catch)

> data IsolationLevel =
>     ReadUncommitted
>   | ReadCommitted
>   | RepeatableRead
>   | Serialisable
>   | Serializable  -- ^ for alternative spellers
>   deriving (Show, Eq, Ord, Enum)


| A wrapper around the action to open the database. That wrapper is not
exported to the end user. The only reason for the wrapper is to
guarantee that the only thing to do with the result of
'Database.Enumerator.Sqlite.connect' function is to pass it out
directly to 'Database.Enumerator.withSession'.

> newtype ConnectA sess = ConnectA (IO sess) deriving Typeable



Position within the result set. Not for the end user.

> type Position = Int

Needed for exceptions

> type RowNum = Int
> type ColNum = Int 

--------------------------------------------------------------------
-- ** Exceptions and handlers
--------------------------------------------------------------------

> type SqlStateClass = String
> type SqlStateSubClass = String
> type SqlState = (SqlStateClass, SqlStateSubClass)

> data DBException
>   -- | DBMS error message.
>   = DBError SqlState Int String
>   | DBFatal SqlState Int String
>   -- | the iteratee function used for queries accepts both nullable (Maybe) and
>   -- non-nullable types. If the query itself returns a null in a column where a
>   -- non-nullable type was specified, we can't handle it, so DBUnexpectedNull is thrown.
>   | DBUnexpectedNull RowNum ColNum
>   -- | Thrown by cursor functions if you try to fetch after the end.
>   | DBNoData
>   deriving (Typeable, Show)

> instance Exception DBException

| Throw a DBException. It's just a type-specific 'Control.Exception.throwDyn'.

> throwDB :: DBException -> a
> throwDB = throw


--------------------------------------------------------------------
-- ** Session interface
--------------------------------------------------------------------

| The 'ISession' class describes a database session to a particular
DBMS.  Oracle has its own Session object, SQLite has its own
session object (which maintains the connection handle to the database
engine and other related stuff). Session objects for different databases
normally have different types -- yet they all belong to the class ISession
so we can do generic operations like @commit@, @execDDL@, etc. 
in a database-independent manner.

Session objects per se are created by database connection\/login functions.

The class 'ISession' is thus an interface between low-level (and
database-specific) code and the Enumerator, database-independent
code.
The 'ISession' class is NOT visible to the end user -- neither the class,
nor any of its methods.

The 'ISession' class describes the mapping from connection object to
the session object. The connection object is created by the end user
(and this is how the end user tells which particular back end he wants).
The session object is not accessible by the end user in any way.
Even the type of the session object should be hidden!

> class ISession sess where
>   disconnect :: sess -> IO ()
>   beginTransaction :: sess -> IsolationLevel -> IO ()
>   commit   :: sess -> IO ()
>   rollback :: sess -> IO ()


We can have several types of statements: just plain strings,
strings bundled with tuning parameters, prepared statements.
BTW, statement with unbound variables should have a different type
from that of the statement without bound variables or the statement
with all bound variables.

| 'Command' is not a query: command deletes or updates rows, creates\/drops
tables, or changes database state.
'executeCommand' returns the number of affected rows (or 0 if DDL i.e. not DML).

> class ISession sess => Command stmt sess where
>   -- insert/update/delete; returns number of rows affected
>   executeCommand :: sess -> stmt -> IO Int

> class ISession sess =>
>   EnvInquiry inquirykey sess result | inquirykey sess -> result where
>   inquire :: inquirykey -> sess -> IO result


| 'Statement' defines the API for query objects i.e.
which types can be queries.

> class ISession sess => Statement stmt sess q | stmt sess -> q where
>   makeQuery :: sess -> stmt -> IO q


|The class IQuery describes the class of query objects. Each
database (that is, each Session object) has its own Query object. 
We may assume that a Query object includes (at least, conceptually)
a (pointer to) a Session object, so a Query object determines the
Session object.
A back-end provides an instance (or instances) of IQuery.
The end user never seens the IQuery class (let alone its methods).

Can a session have several types of query objects?
Let's assume that it can: but a statement plus the session uniquely
determine the query,

Note that we explicitly use IO monad because we will have to explicitly
do FFI.

> class ISession sess => IQuery q sess b | q -> sess, q -> b
>   where
>   fetchOneRow :: q -> IO Bool
>   currentRowNum :: q -> IO Int
>   freeBuffer :: q -> b -> IO ()
>   destroyQuery :: q -> IO ()

|A \'buffer\' means a column buffer: a data structure that points to a
block of memory allocated for the values of one particular
column. Since a query normally fetches a row of several columns, we
typically deal with a list of column buffers. Although the column data
are typed (e.g., Integer, CalendarDate, etc), column buffers hide that
type. Think of the column buffer as Dynamics. The class DBType below
describes marshalling functions, to fetch a typed value out of the
\'untyped\' columnBuffer.

Different DBMS's (that is, different session objects) have, in
general, columnBuffers of different types: the type of Column Buffer
is specific to a database.
So, ISession (m) uniquely determines the buffer type (b)??
Or, actually, a query uniquely determines the buffer.


| The class DBType is not used by the end-user.
It is used to tie up low-level database access and the enumerator.
A database-specific library must provide a set of instances for DBType.

> class DBType a q b | q -> b where
>   allocBufferFor :: a -> q -> Position -> IO b
>   fetchCol   :: q -> b -> IO a

| Used by instances of DBType to throw an exception
when a null (Nothing) is returned.
Will work for any type, as you pass the fetch action in the fetcher arg.

> throwIfDBNull :: (Monad m) => m (RowNum, ColNum) -> m (Maybe a) -> m a
> throwIfDBNull pos fetcher = do
>   v <- fetcher 
>   case v of
>     Nothing -> do
>       (row,col) <- pos
>       throwDB (DBUnexpectedNull row col)
>     Just m -> return m



------------------------------------------------------------------------
Prepared commands and statements

> newtype PreparedStmt mark stmt = PreparedStmt stmt

| This type is not visible to the end user (cf. ConnectA). It forms a private
`communication channel' between Database.Enumerator and a back end.

Why don't we make a user-visible class with a @prepare@ method?
Because it means to standardize the preparation method signature
across all databases. Some databases need more parameters, some
fewer. There may be several statement preparation functions within one
database.  So, instead of standardizing the signature of the
preparation function, we standardize on the _result_ of that
function. To be more precise, we standardize on the properties of the
result: whatever it is, the eventual prepared statement should be
suitable to be passed to 'bindRun'.

> newtype PreparationA sess stmt = PreparationA (sess -> IO stmt)


> class ISession sess => IPrepared stmt sess bound_stmt bo
>   | stmt -> bound_stmt, stmt -> bo  where
>   bindRun :: sess -> stmt -> [BindA sess stmt bo] -> (bound_stmt -> IO a) -> IO a
>   -- Should this be here? we have a need to free statements
>   -- separately from result-sets (which are handled by IQuery.destroyQuery).
>   -- It might be useful to prepare a statement, use it a number of times
>   -- (so many result-sets are created+destroyed), and then destroy it,
>   -- so it has a lifecycle independent of Queries.
>   destroyStmt :: sess -> stmt -> IO ()


| The binding object (bo) below is very abstract, on purpose.
It may be |IO a|, it may be String, it may be a function, etc.
The binding object can hold the result of marshalling,
or bo can hold the current counter, etc.
Different databases do things very differently:
compare PostgreSQL and the Stub (which models Oracle).

> newtype BindA sess stmt bo = BindA (sess -> stmt -> bo)

| The class DBBind is not used by the end-user.
It is used to tie up low-level database access and the enumerator.
A database-specific library must provide a set of instances for DBBind.
The latter are the dual of DBType.

> class ISession sess => DBBind a sess stmt bo | stmt -> bo where
>   -- | This is really just a wrapper that lets us write lists of
>   -- heterogenous bind values e.g. @[bindP \"string\", bindP (0::Int), ...]@
>   bindP :: a -> BindA sess stmt bo

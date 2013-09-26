{-# OPTIONS -fglasgow-exts #-}

module Main where

import System.Environment (getArgs)
import Control.Exception (try, handle, bracket, throwIO)
import System.IO
import Data.List (isPrefixOf)
import Data.Char (toLower)
import System.Posix (epochTime,ProcessID,getProcessID,EpochTime)
import Database.PostgreSQL.PGFunctions
import Prelude hiding (log)

docstrings = [
 "A helper to access a PostgreSQL from a `scripting' language.",
 "This function attempts to closely emulate `dbaccess' of Informix.",
 "The goal is to have my Scheme-to-Informix interface work with",
 "PostgreSQL with minimal changes.",
 "",
 "Synopsys:",
 "   dbaccess <conn-parms>",
 "where <conn-parms> is the string of database connection parameters.",
 "",
 "The program connects to the database and listens on its standard",
 "input for a SQL command. A command is an ASCII sequence terminated by",
 "NL-NULL-NL.",
 "If the command is other than SELECT or COPY, we submit it to the database.",
 "On success, no reply is generated, and we wait for a new command.",
 "If the command starts with a /*simple*/ SELECT, we expect few rows.",
 "We submit the query to the database and write the result to stdout.",
 "If the command was SELECT, we make a query and write the result,",
 "in groups, to the stdout. Each group of results has the form:",
 "  0 NL",
 " for the last group of results",
 " <nrows> <space> <ncols> NL",
 " followed by the matrix of <nrows> * <ncols> field values in the",
 " row-major order. Each field value has the form",
 " <length>NL<value>",
 " where <length> is the length of the value proper (no NL).",
 "",
 "If the command was COPY, we assume ",
 "   COPY /*<len> fname*/ table ... FROM STDIN...",
 " where <len> is the length of the file name. We open the file and",
 " send its contents to the server",
 "",
 "Any error causes the termination of this program.",
 "We log all the results to stderr."
 ]


main = do
       args <- getArgs
       hSetBuffering stderr LineBuffering
       hSetBuffering stdout LineBuffering
       case args of
		 [connparms] -> main' connparms
		 _ -> mapM_ err'note ("One argument is expected" :
				      docstrings)

err'note = hPutStrLn stderr

data Log = Log { log_pid :: ProcessID, log_time :: EpochTime }

log log_data strs = err'note (show (log_time log_data) ++
			      " [" ++ show (log_pid log_data) ++ "] " ++
			      concat strs)

main' connparms =
    do
    pid <- getProcessID
    etime <- epochTime
    let logd = Log pid etime
    catchPG (
	     bracket (openDb connparms) closeDb $ \db ->
	       log logd ["Starting"] >>
	       -- setErrorVerbosity db ePQERRORS_VERBOSE
	       main'' logd stdin db stdout)
	    (\e -> err'note (show e) >> rethrowPG e)


data CommandCategory = 
    CmdQuery | CmdSimpleQuery String | CmdCopy FilePath String | CmdOther

-- Main loop proper
main'' logd hin db hout =
    do
    cmd <- get'command hin ""
    logd <- epochTime >>= (\et -> return logd{log_time=et})
    log logd ["SQL: ",cmd]
    case classify cmd of
		      CmdQuery -> exec'complex'query logd cmd db hout
		      CmdSimpleQuery cmd -> 
			  exec'simple'query logd cmd db hout
		      CmdOther -> exec'other logd cmd db hout
		      CmdCopy fname cmd -> exec'copy logd cmd db fname
    hFlush hout
    main'' logd hin db hout
 where
 get'command hin acc = do 
		       s <- hGetLine hin
		       if s == "\NUL" then return acc 
			  else get'command hin (acc ++ s ++ "\n")
 psimple = "/*simple*/"
 classify cmd | isPrefixOf psimple cmd = CmdSimpleQuery 
					      (drop (length psimple) cmd)
 classify cmd | isPrefixOfci "select" cmd = CmdQuery
 classify cmd | isPrefixOfci "copy"   cmd = 
		  let (fname,s) = read'fname $ snd (break (== '/') cmd)
		  in CmdCopy fname ("copy " ++ s)
 classify _ = CmdOther

 read'fname ('/':'*':str) = let [(l::Int,' ':s)] = reads str
				fname = take l s
				('*':'/':s') = drop l s
			    in (fname,s')

 -- case-insensitive
 isPrefixOfci [] _ = True
 isPrefixOfci _ [] = False
 isPrefixOfci (cp:rp) (cs:rs) | (toLower cp == toLower cs) =
				  isPrefixOfci rp rs
 isPrefixOfci _ _ = False


-- ------------------------------------------------------------------------
-- Executors

nrows_in_a_group = 10 -- aka, the prefetch count
cursor_name = "pgaccessc"

-- A general query: may return too many rows. So, we have to open
-- a cursor and query by groups
exec'complex'query logd cmd db hout = 
    do
    nqExec db $ unwords ["DECLARE",cursor_name,"NO SCROLL CURSOR FOR",cmd]
    let fetchq = unwords ["FETCH FORWARD",show nrows_in_a_group,"FROM",
			  cursor_name]
    psname <- stmtPrepare db "" fetchq []
    let loop = do
	       (rs,ntuples) <- stmtExec0t db psname
	       log logd ["SQLtuples...: ",show ntuples]
	       put'tuples rs ntuples hout
	       stmtFinalise rs
	       if ntuples == 0 then return () else loop
    loop
    nqExec db $ unwords ["CLOSE",cursor_name]
    return ()



-- execute a query that is expected to produce few values.
-- That is, we're justified in asking for all tuples and so can get
-- by without opening a cursor, etc.
exec'simple'query logd cmd db hout = 
    do
    (rs,ntuples) <- stmtExecImm0 db cmd
    log logd ["SQLtuples: ",show ntuples]
    put'tuples rs ntuples hout
    stmtFinalise rs

put'tuples rs ntuples hout =
    if ntuples == 0 then hPutStrLn hout "0" else put'tuples'
 where
 put'tuples' = do
	       ncols <- fPQnfields rs
	       hPutStrLn hout $ unwords [show ntuples,show ncols]
	       sequence_ [put'col i j | i<-[0..ntuples-1], j<-[0..ncols-1]]
 put'col i j = do
	       len <- fPQgetlength rs (fromIntegral i) (fromIntegral j)
	       hPutStrLn hout $ show len
	       vp  <- fPQgetvalue rs (fromIntegral i) (fromIntegral j)
	       hPutBuf hout vp (fromIntegral len)


-- Execute command (such as DDL or DML) that produces no resultset
exec'other logd cmd db hout = 
    do
    res <- nqExec db cmd
    log logd ["SQLDone: ", show res]

-- Execute the COPY FROM STDIN command
exec'copy logd cmd db fname = 
    do
    log logd ["SQLCopy: ", fname]
    hin <- openBinaryFile fname ReadMode
    nqCopyIn db cmd hin
    hClose hin
    log logd ["SQLCopyDone"]

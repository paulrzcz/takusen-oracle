
|
Module      :  Database.Oracle.Test.OCIFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Test harness for "Database.Oracle.OCIFunctions".
This module depends on on "Database.Oracle.OCIFunctions".
so it should only use functions from there (and "Database.Oracle.OCIConstants").


> module Database.Oracle.Test.OCIFunctions (runTest) where

> import qualified Database.Oracle.OCIFunctions as OCI
> import Database.Oracle.OCIFunctions (EnvHandle, ErrorHandle, ConnHandle, StmtHandle)
> import Database.Oracle.OCIConstants
> import Foreign.Ptr
> import Foreign.C.String
> import Foreign.C.Types
> import Foreign
> import System.IO
> import System.Environment (getArgs)
> import Control.Monad
> import Test.MiniUnit
> import Data.Char


> nullAction :: IO ()
> nullAction = return ()

> printError :: String -> IO ()
> printError s = hPutStrLn stderr s

> reportAndIgnore :: ErrorHandle -> OCI.OCIException -> IO () -> IO ()
> reportAndIgnore err ociexc cleanupAction = do
>   (e, m) <- OCI.formatErrorMsg ociexc err
>   printError $ (show e) ++ ": " ++ m
>   cleanupAction

> reportAndRethrow :: ErrorHandle -> OCI.OCIException -> IO () -> IO ()
> reportAndRethrow err ociexc cleanupAction = do
>   (_, m) <- OCI.formatErrorMsg ociexc err
>   printError m
>   cleanupAction
>   OCI.throwOCI ociexc


> logon :: (String, String, String) -> IO (EnvHandle, ErrorHandle, ConnHandle)
> logon (testUser, testPswd, testDb) = do
>   env <- OCI.envCreate
>   err <- OCI.handleAlloc oci_HTYPE_ERROR (castPtr env)
>   OCI.catchOCI ( do
>     server <- OCI.handleAlloc oci_HTYPE_SERVER (castPtr env)
>     OCI.serverAttach (castPtr err) (castPtr server) testDb
>     conn <- OCI.handleAlloc oci_HTYPE_SVCCTX (castPtr env)
>     -- the connection holds a reference to the server in one of its attributes
>     OCI.setHandleAttr (castPtr err) (castPtr conn) oci_HTYPE_SVCCTX (castPtr server) oci_ATTR_SERVER
>     session <- OCI.handleAlloc oci_HTYPE_SESSION (castPtr env)
>     if (testUser == "")
>       then do
>         OCI.sessionBegin (castPtr err) (castPtr conn) (castPtr session) oci_CRED_EXT
>       else do
>         OCI.setHandleAttrString (castPtr err) (castPtr session) oci_HTYPE_SESSION testUser oci_ATTR_USERNAME
>         OCI.setHandleAttrString (castPtr err) (castPtr session) oci_HTYPE_SESSION testPswd oci_ATTR_PASSWORD
>         OCI.sessionBegin (castPtr err) (castPtr conn) (castPtr session) oci_CRED_RDBMS
>     -- the connection also holds a reference to the session in one of its attributes
>     OCI.setHandleAttr (castPtr err) (castPtr conn) oci_HTYPE_SVCCTX (castPtr session) oci_ATTR_SESSION
>     -- and we need to create a valid transaction handle for the connection, too.
>     trans <- OCI.handleAlloc oci_HTYPE_TRANS (castPtr env)
>     OCI.setHandleAttr (castPtr err) (castPtr conn) oci_HTYPE_SVCCTX (castPtr trans) oci_ATTR_TRANS
>     return (env, castPtr err, castPtr conn)
>     ) (\ociexc -> do
>       reportAndIgnore (castPtr err) ociexc nullAction
>       return (nullPtr, nullPtr, nullPtr)
>     )

> logoff :: (EnvHandle, ErrorHandle, ConnHandle) -> IO ()
> logoff (env, err, conn) = OCI.catchOCI ( do
>     session <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SESSION
>     server <- OCI.getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SERVER
>     OCI.sessionEnd err conn session
>     OCI.serverDetach err server
>     OCI.handleFree oci_HTYPE_SESSION (castPtr session)
>     OCI.handleFree oci_HTYPE_SERVER (castPtr server)
>     OCI.handleFree oci_HTYPE_SVCCTX (castPtr conn)
>     OCI.handleFree oci_HTYPE_ERROR (castPtr err)
>     OCI.handleFree oci_HTYPE_ENV (castPtr env)
>     OCI.terminate
>   ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)


> testCreateEnv :: IO ()
> testCreateEnv = do
>   env <- OCI.envCreate
>   OCI.handleFree oci_HTYPE_ENV (castPtr env)


> testConnect :: (String, String, String) -> IO ()
> testConnect args = do
>   x <- logon args
>   logoff x


> getStmt :: EnvHandle -> ErrorHandle -> ConnHandle -> String -> IO StmtHandle
> getStmt env err conn sql = do
>   stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>   OCI.stmtPrepare err (castPtr stmt) sql
>   OCI.stmtExecute err conn (castPtr stmt) 0
>   return (castPtr stmt)


> testBeginTrans :: (String, String, String) -> IO ()
> testBeginTrans args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       OCI.beginTrans err conn oci_TRANS_SERIALIZABLE 
>       stmt <- getStmt env err conn "select dummy from dual"
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)


> testExecute :: (String, String, String) -> IO ()
> testExecute args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- getStmt env err conn "select dummy from dual"
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)


> testFetch :: (String, String, String) -> IO ()
> testFetch args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- getStmt env err conn "select dummy from dual"
>       (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>       rc <- OCI.stmtFetch err (castPtr stmt)
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)

> testFetchStmt :: (String, String, String) -> IO ()
> testFetchStmt args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- getStmt env err conn "select cursor(select 101 from dual) from dual"
>       (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 8 oci_SQLT_RSET
>       stmt2 <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       withForeignPtr buf $ \p -> poke (castPtr p) stmt2
>       rc <- OCI.stmtFetch err (castPtr stmt)
>       -- stmt2 <- OCI.bufferToStmtHandle buf
>       (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt2) 1 4 oci_SQLT_INT
>       rc <- OCI.stmtFetch err (castPtr stmt2)
>       mb_i <- OCI.bufferToInt nullptr buf
>       assertEqual "testFetchStmt" (Just 101) mb_i
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt2)
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)


> testNestedFetchStmt :: (String, String, String) -> IO ()
> testNestedFetchStmt args = do
>   let
>     -- This returns two rows, each of which contains one cursor.
>     -- The first cursor returns 101, the second 102.
>     sql = "select cursor(select n from dual) from"
>       ++ " (select 101 as n from dual union select 102 from dual)"
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- getStmt env err conn sql
>       -- Create result buffer bfor row 1 StmtHandle
>       (_, obuf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 8 oci_SQLT_RSET
>       stmt101 <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       withForeignPtr obuf $ \p -> poke (castPtr p) stmt101
>       --
>       -- Fetch row 1 of 2
>       rc <- OCI.stmtFetch err (castPtr stmt)
>       --
>       -- Fetch stmt101 value
>       (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt101) 1 4 oci_SQLT_INT
>       rc <- OCI.stmtFetch err (castPtr stmt101)
>       mb_101 <- OCI.bufferToInt nullptr buf
>       assertEqual "testFetchStmt" (Just 101) mb_101
>       --OCI.handleFree oci_HTYPE_STMT (castPtr stmt101)
>       --
>       -- Create result buffer for row 2 StmtHandle...
>       -- or re-use the StmtHandle stmt101.
>       -- Both ways work.
>       --(_, obuf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 8 oci_SQLT_RSET
>       --stmt102 <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       --withForeignPtr obuf $ \p -> poke (castPtr p) stmt102
>       let stmt102 = stmt101
>       --
>       -- Fetch row 2 of 2
>       rc <- OCI.stmtFetch err (castPtr stmt)
>       -- What happens if we free here?
>       -- Will the child cursor still be valid?
>       -- No - we get ORA-01001: invalid cursor
>       -- So it looks like closing the parent closes the children too.
>       --OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>       --
>       -- Fetch stmt102 value
>       (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt102) 1 4 oci_SQLT_INT
>       rc <- OCI.stmtFetch err (castPtr stmt102)
>       mb_102 <- OCI.bufferToInt nullptr buf
>       assertEqual "testFetchStmt" (Just 102) mb_102
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt102)
>       --
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)



> testFetchFail :: (String, String, String) -> IO ()
> testFetchFail args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- getStmt env err conn "select dummy, 1 from dual"
>       (_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>       rc <- OCI.stmtFetch err (castPtr stmt)
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)


> testBind :: (String, String, String) -> IO ()
> testBind args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       -- Oracle can't cope with ? as a bind variable placeholder.
>       -- We must use the :x style instead.
>       -- Sqlite can cope with either. I think the ANSI/ISO standard is :n.
>       OCI.stmtPrepare err (castPtr stmt)
>         (OCI.substituteBindPlaceHolders "select :1 from dual union select :2 from dual")
>       withCStringLen "hello" $ \(cstr, clen) ->
>         OCI.bindByPos err (castPtr stmt) 1 0 (castPtr cstr) clen oci_SQLT_CHR
>       withCStringLen "hello2" $ \(cstr, clen) ->
>         OCI.bindByPos err (castPtr stmt) 2 0 (castPtr cstr) clen oci_SQLT_CHR
>       OCI.stmtExecute err conn (castPtr stmt) 0
>       buffer@(_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt) 1 100 oci_SQLT_CHR
>       rc <- OCI.stmtFetch err (castPtr stmt)
>       s <- OCI.bufferToString buffer
>       rc <- OCI.stmtFetch err (castPtr stmt)
>       s <- OCI.bufferToString buffer
>       rc <- OCI.stmtFetch err (castPtr stmt)
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)

> testOutputBind :: (String, String, String) -> IO ()
> testOutputBind args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       -- Oracle can't cope with ? as a bind variable placeholder.
>       -- We must use the :x style instead.
>       -- Sqlite can cope with either. I think the ANSI/ISO standard is :n.
>       OCI.stmtPrepare err (castPtr stmt) (OCI.substituteBindPlaceHolders
>         ("begin if :1 <> 'hello' then "
>         ++ "raise_application_error(-20001, 'xx-' || :1 || '-xx'); end if; "
>         ++ ":1 := 'abcdefg' || substr(:1, 1, 1) || 'ijk'; :2 := 1026; end;"))
>       nullIndFPtr1 <- mallocForeignPtr
>       nullIndFPtr2 <- mallocForeignPtr
>       sizeFPtr1 <- mallocForeignPtr
>       sizeFPtr2 <- mallocForeignPtr
>       cintFPtr <- mallocForeignPtrBytes (sizeOf (0::CInt))
>       cstrFPtr <- mallocForeignPtrBytes 16000  -- size of "hello"
>       withForeignPtr cstrFPtr $ \p ->
>         withCStringLen "hello" $ \(cstr, clen) -> copyBytes p (castPtr cstr) (clen+1)
>       withForeignPtr sizeFPtr1 $ \p -> poke p 5
>       withForeignPtr sizeFPtr2 $ \p -> poke p (fromIntegral (sizeOf (0::CInt)))
>       OCI.bindOutputByPos err (castPtr stmt) 1 (nullIndFPtr1, castForeignPtr cstrFPtr, sizeFPtr1) 16000 oci_SQLT_CHR
>       OCI.bindOutputByPos err (castPtr stmt) 2 (nullIndFPtr2, castForeignPtr cintFPtr, sizeFPtr2) (sizeOf (0::CInt)) oci_SQLT_INT
>       OCI.stmtExecute err conn (castPtr stmt) 1
>       s <- withForeignPtr cstrFPtr (peekCString . castPtr)
>       assertEqual "testOutputBind: 1" "abcdefghijk" s
>       ind <- withForeignPtr nullIndFPtr1 peek
>       assertEqual "testOutputBind: 2" 0 ind
>       i <- withForeignPtr cintFPtr peek
>       assertEqual "testOutputBind: 3" 1026 (i :: CInt)
>       size <- withForeignPtr sizeFPtr1 peek >>= return . fromIntegral
>       assertEqual "testOutputBind: 4" 11 size
>       size <- withForeignPtr sizeFPtr2 peek >>= return . fromIntegral
>       assertEqual "testOutputBind: 5" (sizeOf (0::CInt)) size
>       s <- OCI.bufferToString (undefined, castForeignPtr cstrFPtr, nullIndFPtr1, sizeFPtr1)
>       assertEqual "testOutputBind: 6" (Just "abcdefghijk") s
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)

> testOutputBindNoStrings :: (String, String, String) -> IO ()
> testOutputBindNoStrings args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       OCI.stmtPrepare err (castPtr stmt) (OCI.substituteBindPlaceHolders
>         ("begin if :1 <> 2 then "
>         ++ "raise_application_error(-20001, 'xx-' || to_char(:1) || '-xx'); end if; "
>         ++ ":1 := 130; :2 := 1026; end;"))
>       let sizeOfCInt = sizeOf (0::CInt)
>       nullIndFPtr1 <- mallocForeignPtr
>       nullIndFPtr2 <- mallocForeignPtr
>       sizeFPtr1 <- mallocForeignPtr
>       sizeFPtr2 <- mallocForeignPtr
>       cintFPtr1 <- mallocForeignPtrBytes sizeOfCInt
>       cintFPtr2 <- mallocForeignPtrBytes sizeOfCInt
>       withForeignPtr cintFPtr1 $ \p -> poke p 2
>       withForeignPtr cintFPtr2 $ \p -> poke p 3
>       withForeignPtr sizeFPtr1 $ \p -> poke p (fromIntegral sizeOfCInt)
>       withForeignPtr sizeFPtr2 $ \p -> poke p (fromIntegral sizeOfCInt)
>       let buffer1 = (nullIndFPtr1, castForeignPtr cintFPtr1, sizeFPtr1)
>       let buffer2 = (nullIndFPtr2, castForeignPtr cintFPtr2, sizeFPtr2)
>       OCI.bindOutputByPos err (castPtr stmt) 1 buffer1 sizeOfCInt oci_SQLT_INT
>       OCI.bindOutputByPos err (castPtr stmt) 2 buffer2 sizeOfCInt oci_SQLT_INT
>       OCI.stmtExecute err conn (castPtr stmt) 1
>       ind <- withForeignPtr nullIndFPtr1 peek
>       assertEqual "testOutputBind: 0" 0 ind
>       i <- withForeignPtr cintFPtr1 peek
>       assertEqual "testOutputBind: 1" 130 (i :: CInt)
>       ind <- withForeignPtr nullIndFPtr2 peek
>       assertEqual "testOutputBind: 2" 0 ind
>       i <- withForeignPtr cintFPtr2 peek
>       assertEqual "testOutputBind: 3" 1026 (i :: CInt)
>       size <- withForeignPtr sizeFPtr1 peek >>= return . fromIntegral
>       assertEqual "testOutputBind: 4" sizeOfCInt size
>       size <- withForeignPtr sizeFPtr2 peek >>= return . fromIntegral
>       assertEqual "testOutputBind: 5" sizeOfCInt size
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)

> testOutputStmtBind :: (String, String, String) -> IO ()
> testOutputStmtBind args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       OCI.stmtPrepare err (castPtr stmt) (OCI.substituteBindPlaceHolders
>         ("begin open :1 for select dummy from dual; end;"))
>       nullIndFPtr <- mallocForeignPtr
>       sizeFPtr <- mallocForeignPtr
>       bufrFPtr <- mallocForeignPtrBytes (sizeOf nullPtr)
>       -- you have to pass in a valid Statement Handle
>       -- is it replaced or just modified?
>       stmtx <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       withForeignPtr bufrFPtr $ \p -> poke p (castPtr stmtx)
>       withForeignPtr sizeFPtr $ \p -> poke p (fromIntegral (sizeOf nullPtr))
>       OCI.bindOutputByPos err (castPtr stmt) 1 (nullIndFPtr, castForeignPtr bufrFPtr, sizeFPtr) (sizeOf nullPtr) oci_SQLT_RSET
>       OCI.stmtExecute err conn (castPtr stmt) 1
>       ind <- withForeignPtr nullIndFPtr $ \p -> peek p
>       assertEqual "testOutputStmtBind: ind" 0 ind
>       --stmt2 <- withForeignPtr bufrFPtr (peek . castPtr)
>       let stmt2 = stmtx
>       buffer@(_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt2) 1 100 oci_SQLT_CHR
>       rc <- OCI.stmtFetch err (castPtr stmt2)
>       s <- OCI.bufferToString buffer
>       assertEqual "testOutputStmtBind: stmt2 dummy" (Just "X") s
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt2)
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)


This just shows that the parent StmtHandle can be safely
closed before we fetch from the child, and this doesn't
seem to matter.
This only works for PL/SQL statements that have output bind variables.
For select statements that return cursors, closing the parent
statement closes all the child cursors too.

> testOutputStmtBindCloseEarly :: (String, String, String) -> IO ()
> testOutputStmtBindCloseEarly args = do
>   (env, err, conn) <- logon args
>   OCI.catchOCI ( do
>       stmt <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       OCI.stmtPrepare err (castPtr stmt) (OCI.substituteBindPlaceHolders
>         ("begin open :1 for select 'A' from dual; end;"))
>       nullIndFPtr <- mallocForeignPtr
>       sizeFPtr <- mallocForeignPtr
>       bufrFPtr <- mallocForeignPtrBytes (sizeOf nullPtr)
>       -- you have to pass in a valid Statement Handle
>       -- is it replaced or just modified?
>       stmtx <- OCI.handleAlloc oci_HTYPE_STMT (castPtr env)
>       withForeignPtr bufrFPtr $ \p -> poke p (castPtr stmtx)
>       withForeignPtr sizeFPtr $ \p -> poke p (fromIntegral (sizeOf nullPtr))
>       OCI.bindOutputByPos err (castPtr stmt) 1 (nullIndFPtr, castForeignPtr bufrFPtr, sizeFPtr) (sizeOf nullPtr) oci_SQLT_RSET
>       OCI.stmtExecute err conn (castPtr stmt) 1
>       ind <- withForeignPtr nullIndFPtr $ \p -> peek p
>       assertEqual "testOutputStmtBindCloseEarly: ind" 0 ind
>       --stmt2 <- withForeignPtr bufrFPtr (peek . castPtr)
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt)
>       let stmt2 = stmtx
>       buffer@(_, buf, nullptr, sizeptr) <- OCI.defineByPos err (castPtr stmt2) 1 100 oci_SQLT_CHR
>       rc <- OCI.stmtFetch err (castPtr stmt2)
>       s <- OCI.bufferToString buffer
>       assertEqual "testOutputStmtBindCloseEarly: stmt2 dummy" (Just "A") s
>       OCI.handleFree oci_HTYPE_STMT (castPtr stmt2)
>     ) (\ociexc -> reportAndIgnore (castPtr err) ociexc nullAction)
>   logoff (env, err, conn)

> testSubst input expect = do
>   let actual = OCI.substituteBindPlaceHolders input
>   when (actual /= expect) (error $ "testSubstBindPlaceHolders failed: " ++ input ++ " -> " ++ actual)

> testSubstituteBindPlaceHolders :: IO ()
> testSubstituteBindPlaceHolders = do
>    testSubst "?'?'?" ":1'?':2"
>    testSubst "?" ":1"
>    testSubst "" ""
>    testSubst "x" "x"
>    testSubst "????????????" ":1:2:3:4:5:6:7:8:9:10:11:12"
>    testSubst "?'''?'''''?"   ":1'''?''''':2"


> parseArgs :: [String] -> IO (String, String, String)
> parseArgs args = do
>    let [u, p, d] = args
>    return (u, p, d)

> testlist args =
>     [ testCreateEnv
>     , testConnect args
>     , testBeginTrans args
>     , testExecute args
>     , testFetch args
>     , testFetchFail args
>     , testFetchStmt args
>     , testNestedFetchStmt args
>     , testBind args
>     , testOutputBind args
>     , testOutputBindNoStrings args
>     , testOutputStmtBind args
>     , testOutputStmtBindCloseEarly args
>     , testSubstituteBindPlaceHolders
>     ]

> runTest :: [String] -> IO ()
> runTest as = do
>   args <- parseArgs as
>   counts <- runTestTT "Oracle low-level tests" (testlist args)
>   return ()


|
Module      :  Database.MSSqlServer.Test.MSSqlFunctions
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

 
> {-# OPTIONS -fglasgow-exts #-}


> module Database.MSSqlServer.Test.MSSqlFunctions (runTest) where

> import Prelude hiding (catch)
> import Foreign
> import Foreign.C
> import Control.Monad
> import Control.Exception
> import Data.Dynamic
> import Database.MSSqlServer.MSSqlFunctions
> import System.Environment (getArgs)
> import Test.HUnit
> import Data.IORef


> nullException = MSSqlException 0 ""

> runTest :: [String] -> IO ()
> runTest args = do
>   ex <- newIORef nullException
>   printIgnoreError ex $ do
>     let [ usr, pwd, dbase, svr ] = args
>     putStrLn "init"
>     dbInit
>     putStrLn "install error handler"
>     enableErrorHandler ex
>     putStrLn "open"
>     sess <- open usr pwd dbase svr
>     putStrLn "exec"
>     execQuery sess "select 'hello'"
>     putStrLn "fetch"
>     rc <- fetchRow sess
>     putStrLn "value"
>     s <- colValString sess 1
>     putStrLn s
>     putStrLn "cancel"
>     dbCancel sess
>     putStrLn "close"
>     dbClose sess
>     putStrLn "exit"
>     dbExit
>     return ()


> testlist db = TestList $ map (\t -> TestCase (t db))
>   [ ]

> ignoreError :: IO () -> IO ()
> ignoreError action =
>   catchMSSql action (\e -> return ())

> printIgnoreError :: IORef MSSqlException -> IO () -> IO ()
> printIgnoreError refex action = catchMSSql action 
>     (\e -> do
>       realEx <- readIORef refex
>       let ex = if realEx == nullException then e else realEx
>       putStrLn (show ex)
>     )

> printPropagateError :: IORef MSSqlException -> IO a -> IO a
> printPropagateError refex action = catchMSSql action 
>     (\e -> do
>       realEx <- readIORef refex
>       let ex = if realEx == nullException then e else realEx
>       putStrLn (show ex)
>       throwMSSql ex
>     )


{-# LANGUAGE NoMonomorphismRestriction #-}
 
module Test.MiniUnitTest where

import Test.MiniUnit
import Data.IORef
import Control.Exception.MonadIO
import Control.Monad.Trans (liftIO)


tests = do
  print_ "MiniUnit tests..."
  test__assertFailure
  test__runSingleTestSuccess
  test__runSingleTestFailure
  test__runSingleTestException
  test__reportResults
  print_ "Testing runTestTT; ignore following test output."
  test__runTestTT
  print_ "MiniUnit tests done."

print_ s = liftIO (putStrLn s)

test__assertFailure = catch
  (assertFailure "test__assertFailure"
    >> error "test__assertFailure: failed: exception not thrown.")
  (\e -> print_ "test__assertFailure OK")

reportResult name result = do
  case result of
    TestSuccess -> print_ (name ++ " OK")
    TestFailure _ -> print_ (name ++ " failed")
    TestException _ -> print_ (name ++ " exception")

test__runSingleTestSuccess =
  runSingleTest (return ())
  >>= reportResult "test__runSingleTestSuccess"

test__runSingleTestFailure =
  runSingleTest (assertFailure "test__runSingleTestFailure")
  >>= reportResult "test__runSingleTestFailure"

test__runSingleTestException = do
  result <- runSingleTest (throwUserError "boo")
  case result of
    TestSuccess -> print_ "test__runSingleTestException failed"
    TestFailure _ -> print_ "test__runSingleTestException failed!"
    TestException _ -> print_ "test__runSingleTestException OK"

test__reportResults = do
  results <- mapM runSingleTest
    [assertFailure "test__runSingleTest", return (), throwUserError "boo"]
  let expect = "Test cases: 3  Failures: 1  Errors: 1"
  if reportResults results == expect
    then print_ "test__reportResults OK"
    else print_ "test__reportResults failed!"


test__runTestTT = do
  r <- runTestTT "MiniUnitTest" [assertFailure "test__runSingleTest", return (), throwUserError "boo"]
  return ()

----

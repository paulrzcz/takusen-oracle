Installing
----------
Prerequisites: GHC >= 7.6.3, Cabal >= 1.16

Using i.e. Writing data access code
-----------------------------------
There are extensive instructions and examples in the Haddock docs
for module Database.Enumerator:

http://hackage.haskell.org/packages/archive/Takusen/0.8.6/doc/html/Database-Enumerator.html

This should give you most, if not all, of the information you need to
create a program that uses Takusen.

Here's a little hello-world test case that uses Sqlite:

{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Database.Sqlite.Enumerator
import Control.Monad.Trans (liftIO)
main = flip catchDB reportRethrow $
  withSession (connect "sqlite_db") (do
    let iter (s::String) (_::String) = result s
    result <- doQuery (sql "select 'Hello world.'") iter ""
    liftIO (putStrLn result)
    )

If this is Main.hs, then
  $ ghc --make Main -o hello
should build a "hello" executable.


Paths, GHCi & runhaskell
------------------------
Just as with ensuring that your path is correctly set when building Takusen,
you must also ensure it is correctly set when building your programs.
If it is not correct, then you are likely to see linker errors.

The Cabal build script (Setup.hs) looks for various executables
on your path, in order to configure, or validate the configuration.
For example:
 - the Oracle configuration checks that sqlplus is available before continuing
 - The PostgreSQL configuration uses the pg_config program to get the library
   and include directory locations.

If you install a new back-end, you will need to re-run the installation
process to be able to use it.



Oracle gotchas on Windows
-------------------------
Some users have reported linker errors because their Oracle bin contains
hsbase.dll, which is an Oracle library related to Heterogenous Services.
This DLL overrides GHC's HSbase.dll, and therefore causes linker errors.

If you can control your Oracle client installation then either
 - don't choose Heterogenous Services when you install,
   or re-run the installer and remove it, or
 - rename hsbase.dll in Oracle bin.

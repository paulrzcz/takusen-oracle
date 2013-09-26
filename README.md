Installing
----------
Prerequisites: GHC >= 6.6, Cabal >= 1.6, filepath

Note that on Windows you will require cabal-1.6.0.2 for ghc-6.6,
as there is a bug in cabal-1.6.0.1 which breaks it on ghc-6.6 on Windows.
cabal-1.6.0.1, which is bundled with ghc-6.10, is fine for ghc-6.8 and later.

(It was possible to use Takusen with GHC-6.4; see notes in separate section
below.)

To run or build Setup.hs you will need filepath installed.
This is only needed by Setup.hs, and is not required by Takusen itself.
filepath comes with ghc-6.8 and later, but is a separate download and
install for ghc-6.6.

Ensure that the database libraries for any particular database you plan
to use are in your path e.g. ensure that %ORACLE_HOME%\bin is in your path.
If the library is not in your path, then the buildtools won't be found,
and the configure will fail.

The installation uses Cabal configurations. Each of the backends is
enabled by a flag, and the flags are all False by default. If you want
to enable a particular backend, you must enable its flag with -f
e.g. Setup configure -fodbc -foracle -fpostgres -fsqlite

Typical build, after unzipping the distribution archive (Takusen-?.?.gz):
  $ ghc --make Setup
  $ Setup configure -fodbc -foracle -fpostgres -fsqlite
  $ Setup build
  $ Setup install

Typical build, using darcs to get latest code:
  $ mkdir takusen
  $ cd takusen
  $ darcs get http://darcs.haskell.org/takusen
  $ ghc --make Setup
  $ Setup configure -fodbc -foracle -fpostgres -fsqlite
  $ Setup build
  $ Setup install

It may be that you also need to specify library or include dirs in the
setup configure step. For example, the Oracle Instant Client on Linux
puts header files in a different location from normal i.e. not under
$ORACLE_HOME. In that case you will have to use --extra-include-dirs=...,
and possibly --extra-lib-dirs=..., in the configure step.

Setup under ghc-6.6 expects the sqlite.h header file to be in the same
location as sqlite3.exe. You can get sqlite3.h from the sqlite source bundle,
and rename to sqlite.h.
  


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



PostgreSQL gotchas on Windows
-----------------------------
The PostgreSQL client library is called libpq.dll on Windows, rather than
the more typical pq.dll. This is fine when using ghc to compile, as gnu ld
is able to figure out that this is the library to use when you pass it -lpq,
but ghci is not quite so slick, and it fails to load the library.

There is any easy workaround, which is to copy libpq.dll and rename it to
pq.dll. If you do this, then you should be able to use ghci with PostgreSQL
without problems. Don't forget to ensure that PostgreSQL's bin is in your path.

In the past I've had problems with older versions of PostgreSQL and
ghc-6.4.1. Specifically, the call to PQprepare would segfault.
This occured with C programs (i.e. no Haskell) compiled with the gcc
and ld that came with ghc-6.4.1. If ld (version 2.15.91) was replaced
with an older one (2.13.91) from MSYS then the test program worked.

With PostgreSQL 8.1.5.1 and ghc-6.6 (gcc 3.4.5 (mingw special)
and ld 2.15.94) this problem seems to have vanished.



Oracle gotchas on Windows
-------------------------
Some users have reported linker errors because their Oracle bin contains
hsbase.dll, which is an Oracle library related to Heterogenous Services.
This DLL overrides GHC's HSbase.dll, and therefore causes linker errors.

If you can control your Oracle client installation then either
 - don't choose Heterogenous Services when you install,
   or re-run the installer and remove it, or
 - rename hsbase.dll in Oracle bin.



GHC-6.4 and Takusen
-------------------
It was possible to use Takusen with GHC-6.4.
We no longer test this configuration, so you may need to do some work
it you want to go down this path.

You will need to install Data.Time, which is quite a chore on Windows
because of the dependencies. You will also need MSYS installed in order
to be able to run autoreconf, so get that out of the way first.

The summary for Windows is:

darcs get --partial http://www-users.cs.york.ac.uk/~ndm/filepath/
(or download and unzip the prepared distribution)
normal cabal configure, build, install

darcs get --partial http://www.cse.unsw.edu.au/~dons/code/fps
(or download and unzip the prepared distribution)
normal cabal configure, build, install

darcs get http://darcs.haskell.org/packages/Win32
Edit Win32.cabal, add fps to build-depends.
normal cabal configure, build, install

darcs get http://semantic.org/TimeLib/TimeLib
There are two packages here: time and fixed. Ignore time.
Go into fixed and do cabal configure, build, install

darcs get http://darcs.haskell.org/packages/time
Edit time.cabal, add fixed and Win32 to build-depends.
create Setup.hs:
  import Distribution.Simple
  main = defaultMainWithHooks defaultUserHooks
From MSYS shell, not Windows cmd.exe:
  $ autoreconf
  $ ghc --make Setup
  $ Setup configure
  $ Setup build
  $ Setup install

And finally, build Takusen with the normal cabal configure,
build, install.

# Changelog

Notable changes to takusen-oracle. Entries for versions before 0.10.0.0 were
reconstructed from git history and Hackage release metadata.

## 0.10.0.0 (unreleased)

### Compatibility

- Buildable with GHC 9.10.3 and cabal-install 3.16 (previous release targeted
  GHC 7-8):
  - added missing imports (`liftM` in `Control.Exception.MonadIO` and
    `Database.Enumerator`; `when`, `void` in `Database.Enumerator`)
  - resolved the `Sunday` constructor ambiguity between `System.Time`
    (old-time) and `time >= 1.12` (`Database.Util`,
    `Database.Oracle.OCIFunctions`)
  - added `FlexibleContexts` and `NondecreasingIndentation` pragmas required
    by modern GHC in the library and test modules
  - `Test.MiniUnitTest` imports `catch` from `Control.Exception` explicitly
    (it used to come from `Prelude` in pre-base-4 GHC) with an explicit
    `SomeException` annotation
- Removed the call to `OCITerminate()` when disconnecting from the database
  (#12; merged in 2019 but never included in a Hackage release)

### Build system

- `custom-setup` stanza with explicit `setup-depends` added to the cabal file
  and `cabal-version` raised to `1.24`; without it, modern cabal-install
  cannot resolve the custom `Setup.hs` dependencies
- Oracle Instant Client is expected under `~/instantclient` (see
  `Extra-Lib-Dirs` in the cabal file); create a symlink if your client
  lives elsewhere
- Haskell Language Server 2.14 works out of the box (cabal cradle)
- The test executables are enabled via `flags: +buildtests` (unchanged), e.g.
  through a local `cabal.project`

## 0.9.4.1 (2016-11-08)

- GHC 8 support
- Fix for statement reuse detection (#8)
- Simplified build configuration: the build no longer guesses the Oracle
  home; link against the Instant Client via `Extra-Lib-Dirs`

## 0.9.3 (2015)

- `OCI_THREADED` mode when creating the environment handle (#4)
- Fix for a resource leak in command binding with prepared statements
- Sources converted from literate Haskell (`.lhs`) to `.hs`

## 0.9.0 - 0.9.2

- Initial Oracle-only split from the original Takusen library; support for
  Oracle 11gR2/12c instant client and GHC 7.4+. See git history for details.

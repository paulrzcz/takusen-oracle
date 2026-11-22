# AGENTS.md

Haskell Oracle client library (OCI via FFI). Single cabal package; build with **cabal**, not stack.

## Build

- Active toolchain: cabal + GHC 9.10.x. `stack.yaml` is stale legacy (lts-7.8 / GHC 8 era) — do not use stack.
- `cabal build` builds the library plus both test executables (`cabal.project` sets `flags: +buildtests`). There are no `test-suite` stanzas, so `cabal test` runs nothing.
- Linking needs the Oracle Instant Client: `Extra-Lib-Dirs` is hardcoded to `/Users/paul/instantclient` (symlink your client there if it lives elsewhere, or `-lclntsh` fails at link time).
- FFI bindings in `Database/Oracle/OCIFunctions.hs` are hand-written; there is no codegen/pre-build step (`OCIFunctions_stub.c/h` in `Extra-tmp-files` are leftovers).

## Tests

- DB-free check: `cabal run miniunit_tests`. It intentionally prints failing self-tests ("ignore following test output") and still exits 0 — don't treat that output as failure.
- Integration: `cabal run takusen_tests -- noperf <user> <password> <connect-string>` (e.g. `localhost/XE`). Use `perf` as the first arg to also run the performance tests — they process 2^20 rows and take minutes.
- Integration tests create/drop their fixtures at runtime (tables, views, package `Takusen`, procedure `takusenTestProc`); the DB user needs create/drop privileges. Objects are documented in the comment block at the top of `Database/Oracle/Test/Enumerator.hs`.

## Layout notes

- Backend-agnostic core: `Database.Enumerator`, `Database.InternalEnumerator` (internal), `Database.Util`, `Control.Exception.MonadIO`, `Foreign.C.UTF8`. Oracle backend: `Database.Oracle.*` — the only real backend.
- `Database/Stub/`, `lhs2hs.hs`, and `Example/` are legacy/illustrative; cabal does not build them (`Example/` is a separate package only referenced by `stack.yaml`).
- This is 2004-era code ported to GHC 9.10 (see CHANGELOG 0.10.0.0): expect CPP, legacy extensions (`OverlappingInstances`, `UndecidableInstances`), `System.Time` from old-time, and `catch` imported from `Prelude`. Don't mass-modernize; portability fixes must be mirrored in both library and test modules.
- Dependency bounds follow PVP with explicit upper bounds (see recent commits for base/mtl/time/old-time/random). Keep that style when touching `Build-Depends`.
- Known benign quirk: the library stanza has two `Exposed-modules:` blocks; Cabal compiles all listed modules, so it builds fine.

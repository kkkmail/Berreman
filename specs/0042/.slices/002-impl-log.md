# Impl-log — spec 0042, slice 002 (IMPLEMENT) — attempt 02

## Progress

- **Read protocol.** Task file (`002-attempt-02-task.txt`, retry hint: attempt 01
  failed the `build` gate) → `implement_worker.system-md`
  (`src/ai_strategy_generator/multistep/`, not the bare path the task file's
  "System prompt" line points at — that literal path is still absent; see
  Gotchas) → `arc-runner.system-md` base → project `arc-runner.user-md` →
  CLAUDE.md → slice `002.slice-md`. Reviewed attempt 01's impl-log / SoW.
- **Diagnosed the attempt-01 build failure** from the gate capture
  `.artifacts/002-01-console-20260716T013243Z.log`. Exactly ONE compile error,
  and it was in the *test* file, not the production code:

  ```
  BerremanTests\MuellerReconstructionTests.fs(66,5): error FS0960:
  'let' and 'do' bindings must come before member and interface definitions in
  type definitions
  ```

  `MuellerReconstructionTests` is an F# **class type** (`type … () =`). In a
  class type every `let` binding (a private field) must precede the FIRST
  `member`. Attempt 01 placed the shared `let asymmetricM = …` fixture at line 66,
  *after* the `[<Fact>]` members — illegal. The Domain file
  (`MuellerReconstruction.fs`) had already compiled to its DLL in that same run
  (log line ~110), confirming the production code was fine and only the test-file
  ordering broke the build.
- **Fixed** by moving the `asymmetricM` fixture (and its comment) up to sit
  immediately after the existing `assertMuellerEqual` `let` binding, ahead of all
  members. No production-code change; no test-body change.
- **Re-traced the new members' types** against the real signatures
  (`vecColumnMajor`/`muellerOfVecColumnMajor` ↔ `MuellerMatrix`,
  `MuellerMatrix.(*)` at Fields.fs:645 → `StokesVector`, `RealVector4.Item` at
  Geometry.fs:130, `MatrixDiff` fields) to rule out a downstream error that the
  structural FS0960 could have masked. All type-check.
- **Verified LF endings** on both edited files via `od -tx1 | grep -o '0d' | wc`
  → 0 CR bytes each (the `od -c` view gives a known false positive here).

## Files modified

- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — reordered the
  `asymmetricM` fixture to precede the `[<Fact>]` members (FS0960 fix). Nothing
  else changed.

`MuellerReconstruction.fs` (Domain) is UNCHANGED from attempt 01 — it compiled
cleanly there and its four primitives (`kron4`, `vecColumnMajor`,
`muellerOfVecColumnMajor`, `MatrixDiff` + `frobeniusDiff`) are correct. No
`.fsproj` edits (both files were wired into their compile lists in slice 001).

## Design decisions

- **Fixture placement, not a rewrite.** The minimal, correct fix for FS0960 is to
  order the `let` before the members — not to convert the fixture to a module
  `let` or duplicate it per test. It stays a single shared binding; the class is
  reconstructed per xUnit test, so recomputing the tiny 4×4 per test is free.
- Index/algebra conventions are unchanged and re-derived to still hold:
  `kron4[4*i+j] = s[i]·a[j]`, `vecColumnMajor[4*i+j] = M[j,i]`, so
  `Σ_k kron4·vec = Σ_j a[j]·(M s)[j] = a·(M s)` (the transpose guard); and
  `muellerOfVecColumnMajor (vecColumnMajor M) = M` (the round-trip).

## Testing state

Per the `IMPLEMENT` worker's **Invariant 6 (act only; run no checks)**, this
worker did NOT execute the `build` / `unit-tests` / `constructor-unit-tests`
gates — the arc-runner's deterministic gate engine runs them after exit. The one
attempt-01 build error (FS0960, test-file `let`/member ordering) is fixed by the
reorder; the fix is mechanical and the surrounding types were re-verified by hand
against the engine seams. `commit_ready: true`.

## Artifacts

None captured this round (a one-line structural reorder of an existing test
file; no runtime traces). The diagnosis used the pre-existing gate capture
`.artifacts/002-01-console-20260716T013243Z.log`.

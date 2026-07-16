# State of the world — spec 0042, slice 005 (ADD_CONTRACT STORE_XDUO_0009 MuellerDataProxy)

## Where we are

Slice 005 is the fifth slice of arc 0042. It opens a NEW contract pair by
*declaring* the Mueller measured-data CSV-**LOAD** seam — the IO boundary that turns
an experiment family's `DataFilePath` into its parsed raw capture rows. It is the
disk-read EDGE that feeds the reconstruction pipeline built in slices 002–004: each
loaded row carries one capture's analyzer azimuth (`description`, the engine `Angle`)
and averaged intensity (`avgTotal`) — the exact (azimuth, intensity) pairs `kron4`
and the `MuellerSolverProxy` consume. Following the `ADD_CONTRACT` discipline, the
surface is *declared* only (no filesystem read); a later `IMPLEMENT_CONTRACT
STORE_XDUO_0009` supplies the real file-backed store. The contract registry already
carries `STORE_XDUO_0009` at `lifecycle: "declared"`, `declaringStep: 5`.

## What's working

- Declare `MuellerRawRow` — the elevated raw-capture row DTO at the load boundary
  (`experiment` / `captureIndex` / `capturedAt` / `description : Angle` / `avgTotal`).
- Declare the typed `MuellerDataError` channel (`MalformedRow` | `EmptyFile`).
- Declare the `[<ReferenceEquality>] MuellerDataProxy` seam (`tryLoadFamily :
  DataFilePath -> Result<MuellerRawRow list, MuellerDataError>`), reusing
  `Experiments.DataFilePath` — no parallel path DU.
- Add an in-memory mock (canned rows keyed by `DataFilePath.value`) plus three
  mock-driven BerremanTests facts: load through the exact signature, unknown path →
  typed error (never a throw), and reference equality.
- Leave the contract `declared`; the real disk-backed store lands in a later
  IMPLEMENT_CONTRACT.

## Tests

Gate roster for this slice (from `005.gates`): `build`, `unit-tests`
(`berreman_unit_tests`), `constructor-unit-tests` (`constructor_unit_tests`).

Per the `ADD_CONTRACT` worker's **Invariant 6 (the worker acts; it runs no checks)**,
this worker did NOT execute the gates — the deterministic gate engine runs them after
exit. New coverage in `BerremanTests/MuellerReconstructionTests.fs`, all driving the
mock `MuellerDataProxy`:

- **canned-rows load** (the acceptance) — a stub proxy loads two canned
  `MuellerRawRow`s through the EXACT `tryLoadFamily` signature (pinned by an
  explicitly-typed local); asserts the list length and value;
- **unknown path → typed error** — an unmapped `DataFilePath` returns
  `Error (EmptyFile _)` with a non-empty reason — never a throw;
- **reference equality** — the `[<ReferenceEquality>]` proxy compares by identity.

The three facts only ADD to the `berreman_unit_tests` count (a `count_at_least` gate,
baseline 132 from checkpoint 3 → 135); no other project's tests changed.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
```

(The gate engine records the real captured counts post-exit; the baseline for a
`count_at_least` gate is the prior green step's checkpoint, not a value the worker
authors — 0 is the safe floor. The three new facts only add to the count.)

## Architecture

- **A declared seam, kept as pure DATA.** `MuellerDataProxy` follows the
  `ExperimentDataProxy` / `MuellerSolverProxy` functional-proxy convention: a record
  of one camelCase `Result`-returning function, so logic that holds the proxy stays
  referentially transparent and a test substitutes an in-memory stub of the SAME
  shape. Its one field is function-valued (no structural equality), so the record is
  `[<ReferenceEquality>]` — a host context (Optimization / Elmish) that holds one
  keeps its required equality, comparing by identity.
- **The typed channel keeps the boundary total.** A malformed row or an empty file is
  a value on the `MuellerDataError` channel, never a throw across the proxy — so the
  future real store's IO/parse exceptions are caught AT the seam and mapped, exactly
  as the sibling `SolverError` / `ExperimentDataError` channels do.
- **`DataFilePath` is reused, not re-minted.** The seam names
  `Experiments.DataFilePath` (`Experiments.fs:142`) via
  `open OpticalConstructor.Domain.Experiments`; `MuellerReconstruction.fs` already
  compiles LAST in the Domain, so the type is in scope with no `.fsproj` reordering.
- **Primitives confined to the DTO seam.** `MuellerRawRow`'s bare `string` / `int` /
  `DateTimeOffset` / `float` are permitted because it is the row-level DTO at the
  parse/storage boundary; the one physical quantity, the analyzer azimuth
  `description`, is elevated to the engine `Angle`.

## Deferred

- The REAL file-backed `MuellerDataProxy` — read the CSV text, parse each row into a
  `MuellerRawRow`, map any IO/parse exception onto `MuellerDataError` — is the later
  `IMPLEMENT_CONTRACT STORE_XDUO_0009`. This slice ships the seam + mock + test only.
- Wiring the loaded rows into the full reconstruction (assemble the design from many
  `(azimuth, intensity)` captures, drive the `MuellerSolverProxy`, read the
  `frobeniusDiff` residual) remains a later slice.

## Gotchas

- **System-prompt path drift.** The task file's "System prompt" line points at
  `C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md`, which does not
  exist; the real file is
  `.../src/ai_strategy_generator/multistep/add_contract_worker.system-md` (a thin
  `ADD_CONTRACT` delta over `.../multistep/arc-runner.system-md`). Read those; do not
  block on the literal task-file path. (Same drift the 003/004 workers recorded.)
- **`MuellerRawRow` primitives are intentional (recorded choice).** The slice spec
  pins the exact shape; `string` / `int` / `DateTimeOffset` / `float` are the raw
  parsed fields at the load boundary (CLAUDE.md permits primitives at the storage
  seam), and the azimuth is elevated to `Angle`. Implemented verbatim.
- **Unknown path → `EmptyFile` (recorded choice).** The acceptance only requires *a*
  typed error on a miss; for an in-memory mock a miss means "no rows", so `EmptyFile`
  is the natural fit over `MalformedRow`. Both are valid `MuellerDataError` cases.
- **No `.contracts-json` edit for an ADD_CONTRACT.** The step-compiler already
  registered `STORE_XDUO_0009` at `declared` (declaringStep 5), the correct terminal
  state for a declaration step — flipping to `implemented` is the later
  IMPLEMENT_CONTRACT's job, not this one's.
- **No record-field-label ambiguity / no FS0960.** The `description` field name
  recurs elsewhere in the Domain but only inside modules this file does not open, and
  `MuellerRawRow`'s full label set is unique (plus the fixture is type-annotated). The
  mock is bound as class-level `let`s ahead of the members; the new facts are members.
- **Line endings.** Both edited `.fs` files are pure LF (verified with
  `od -An -tx1 | grep -o '0d' | wc -l` → 0).

## Changelog

- 2026-07-15 — slice 005 (ADD_CONTRACT STORE_XDUO_0009) attempt 01: declare the
  Mueller measured-data CSV-LOAD seam — the elevated `MuellerRawRow` row DTO, the
  typed `MuellerDataError` (`MalformedRow` | `EmptyFile`) channel, and the
  `[<ReferenceEquality>] MuellerDataProxy` (`tryLoadFamily : DataFilePath ->
  Result<MuellerRawRow list, MuellerDataError>`) reusing `Experiments.DataFilePath` —
  plus an in-memory mock and three mock-driven facts (canned-rows load through the
  exact signature, unknown path → typed error, reference equality). Contract left
  `declared`.

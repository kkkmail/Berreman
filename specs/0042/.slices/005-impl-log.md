# Impl-log — spec 0042, slice 005 (ADD_CONTRACT STORE_XDUO_0009 MuellerDataProxy)

## Progress

- [x] Read the system prompt (`add_contract_worker.system-md` + shared base
      `arc-runner.system-md`), the project prompt (`arc-runner.user-md`), the slice
      spec (`005.slice-md`), and the reference conventions: the declared
      `ExperimentDataProxy` seam (`ExperimentDataProxy.fs:43`) and its mock/test
      (`ExperimentDataProxyTests.fs:38`), `Experiments.DataFilePath`
      (`Experiments.fs:142`), and the sibling `MuellerSolverProxy` seam + mock in
      `MuellerReconstruction.fs` / `MuellerReconstructionTests.fs`.
- [x] Wrote impl-plan.
- [x] Declared `MuellerRawRow` / `MuellerDataError` / `[<ReferenceEquality>]
      MuellerDataProxy` in `MuellerReconstruction.fs`.
- [x] Added the mock (`makeMockData` / `cannedRows` / `seededDataMock`) + three
      mock-driven `[<Fact>]`s to `MuellerReconstructionTests.fs`.
- [x] Verified LF line endings on the two source files (`od -An -tx1` → 0 CR bytes).
- [x] Wrote state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
  — added `open OpticalConstructor.Domain.Experiments` (for `DataFilePath`) and
  appended the DECLARED CSV-load seam at the end of the module: the elevated row
  DTO `MuellerRawRow` (`experiment` / `captureIndex` / `capturedAt` / `description :
  Angle` / `avgTotal`), the typed channel `MuellerDataError = MalformedRow | EmptyFile`,
  and the `[<ReferenceEquality>] MuellerDataProxy = { tryLoadFamily : DataFilePath ->
  Result<MuellerRawRow list, MuellerDataError> }`. No behaviour — the seam only.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — added
  `open OpticalConstructor.Domain.Experiments`, a `let`-bound in-memory mock
  (`cannedRows` + `makeMockData` keyed by `DataFilePath.value` + `seededDataMock`,
  ahead of the members per FS0960), and three `[<Fact>]`s: a canned-rows load through
  the EXACT `tryLoadFamily` signature (pinned by an explicitly-typed local), an
  unknown path yielding a typed `MuellerDataError` (never a throw), and a
  reference-equality check (mirrors the `MuellerSolverProxy` pair).

No `.fsproj` edit (both files already belong to their projects; BerremanTests already
references `OpticalConstructor.Domain`). No `.contracts-json` edit — the step-compiler
already registered `STORE_XDUO_0009` at `lifecycle: "declared"`, `declaringStep: 5`,
which is the correct terminal state for an ADD_CONTRACT (the surface is *declared*,
not *implemented*).

## Testing state

Per the `ADD_CONTRACT` delta's **Invariant 6 (the worker acts; it runs no checks)** —
inherited from the shared base and reiterated in `add_contract_worker.system-md` —
this worker did NOT execute the `build` / `unit-tests` / `constructor-unit-tests`
gates. Gate execution belongs to the arc-runner's deterministic gate engine, which
runs the slice roster (`005.gates`) after I exit. The code is written
correct-by-construction as a direct structural clone of two seams that already
compile in this same module/test file (`ExperimentDataProxy` and `MuellerSolverProxy`).

The three new facts satisfy the acceptance: a stub `MuellerDataProxy` loads canned
rows through the exact `tryLoadFamily` signature, an unknown path returns a typed
`MuellerDataError` (`EmptyFile`, never a throw), and the proxy compares by reference.
They only ADD to `berreman_unit_tests` (baseline 132 from checkpoint 3 → 135); the
`count_at_least` gate never regresses.

`commit_ready: true` — every requirement enumerated in `005.slice-md` is addressed
this round (the declared seam + its elevated row/error types, the in-memory mock, and
the mock-driven acceptance test); nothing deferred to a "round 2".

## Artifacts

None — an ADD_CONTRACT declaration slice produces no captured logs / traces. (The
per-arc artifacts folder is `specs/0042/.artifacts/`.)

## Gotchas

- **System-prompt path drift.** The task file's "System prompt" line points at
  `C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md`, which does not
  exist; the real file is
  `.../src/ai_strategy_generator/multistep/add_contract_worker.system-md` (a thin
  `ADD_CONTRACT` delta over `.../multistep/arc-runner.system-md`). Read those; do not
  block on the literal task-file path. (Same drift the 003/004 workers recorded.)
- **Bare primitives in `MuellerRawRow` are intentional (recorded choice).** The slice
  spec pins the exact shape `{ experiment : string; captureIndex : int; capturedAt :
  System.DateTimeOffset; description : Angle; avgTotal : float }`. `string` / `int` /
  `DateTimeOffset` / `float` would normally violate "elevate every primitive", but
  this is the **row-level DTO at the parse/storage seam**, exactly where CLAUDE.md
  permits primitives; the one physically-meaningful quantity, the analyzer azimuth
  `description`, IS elevated to the engine `Angle`. Implemented verbatim as specified.
- **`DataFilePath` is reused, not re-minted.** The seam reuses
  `Experiments.DataFilePath` (`Experiments.fs:142`) via `open
  OpticalConstructor.Domain.Experiments`; no parallel path DU. `MuellerReconstruction.fs`
  already compiles LAST in the Domain (after `Experiments.fs`), so the type is in scope.
- **No record-field-label ambiguity.** Several Domain records carry a `description`
  field (`ElementId.fs`, `MaterialStore.fs`, `MaterialLibrary.fs`, `SampleStore.fs`),
  but they live in modules `MuellerReconstruction.fs` / the test file do not open, and
  `MuellerRawRow`'s full label set (`experiment` + `captureIndex` + `avgTotal` …) is
  unique; the fixture is also type-annotated (`cannedRows : MuellerRawRow list`), so
  record resolution is unambiguous.
- **Unknown path → `EmptyFile` (recorded choice).** The acceptance only requires *a*
  typed `MuellerDataError` on a miss. For an in-memory mock a miss means "no rows for
  this path", so `EmptyFile` is the better semantic fit than `MalformedRow` (which is
  for a row that failed to parse). Both are valid; `EmptyFile` chosen deliberately.
- **Deterministic `capturedAt`.** The mock's timestamps are fixed
  `System.DateTimeOffset` literals — no ambient-clock read — so the fixture is
  reproducible across runs (CLAUDE.md: inject/pin time, never call `.UtcNow`).
- **FS0960 respected.** The mock (`cannedRows` / `makeMockData` / `seededDataMock`) is
  bound as class-level `let`s BEFORE the first `[<Fact>]` member; the three new facts
  are members only. No binding-order trap.
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
  `declared`; the real file-backed store lands in a later IMPLEMENT_CONTRACT.

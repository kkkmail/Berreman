# Impl-plan — spec 0042, slice 005 (ADD_CONTRACT STORE_XDUO_0009 MuellerDataProxy)

## Goal

Declare the Mueller measured-data CSV-**LOAD** seam in the Domain: the
DECLARED-lifecycle `[<ReferenceEquality>] MuellerDataProxy`, its elevated raw-row
type `MuellerRawRow`, and the typed `MuellerDataError` channel — plus an in-memory
**mock** and a **mock-driven test** that pins the surface. No real filesystem read
(that is the later `IMPLEMENT_CONTRACT STORE_XDUO_0009`).

This is a `Contract`-family `ADD_CONTRACT` step: author the interface/proxy + mock
+ test, leave the contract `declared`. `.contracts-json` already carries
`STORE_XDUO_0009` at `lifecycle: "declared"`, `declaringStep: 5` (the step-compiler
registered it), so no registry edit is required for an ADD_CONTRACT.

## Approach

1. **`MuellerReconstruction.fs`** (OpticalConstructor.Domain) — append the seam at
   the end of the module (it already compiles LAST, after `Experiments.fs`):
   - `open OpticalConstructor.Domain.Experiments` so `DataFilePath`
     (`Experiments.fs:142`) is in scope — the seam REUSES it, no parallel path DU.
   - `type MuellerRawRow = { experiment : string; captureIndex : int; capturedAt :
     System.DateTimeOffset; description : Angle; avgTotal : float }` — the row-level
     DTO at the load boundary. `description` is the engine `Angle` (elevated); the
     bare primitives (`string`/`int`/`DateTimeOffset`/`float`) are permitted here
     because this is the parse/storage seam (CLAUDE.md).
   - `type MuellerDataError = MalformedRow of reason : string | EmptyFile of reason
     : string` — the typed channel (never a throw across the boundary).
   - `[<ReferenceEquality>] type MuellerDataProxy = { tryLoadFamily : DataFilePath
     -> Result<MuellerRawRow list, MuellerDataError> }` — the functional-proxy seam
     (the `ExperimentDataProxy.fs:43` convention).

2. **`MuellerReconstructionTests.fs`** (BerremanTests) — add the MOCK + facts:
   - a `let`-bound canned `MuellerRawRow list` + `makeMockData` (rows keyed by
     `DataFilePath.value`; a miss returns `Error (EmptyFile _)`, never a throw),
     bound ahead of the `[<Fact>]` members (FS0960);
   - a fact loading canned rows through the EXACT `tryLoadFamily` signature (pinned
     by an explicitly-typed local);
   - a fact asserting an unknown path returns a typed `MuellerDataError`;
   - a reference-equality fact (mirrors the `MuellerSolverProxy` pair).
   - add `open OpticalConstructor.Domain.Experiments` for `DataFilePath`.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`

No `.fsproj` change: both files are already in their projects, and BerremanTests
already references `OpticalConstructor.Domain`. `.contracts-json` already correct.

## Risks

- **Record-field-label ambiguity.** Several Domain records have a `description`
  field, but they live in modules `MuellerReconstruction.fs` does not open, and
  `MuellerRawRow`'s full label set is unique — plus the fixture is type-annotated.
- **Compile order.** `MuellerReconstruction.fs` is last in the Domain; `DataFilePath`
  (`Experiments.fs`) is available. ✓
- **LF endings** must be preserved on both `.fs` files.
- Per **Invariant 6** the worker runs NO gates — correct-by-construction, mirroring
  the already-compiling `ExperimentDataProxy` / `MuellerSolverProxy` seams.

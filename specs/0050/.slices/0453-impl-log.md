# Step 0453 — impl log (IMPLEMENT, S132-TABLE-SIGMA)

## Progress

- Read the task, the arc-runner base prompt, the IMPLEMENT delta, the project prompt, the slice, and the "Binding
  ruling" sections of rulings R0001–R0005.
  - The rulings confine their extra authority to steps 0427, 0428 and 0436.
  - Here they only ask that their rules be preserved. This step touches none of the files they govern.
- Surveyed:
  - `Dispersion.fs`: `TabulatedInterpolation`, `TabulatedTableError` and `TabulatedNkTable`, all from step 2;
  - `MaterialImport.fs`' `entryFromTabulated`;
  - `CanonicalJson.options`, which uses FSharp.SystemTextJson with `WithUnwrapOption`;
  - the tabulated facts of `EpsWithDispValueTests` and the round-trip helpers of `EntryFileJsonTests`;
  - every `TabulatedTableError` match in the tree. There are three complete matches: `MaterialImport.fs`,
    `EpsWithDispValueTests.tableOf` and `LibraryFacetsTests`.
- Added `TabulatedNkUncertainty`, `InvalidUncertaintyErr`, `uncertaintyOpt` and `withUncertainty`, and mapped the new
  case in the three matches.
- Added the two census facts.
- Build green. The first elevation run failed with ELEV010 on `withUncertainty`'s two `float array` parameters.
  - Repaired: `withUncertainty` now takes the `TabulatedNkUncertainty` carrier (see Decisions), and the facts were
    updated to match.
  - Rebuilt. Elevation is then clean, and every test run is green on the final build (see Testing state).

## Files modified

- `Berreman/Berreman/Dispersion.fs`:
  - `TabulatedTableError` gains `InvalidUncertaintyErr of reason : string`, and its doc now names `withUncertainty`.
  - New `TabulatedNkUncertainty`:
    - a record of `sigmaN : float array` and `sigmaK : float array`;
    - marked `[<Softellect.Sys.NumericCarrier("per-sample one-sigma uncertainties of a tabulated n,k table, read
      beside its columns")>]`;
    - declared just above `TabulatedNkTable`.
  - `TabulatedNkTable` gains its last field, `uncertaintyOpt : TabulatedNkUncertainty option`, with a one-line doc.
    `tryCreate` keeps its signature and sets `None`.
  - New `member this.withUncertainty (uncertainty : TabulatedNkUncertainty) : Result<TabulatedNkTable,
    TabulatedTableError>`. The how_to's two `float array` parameters were replaced by the carrier record; see
    Decisions.
    - If either column's length is not the table's, it answers `SampleCountMismatchErr "<count> samples, <a> sigma n
      values and <b> sigma k values"`.
    - Otherwise, the first NaN, infinite or negative value answers `InvalidUncertaintyErr "sigma n|sigma k at index
      <i> is <v>; an uncertainty is finite and non-negative"`. Sigma n is checked before sigma k.
    - Otherwise it answers `{ this with uncertaintyOpt = Some uncertainty }`. The arrays are kept as given, never
      sorted or copied.
  - `complexIndex` is untouched.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`: `entryFromTabulated` answers
  `InvalidUncertaintyErr reason` as `NoDataErr reason`, beside the other three cases.
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`:
  - `tableOf` maps the new case to `failwith`, as it maps the others;
  - new fact `A tabulated table takes per-sample uncertainties of its own length and refuses any other`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs`:
  - private `stepTwoTable`, the hand-written 400/500/700 nm table of step 2's facts;
  - private `tabulatedTableOf`;
  - new fact `a tabulated material keeps its per-sample uncertainties through an entry file`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryFacetsTests.fs`: one line in the existing fact
  `a tabulated axis lands on the tabulated n,k facet key`. Its complete match on `TabulatedTableError` maps the new case
  to `Assert.Fail`, as it maps the other three. See Decisions.

`specs/0050/.manifest.state.json` was already modified when the round started. It is the supervisor's file, and this
round did not touch it.

## Decisions

- **`withUncertainty` takes the `TabulatedNkUncertainty` carrier, not two `float array` parameters.** This deviates
  from the how_to's signature `withUncertainty (sigmaN : float array) (sigmaK : float array)`.
  - The first elevation run reported `ELEV010 Berreman/Berreman/Dispersion.fs 635 withUncertainty mixes bare
    primitive(s) float with elevated types in one signature ... (no boundary claim exempts this)`.
  - `tryCreate` has the same shape and is not flagged, because the analyzers' built-in `elevationPatternMembers`
    (`value`, `tryCreate`, `create`, `ofWire`, …) exempt it. `withUncertainty` is not on that list.
  - AGENTS.md: "Where any document ... disagrees with the analyzers, the analyzers win" (spec 0048 `.manual/002`
    R6). Editing `.elevation-analyzers.json` or recording the finding is forbidden.
  - The carrier is the step's own `NumericCarrier` record, so the parameter is elevated and the primitives stay inside
    the carrier.
  - Everything else is as briefed:
    - the contract symbol `Berreman.Dispersion.TabulatedNkTable.withUncertainty`;
    - the `Result<TabulatedNkTable, TabulatedTableError>` return type;
    - the length check (`SampleCountMismatchErr`) and the value check (`InvalidUncertaintyErr`);
    - `uncertaintyOpt` is set only here, and `tryCreate` sets it to `None`.
  - The facts call `table.withUncertainty { sigmaN = [| 0.01; 0.02; 0.03 |]; sigmaK = [| 0.0; 0.001; 0.002 |] }`, the
    brief's exact values.
- **`LibraryFacetsTests.fs` is edited although the effects' edits list omits it.**
  - how_to (2) says "Every complete match on `TabulatedTableError` that FS0025 reports maps the new case as it maps the
    other three".
  - `OpticalConstructor.Tests` compiles with `--warnaserror+:25`, and FS0025 is an error in every project here, so the
    build fails without this line.
  - `OpticalConstructor.Tests` is in the step's `touches`.
  - The edit is one pattern line in an existing fact. No test identity is added, renamed or removed, and no assertion
    changes.
- **The validation order inside `withUncertainty`:**
  - The length check comes first, so columns of two values answer `SampleCountMismatchErr` whatever they hold.
  - Then sigma n is scanned, and then sigma k. A reason names its column and the first offending index.
- **The unit fact checks more than the brief's minimum.**
  - It checks that `{ withSigma with uncertaintyOpt = None }` equals the original table, so every other column and
    the interpolation are unchanged.
  - At each sample it compares `complexIndex` both with the sample's n and k (`verifyIndex`) and with the plain
    table's index, through `MatrixComparison.verifyMatrixEqualityEps`.
  - It refuses one short column (sigma k) as well as two short columns.
- **The entry-file fact walks two materials through the same loop:** the table with uncertainties and the same table
  without them. Each material is checked four ways:
  - the decoded `MaterialEntry` equals the original;
  - the provenance is `UserProvenance`;
  - the decoded table equals the original;
  - its `uncertaintyOpt` is `Some { sigmaN; sigmaK }` with the uncertainties and `None` without them.
- **No schema, file or interpolation change.**
  - FSharp.SystemTextJson writes the option as the record's value, or as `null`, inside the payload, which the
    serializer binds (step 30).
  - `git ls-files | xargs grep -l '"interpolation"'` finds no tracked catalogue entry or kit file holding a tabulated
    table, so no committed file changes.

## Testing state

These are development checks. The gate engine runs the step's roster after this session and is the authority. Every
figure below is from the final build, after the ELEV010 repair.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0 in 71 s, against a 600 s deadline, with 0
  errors. The 6 warnings are the vendored MathNet's SYSLIB0051 and the exempt NU advisories, all of which predate this
  step. Log: `.artifacts/0453-dev-build.log`.
- **unit-tests, filtered to `BerremanTests.EpsWithDispValueTests`:** `Passed!`, 26 of 26, in 2 s. That is 25 facts at
  HEAD plus the step's one. Log: `.artifacts/0453-dev-unit-tests.log`.
- **constructor-unit-tests, the whole suite as the gate selects it:** `Passed!`, 1802 of 1802, in 103 s against a
  180 s deadline. That is checkpoint 452's 1801 plus 1. Log: `.artifacts/0453-dev-constructor-unit-tests.log`.
  - `EntryFileJsonTests`, `LibraryFacetsTests` and `MaterialImportTests`, filtered, passed 79 of 79 before the whole
    run. Log: `.artifacts/0453-dev-constructor-classes.log`.
- **ui-smoke, the whole suite:** `Passed!`, 267 of 267, in 225 s against a 600 s deadline. Log:
  `.artifacts/0453-dev-ui-smoke.log`.
- **ui-tests, filtered to `MaterialEditorWindowTests` and `NkDispersionChartTests`:** `Passed!`, 44 of 44, in 3 s. Log:
  `.artifacts/0453-dev-ui-tests.log`.
- **kit-acceptance, filtered to `KitAcceptanceTests`:** `Passed!`, 14 succeeded and 1 skipped (AT-2's five-seed ensemble, xUnit v3 Explicit; the gate's skip policy is `allow`), in 1439 s against a 3600 s deadline. Both `… kit equals the committed kit file by file` facts are among the 14 that succeeded (only the ensemble skipped), and `data/` is untouched. Log: `.artifacts/0453-dev-kit-acceptance.log`.
- **elevation, all 13 projects:**
  - First run, over the how_to's `withUncertainty (sigmaN : float array) (sigmaK : float array)`: `ELEV-GATE: FAILED -
    1 new finding(s)`, ELEV010 at `Dispersion.fs:635`. Log: `.artifacts/0453-dev-elevation-run1.log`.
  - Second run, after the carrier-parameter repair: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 226s`. Log:
    `.artifacts/0453-dev-elevation.log`.
- **The 21 s44 invariants:** each decoded `git grep`, run with `--untracked`, finds nothing.
- **LF check:** `git diff --numstat` equals `git diff --ignore-cr-at-eol --numstat`, so there is no CRLF churn.

## Artifacts

All of these are under `specs/0050/.artifacts/`:

- `0453-dev-build.log`
- `0453-dev-unit-tests.log`
- `0453-dev-constructor-unit-tests.log`
- `0453-dev-constructor-classes.log`
- `0453-dev-ui-tests.log`
- `0453-dev-ui-smoke.log`
- `0453-dev-kit-acceptance.log`
- `0453-dev-elevation-run1.log`
- `0453-dev-elevation.log`

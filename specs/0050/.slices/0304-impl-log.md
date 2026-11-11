# Step 0304 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, AGENTS.md, the touched files and their callers.
- [x] Tests first (red). The test project failed to compile with 35 errors. Each was a missing production symbol
  (`ScanQuantity`, `admits`, `validatedFor`) or the `MeasuredSeries`→`MappedPoints` change
  (`.artifacts/0304-red-build.log`).
- [x] Production code: the fsproj move, `ImportProfiles.fs`, `ExperimentDataLoad.fs` and the two UI files.
- [x] Green: solution build with 0 warnings; constructor-unit-tests at 1514 (1512 + 2).
- [x] Mutation check: four defects in one build, each failing its own fact. The files were restored and their
  SHA-256 verified.
- [x] Final rebuild, then development runs of every gate in `0304.gates`.
- [x] The first elevation run FAILED with one finding: ELEV010 at `ImportProfiles.fs:196`, on `ScanQuantity.admits
  (value : SweptValue) : bool`. The repair is recorded under Decisions. After it came a rebuild and a re-run of every
  gate (see Testing state).

## Files modified

- **`OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`.** The `ExperimentDataProxy.fs` and
  `ExperimentDataLoad.fs` `<Compile>` lines move, in that order, to directly after `ImportProfiles.fs`. Their two old
  comment blocks were deleted with them, so the lines carry no comment. Nothing compiled between the old and new places
  (`MuellerReconstruction.fs`, `MuellerInverse.fs`, `ItemEvaluation.fs`) names either module; two comments in
  `MuellerReconstruction.fs` cite the proxy by name only.
- **`OpticalConstructor.Domain/ImportProfiles.fs`.** It opens `Library` and `Experiments`, and its module doc names
  the new validation. After `MappedPoints` it adds:
  - `ScanQuantity` (`elementId`, `label`, `variable`, `ranges`) with `admits (x : float) : bool`, in the display unit,
    true when some range's `VariableRange.admits` (`MeasuredData.fs`) holds;
  - `ExperimentScan.quantities`: `axes` distinct by `(elementId, variable)` in order of first appearance, each labelled
    by its first axis and ranged by all its axes in order;
  - `MappedPoints.validatedFor detector scan`, the one rule:
    - `SeriesPoints` is step 288's match, moved with its texts, rewrapped by `Result.map SeriesPoints`;
    - `CapturePoints` refuses an ellipsometer, passes under no quantity, ranges every `description.degrees` under one
      quantity, and refuses more quantities;
    - `ObservationPoints` pass;
    - `ColumnPoints` checks row by row: a signal under an ellipsometer, then the cell count against the quantities,
      then cell by cell the unit kind, raw vs physical, and the range.
- **`OpticalConstructor.Domain/ExperimentDataLoad.fs`.**
  - `loadAndValidate` answers `Result<MappedPoints, ExperimentDataError>`. It keeps step 291's reader choice and its two
    refusals, and wraps the read series as `SeriesPoints`. It then answers
    `validatedFor (detectorKindOf library slots) experiment.scan`. Its own scan match is deleted.
  - `ExperimentDataBadge.ofCheck` takes `Result<MappedPoints, ExperimentDataError> option`, and its `Ok` arm is
    `ValidatedDataBadge points.pointCount`.
  - The module doc and the opens follow: `ImportProfiles` is added, and the `validateAgainstExperiment` mention is
    dropped from `MeasuredData`'s open comment.
- **`OpticalConstructor.Ui/SolverHandoffView.fs`.** `validateExperiment` answers `HandoffReady points.pointCount`. Its
  doc names the mapped points and S9.2; the error arms are unchanged.
- **`OpticalConstructor.Ui/TableAndElementRotationView.fs`.** Doc only. `checkExperimentData` and the attach arm,
  which calls it, already pass the outcome to `ofCheck` as it comes, and they compile unchanged against the new type.
- **`OpticalConstructor.Tests/ExperimentDataProxyTests.fs`.**
  - It opens `ImportProfiles`.
  - The three changed facts expect `Result<MappedPoints, …>`. Where they expected the series they now expect
    `Ok (SeriesPoints (IntensityData intensityFixture))`.
  - The badge fact's `validated` is `Ok (SeriesPoints (IntensityData intensityFixture))`.
  - Every assertion is kept.
- **`OpticalConstructor.Tests/ImportProfileTests.fs`.**
  - It opens `Experiments`, and its module doc names the validation.
  - The `captureAt` and `observationOf` fixtures move out of `mapped points count what they hold` to private module
    bindings, so both facts read the same two fixtures. That fact's assertions are unchanged.
  - New fixtures: step 287's plate axes over the `sample` id, `plateScan`, `scanOf`, `turnScan`, `darkLine` and
    `assertValidation`.
  - Two new facts:
    - `a scan's quantities join the axes that vary one slot's variable`
    - `mapped capture rows and column rows are validated against the experiment's scan and detector`

## Decisions

- **Deviation from the slice: `ScanQuantity.admits` takes the display-unit `float`, not a `SweptValue`.**
  - The slice writes `member this.admits (value : SweptValue) : bool`. That signature is an ELEV010 breach: a bare
    `bool` beside an elevated type in one signature, and members are not exempt. It was the first elevation run's one
    new finding.
  - The analyzers win over any document (AGENTS.md; spec 0048 `.manual/002` R6), and the known-findings list may not
    grow.
  - `admits` therefore keeps its name, its owner and its meaning, "some range admits it", but takes `x : float` in the
    quantity's display unit. That is exactly the shape of `VariableRange.admits (x : float) : bool`, which it delegates
    to and which the tree already holds clean.
  - Callers:
    - the Phone ROI arm passes `row.description.degrees` with no re-wrap;
    - the column arm passes `cell.displayValue.value`.
  - The alternatives were rejected:
    - a new two-case result union is a public type the slice's contracts do not name;
    - inlining the `List.exists` at both call sites deletes a member the slice creates.
- **`loadAndValidate` lost its `readByProfile ()` wrapper.** That unit-taking closure existed only because the deleted
  scan match called it from two arms. The reader choice and both refusal texts are byte-for-byte step 291's, and both
  refusals still read nothing: the `readsNothing` proxy of the changed fact throws if read.
- **Step 287's plate scan is rebuilt test-locally in `ImportProfileTests`.** Its axes are private to
  `ExperimentProxyTests`, which this step may not edit. The rebuilt axes have the same element id, label, variables,
  ranges and points.
- **The `Lab scan` rows are test-local, as the slice says.** Step 302's `labScanRows` is private to
  `LabFileImportTests`, which compiles after this file and is outside this step's edits.
  - The rows have the same keys, signals and σ, at 0° and 20°.
  - `labScanIn` builds them with a scan column of a given unit and reading.
  - The same two rows therefore cover a raw-dial column and a nanometre column.
- **Order of the column checks.**
  - The rows are checked in order, and the first mismatch wins.
  - Within a row the order is: a signal under an ellipsometer, then the cell count, then cell by cell. For each cell
    the unit kind comes first (angle vs spectral), then raw vs physical, then the range.
  - A unit or reading mismatch is a property of the profile, so it is reported before any value is ranged.
- **Texts the slice leaves open:**
  - an angle on a wavelength is `column <n> holds an angle (<unit>), and <label> <variable> is a wavelength`;
  - a spectral column on an angle is `… holds a spectral value (<unit>), and … is an angle`;
  - a raw dial reading on a physical quantity is `… holds a raw dial reading, and … is a physical value`;
  - a physical column on a dial is `… holds a physical value, and … is a raw dial reading`;
  - the range text is `a scan value <v> <unitLabel> lies outside every range of <label> <variable>`, in the shape of
    `validateAgainstExperiment`'s `<x> <unit>`.

  The units render through `ScanColumnUnit.text` (ELEV002).
- **No helper outside the slice's contracts.**
  - Each check is a lambda inside `validatedFor`, so nothing new is a module-level function of one domain type
    (ELEV013).
  - The quantity's name (`<label> <variable.label>`) is a local string, not a new public member on `ScanQuantity`.
- **The column arm's range guard reads `cell.displayValue.value`.** `VariableRange.admits` takes the display-unit
  float, so this is ELEV004, which the gate excludes (spec 0048 `.manual/002` R7).
  - The compiler copies the `when` guard into each arm of the match's decision tree. The analyzer counts each copy, so
    the suppressed total rose from 1408 (step 303) to 1412: four ELEV004 at `ImportProfiles.fs:277`.

## Testing state

These are development runs of each gate's own command over the final build, after the ELEV010 repair:

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 56 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded; exit 0 | 322 s | 600 s |
| constructor-unit-tests | Passed!, 1514 of 1514, 0 failed, 0 skipped | 72 s | 180 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 230 s | 600 s |
| ui-tests (InverseConstructorTests, SolverHandoffWindowTests) | Passed!, 23 of 23, 0 failed, 0 skipped | 28 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS`, exit 0 | 0.2 s each | 300 s |

- **The first elevation run** (201 s, before the repair) failed with exactly one new finding:
  `FAILED: ELEV010 …/ImportProfiles.fs:196:21 - admits mixes bare primitive(s) bool with elevated types in one
  signature`. Every other gate was green in that round too: constructor 1514, ui-smoke 251 in 224 s, ui-tests 23.
- **constructor-unit-tests rose from 1512 to 1514**, by the two new facts. All five identities the slice names are
  `Passed` in the TRX under their exact names.
- **ui-tests ran over the two selected classes.** Neither file was edited, so the count is unchanged by construction.
  The checkpoints record no `ui_tests` baseline yet; `ui_smoke_tests` was 251 at checkpoint 296.
- **The mutation check ran before the ELEV010 repair.** The repair changed only `admits`' parameter type and its two
  call sites. None of those is a mutated line, and both facts that exercise `admits` passed again after it.
- No run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0304-red-build.log`: the red build.
- `specs/0050/.artifacts/0304-mutation/`: `mutate.py`, the mutated build and test logs, and the SHA-256 before and
  after the restore.
- `specs/0050/.artifacts/0304-probe/degrees.fsx`: the probe that 0°, 20° and 45° round-trip through `Angle` exactly,
  so the facts can assert exact texts.
- The development gate runs:
  - `specs/0050/.artifacts/0304-build.log`
  - `0304-constructor-unit-tests.log`
  - `0304-ui-smoke.log`
  - `0304-ui-tests.log`
  - `0304-elevation.log`
  - `0304-invariants.log`

## Gotchas

- **F# lambdas take no return-type annotation** (`fun (x : T) : R -> …` is FS0010). Annotate a `let` inside the lambda
  instead.
- **Bash heredocs fail in this harness on text with apostrophes and backticks** ("unexpected EOF while looking for
  matching `''"), and nothing is written. Use the Write or Edit tools for F# text.
- **`grep -c $'\r'` counts every line in this Git Bash**, so it cannot detect CRLF. Use `git ls-files --eol`, or count
  `b'\r'` in Python. Every touched file is `i/lf w/lf`, with zero CR bytes.
- **A cold build of the test project prints two `SYSLIB0051` warnings** in the vendored, untouched
  `MathNetNumerics/Numerics/Exceptions.cs`. The solution build reports 0 warnings.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

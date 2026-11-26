# Step 0304 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Steps 298 to 303 added the profiles, `MappedPoints` and one read of a lab file through any profile
  (`ChosenImportProfile.mapText`).
- This step validates mapped points against an experiment, by one rule: `MappedPoints.validatedFor detector scan`.
  It checks against the scan's quantities (`ExperimentScan.quantities`, one per slot variable) and the detector.
- The data seam (`loadAndValidate`, `ExperimentDataBadge.ofCheck`, the solver hand-off) now carries mapped points
  instead of a bare series.
- `loadAndValidate` still reads only the two X,Y built-ins. Reading the other profiles through the seam belongs to later
  S9.2 steps.

## What's working

- Add `ScanQuantity` and `ExperimentScan.quantities`: one quantity per slot variable, ranged by every axis that varies
  it (S9.2).
- Add `MappedPoints.validatedFor`, the one validation of mapped points (S9.2):
  - X,Y series are checked by step 288's moved rule;
  - Phone ROI rows by detector and range;
  - mapped rows by units, raw dial vs physical, and range;
  - observations pass.
- `loadAndValidate` and `ExperimentDataBadge.ofCheck` carry `MappedPoints`. The hand-off answers the points' count.
- Move `ExperimentDataProxy.fs` and `ExperimentDataLoad.fs` to compile after `ImportProfiles.fs`.
- Add two `ImportProfileTests` facts; constructor-unit-tests rises by 2, to 1514.

## Tests

These were development runs of each gate's own command over the final build, after the ELEV010 repair (see
Architecture). The arc-runner's gate engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 56 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded; exit 0 | 322 s | 600 s |
| constructor-unit-tests | Passed!, 1514 of 1514, 0 failed, 0 skipped | 72 s | 180 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 230 s | 600 s |
| ui-tests (InverseConstructorTests, SolverHandoffWindowTests) | Passed!, 23 of 23, 0 failed, 0 skipped | 28 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | 0.2 s each | 300 s |

- **constructor-unit-tests: 1514**, the 1512 baseline plus the 2 new facts. All five identities the slice names are
  `Passed` in the TRX under their exact names:
  - `ImportProfileTests.a scan's quantities join the axes that vary one slot's variable` (new)
  - `ImportProfileTests.mapped capture rows and column rows are validated against the experiment's scan and detector`
    (new)
  - `ExperimentDataProxyTests.the data badge follows the attachment, its file's hash and its validation` (changed)
  - `ExperimentDataProxyTests.loading data validates a one-axis scan as before and refuses an X,Y file for a
    multi-variable scan` (changed)
  - `ExperimentDataProxyTests.loading data reads it through its import profile and refuses a profile or a row
    selection the X,Y files cannot serve` (changed)
- **ui-tests** ran over `InverseConstructorTests` and `SolverHandoffWindowTests` only, as the step selects. All 23
  passed. Neither class was edited, so its count is unchanged.
- **Elevation, first run: FAILED** (201 s), with one new finding: ELEV010 on
  `ScanQuantity.admits (value : SweptValue) : bool`, a bare `bool` beside an elevated type. After the repair the
  solution was rebuilt, and every gate above was re-run.
- **Red first.** Before the production code, the test project failed to compile. Each error was a missing production
  symbol or the `MeasuredSeries`→`MappedPoints` type change.
- **Mutation check.** Four defects went into one build, and each failed exactly its own fact:
  - quantities keyed by variable alone;
  - a multi-variable scan accepted for an X,Y file;
  - a raw dial column accepted on R1;
  - a validated badge with 0 points.

  Both files were restored byte for byte (SHA-256 checked) and the solution rebuilt. The check ran before the ELEV010
  repair, which touched only `admits`' parameter type and its two call sites, none of them a mutated line.

```yaml
gates:
  constructor_unit_tests: 1514
  ui_smoke_tests: 251
  ui_tests: 23
```

## Architecture

- **One validation rule, on the points.** `MappedPoints.validatedFor` lives in `ImportProfiles.fs` beside the points it
  checks, and every caller goes through it.
  - Step 288's X,Y scan rule moved there from `loadAndValidate`, with its texts.
  - `validateAgainstExperiment` (`MeasuredData.fs`) is still the series check it calls.
  - `VariableRange.admits` is still the one range test: `ScanQuantity.admits` asks whether any of its ranges admits a
    value.
- **Recorded deviation: `ScanQuantity.admits (x : float) : bool`, not `(value : SweptValue)`.**
  - The slice's signature breaches ELEV010: a bare `bool` beside an elevated type, and members are not exempt.
  - The analyzers win over the slice text (spec 0048 `.manual/002` R6).
  - The member keeps its name, owner and meaning. It takes the display-unit `float`, exactly as
    `VariableRange.admits (x : float) : bool` does.
  - A new result union, or deleting the member, would have changed the surface more.
- **A quantity is a slot's variable.** `ExperimentScan.quantities` joins every axis with the same `(elementId,
  variable)`, so a plate's azimuth over A1's and A2's blocks is one quantity with two ranges (001 §Q AT-2). It is
  labelled by its first axis.
- **A kit profile's scan columns map to the quantities by position.** The i-th scan cell is the i-th quantity's:
  - an angle column fits angles;
  - a spectral column fits a wavelength;
  - a raw dial reading fits only a dial quantity (001 §I).
- **Compile order.** `ExperimentDataProxy.fs` and `ExperimentDataLoad.fs` now compile directly after
  `ImportProfiles.fs`, so the data seam can name `MappedPoints`. Nothing between their old and new places uses them.
- **The UI follows the type.**
  - `SolverHandoffView.validateExperiment` answers the points' count.
  - `checkExperimentData` passes the outcome to `ofCheck` unchanged; only its doc changed.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **Step 287's plate scan and step 302's `Lab scan` rows are test-local copies in `ImportProfileTests`.** Their owners
  (`ExperimentProxyTests`, `LabFileImportTests`) keep them private and are outside this step's edits. The copies have
  the same ids, labels, ranges, points, keys and readings.
- **A Phone ROI file's description is ranged against one quantity only.** Under a scan of two or more quantities it is
  refused (`DataRangeMismatchErr`), never matched to one of them. Under a dark line it passes.
- **Column rows report the first mismatch.** Rows are checked in order. Within a row: a signal under an ellipsometer,
  then the cell count, then cell by cell the unit kind, raw vs physical, and the range.
- **A member like `admits (value : <our type>) : bool` is ELEV010, even on a record.** Keep a predicate's signature
  all-primitive (as `VariableRange.admits` is) or all-elevated.
- **The column arm's range guard reads `cell.displayValue.value`.** That is ELEV004, measured and not gated (spec 0048
  `.manual/002` R7). The compiler copies the `when` guard into each decision-tree arm and each copy is counted, so the
  suppressed total went from 1408 to 1412.
- **F# lambdas take no return-type annotation** (FS0010). Annotate a `let` inside the lambda instead.
- **Bash heredocs fail in this harness on F# text with apostrophes**, and nothing is written. Use the Write or Edit
  tools.
- **`grep -c $'\r'` counts every line in this Git Bash.** Check line endings with `git ls-files --eol`; every touched
  file is `i/lf w/lf`.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0304: mapped points validated against the experiment's scan and detector (S9.2).
  - `ImportProfiles.fs` gains `ScanQuantity`, `ExperimentScan.quantities` and `MappedPoints.validatedFor`.
  - `admits` takes a display-unit `float`, per ELEV010.
  - `loadAndValidate`, `ExperimentDataBadge.ofCheck` and the solver hand-off carry `MappedPoints`.
  - The data seam compiles after `ImportProfiles.fs`.
  - `ImportProfileTests` gains two facts; constructor-unit-tests rises by two, to 1514.

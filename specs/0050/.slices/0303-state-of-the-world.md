# Step 0303 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Steps 298 to 300 added the closed sets, the kit and built-in profiles, `ChosenImportProfile` and `MappedPoints`.
- Step 301 added the Mueller-matrix observation file.
- Step 302 added a kit profile's column mapping.
- This step adds one read of a lab file through any profile, `ChosenImportProfile.mapText`. Each built-in calls its
  existing parser (001 §K "Built-in profiles REUSE the existing parsers"; 001 §R), and a kit profile calls its column
  mapping.
- Nothing calls `ChosenImportProfile.mapText` yet. Later S9.2 steps attach the mapped points to experiments.

## What's working

- Add `ChosenImportProfile.mapText` (S9.2): one read of a lab file through any profile, answering `MappedPoints`.
- The built-ins call their parsers unchanged: `Intensity series` and `Ellipsometric series` read through
  `MeasuredData`'s parsers, `Phone ROI CSV` through `parseMuellerCsv` and `Mueller-matrix observations` through
  step 301's reader. A kit profile reads through step 302's column mapping.
- Map Phone ROI's `MuellerDataError` onto the data channel: `MalformedRowErr` becomes `MalformedDataFileErr` and
  `EmptyFileErr` becomes `EmptyDataFileErr`.
- Rewrite `MeasuredData.fs`'s module doc: its two parsers are the bodies of the `Intensity series` and `Ellipsometric
  series` import profiles.
- Add three facts to `LabFileImportTests`; constructor-unit-tests rises by 3, to 1512.

## Tests

These were development runs of each gate's own command over the final build. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 62 s after the edits; 9 s final incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded; exit 0 | 227 s | 600 s |
| constructor-unit-tests | Passed!, 1512 of 1512, 0 failed, 0 skipped | 76 s | 180 s |
| ui-tests (FsxAcceptanceTests) | Passed!, 14 of 14, 0 failed, 0 skipped | 96 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | < 1 s each | 300 s |

- **constructor-unit-tests: 1512**, the 1509 baseline plus the 3 new facts. Each new identity is `Passed` in the TRX,
  named exactly as the slice names it:
  - `each built-in profile reads its fixture exactly as its parser did`
  - `a kit profile reads through its column mapping`
  - `the Phone ROI profile maps the parser's errors onto the data channel`
- **ui-tests** ran over `OpticalConstructor.Ui.Tests.FsxAcceptanceTests` only, as the step selects. All 14 passed, the
  same count as step 302, and that class was not edited. The constructor/export numerical parity with the Analytics
  examples holds.
- **Red first.** Before the production code, the test project failed to compile with exactly one `FS0039`, for the
  missing `ChosenImportProfile.mapText`.
- **Mutation check.** Three defects in one build each failed their own fact:
  - the intensity profile reversed its points;
  - `EmptyFileErr` was mapped to `MalformedDataFileErr`;
  - the kit profile reversed its rows.

  The file was restored byte for byte (SHA-256 checked) and the solution rebuilt before the runs above.

```yaml
gates:
  constructor_unit_tests: 1512
  ui_tests: 14
```

## Architecture

- **`ChosenImportProfile.mapText` is an extension member in `OpticalConstructor.Storage/LabFileImport.fs`**, beside
  step 301's observation reader and step 302's `ImportProfile.mapText`, and it calls both. It lives in Storage because
  `parseMuellerCsv` is Storage's (`MuellerDataStore`), and Storage can see Domain's parsers.
- **Every parser is called, never copied.** Each case is the parser's `Result` with `Result.map` into its
  `MappedPoints` case:
  - `IntensityData >> SeriesPoints`
  - `EllipsometricData >> SeriesPoints`
  - `CapturePoints`
  - `ObservationPoints`
  - `ColumnPoints`

  Each parser's rules, diagnostics and numbers are therefore exactly its own. No parser throws (`parseMuellerCsv` and
  step 302's CSV read catch FSharp.Data's exceptions themselves), so no exception crosses the member.
- **One error channel.**
  - The X,Y parsers, the observation reader and the kit mapping already answer `ExperimentDataError`.
  - Phone ROI's `MuellerDataError` is mapped by a lambda inside the member: `MalformedRowErr r` and
    `ArchiveUnreadableErr (_, r)` become `MalformedDataFileErr r`, `EmptyFileErr r` becomes `EmptyDataFileErr r`, and
    `DataSetMissingErr (_, archive)` becomes `MalformedDataFileErr` of the archive's path.
  - The two archive cases are unreachable from a text parse. They are mapped so the match is total.
- **`MeasuredData.fs` is unchanged in code.** Only its module doc changed, to point at the profiles its two parsers
  serve.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **`ChosenImportProfile.mapText` and `ImportProfile.mapText` share a name on different receivers.** ELEV019 compares
  extension members per receiver, so this is clean, and the elevation run confirms it.
- **The Phone ROI archive cases' `archive.value` is one ELEV004 finding.** It is excluded in the gate, measured and not
  enforced, which moves the suppressed total from 1407 to 1408.
- **`Assert.Equal` over an F# `Result` prints only the type names on a mismatch.** `LabFileImportTests.assertReads`
  compares with `=` and prints both reads with `%A`.
- **Step 302's lab-scan fixture is now two private bindings**, `labScanText` and `labScanRows`, read by both step
  302's fact and this step's kit fact. Step 302's assertion is unchanged.
- **Backslash escapes do not survive a bash heredoc in this harness.** Use the Write or Edit tools for text that
  carries `\n` escapes.
- **A cold build of a test project reports two `SYSLIB0051` warnings** in the vendored, untouched
  `MathNetNumerics/Numerics/Exceptions.cs`. The solution build after this step's change reports 0 warnings.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0303: one read of a lab file through any profile (S9.2).
  - `LabFileImport.fs` gains `ChosenImportProfile.mapText`, over the four built-in parsers and the kit column mapping.
  - `MeasuredData.fs`'s module doc names its parsers as the X,Y built-in profiles' bodies.
  - `LabFileImportTests` gains three facts; constructor-unit-tests rises by three, to 1512.

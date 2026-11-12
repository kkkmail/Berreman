# Step 0313 — impl log

## Progress

- Read the task, the two system prompts, the project prompt, AGENTS.md and the slice.
- Read the code the slice touches:
  - `Kit.fs` (`MappedAttachment`) and `Experiments.fs` (`Experiment`, `commit`, `edit`, `withStoredExperiment`);
  - `experiment.schema.json` and `SchemaValidation`, plus `kit.schema.json`'s local `#/$defs` refs as precedent;
  - `KitContentFiles.copyExperimentsFrom`;
  - the five facts the slice creates or changes.
- Checked that every new name is free across Berreman and STL: `MappedPointNumber`, `ExclusionReason`,
  `PointExclusion`, `CorrectionNote`, `SetupCorrection` and the three members.
- **Domain, `Kit.fs`.** Added `MappedPointNumber`, `ExclusionReason` and `PointExclusion` directly before
  `MappedAttachment`. `MappedAttachment` gained `exclusions` and the members `withExclusion`, `withoutExclusion` and
  `tryFindExclusion`.
- **Domain, `Experiments.fs`.** Added `CorrectionNote` and `SetupCorrection` after `ExperimentNotes`. `Experiment`
  gained `corrections` after `dataOpt`. `commit` appends with `corrections = []`; its edit arm already copies with
  `{ e with … }`, so it keeps them. `commit`'s doc now says so.
- **Storage.**
  - `experiment.schema.json`: `dataOpt` requires `exclusions` and `experiment` requires `corrections`.
  - The setup-reference object moved to a local `$defs/setupReference`. `setup`, `previous` and `corrected` all
    `$ref` it, so the shape is stated once.
  - `KitContentFiles.fs`: doc only. The copy's `{ … with }` updates already keep both fields as written.
- **Ui, `TableAndElementRotationView.fs`.** The `AttachDataFile` arm sets `exclusions = []`. The `chartForRun`
  scan-description `Experiment` literal sets `corrections = []`, a compile repair only.
- **Fixtures.** Two scripts inserted the new fields into full record literals only, never into `{ x with … }` copies
  (`.artifacts/0313-scripts/add_fields.py`, `add_after_block.py`). The `})`-closed and single-line literals were done
  by hand.
- **First build: one error.** `ExperimentCopyTests.fs(675)`, the `expected` attachment of the copy fact the slice
  changes. Ui.Tests already compiled, so no Ui.Tests literal was missed.
- **Wrote the two new facts and the three changed ones.** The second build was green.
- Ran every gate's own command over the final tree; see Testing state.
- **Mutation checks of the schema refusals,** each on the build-copied schema in the test output only. The copy was
  restored byte-identical to the source schema (`cmp`) after each run:
  1. Dropping the `corrections` / `exclusions` / `note` requirements, the point minimum and the reason pattern: the
     refusal fact fails. The missing `corrections` reaches the binder and fails as `MalformedJsonErr`.
  2. Dropping only `note`, the point minimum and the reason pattern: the fact fails with `accepted a schema-invalid
     experiment file`.
  3. After restoring, `KitContentFileJsonTests` passes again, 14 of 14.

## Files modified

- `OpticalConstructor.Domain/Kit.fs`:
  - `MappedPointNumber` (`.value`; `tryCreate` answers `None` below 1);
  - `ExclusionReason` (`.value`; `tryCreate` answers `None` for a blank text);
  - `PointExclusion = { point; reason }`;
  - `MappedAttachment.exclusions`, `withExclusion`, `withoutExclusion` and `tryFindExclusion`.
- `OpticalConstructor.Domain/Experiments.fs`:
  - `CorrectionNote` (`.value`; `tryCreate` answers `None` for a blank text);
  - `SetupCorrection = { previous; corrected; note }`;
  - `Experiment.corrections`;
  - `commit` appends with `corrections = []`, and its doc changed.
- `OpticalConstructor.Storage/schema/experiment.schema.json`: as above; schema version 1 kept.
- `OpticalConstructor.Storage/KitContentFiles.fs`: the `copyExperimentsFrom` doc, which now cites S9.3.
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`: `exclusions = []` in `AttachDataFile`, and
  `corrections = []` in `chartForRun`'s description literal.
- Tests named in the slice:
  - `KitTests`: the new fact.
  - `ExperimentProxyTests`: the new fact, plus `corrections = []` / `exclusions = []` in six fixtures.
  - `KitContentFileJsonTests`: the fixture `e1Corrected`; `editedFileOf experiment edit`, of which
    `editedExperimentFile` is now the partial application over E1; the round-trip and refusal facts extended.
  - `ExperimentCopyTests`: `darkLine` fixture fields; the copy fact extended.
  - Fixture fields only: `ExperimentCollectionRoundTripTests`, `KitLibraryTests`, `OutOfBandDiagnosticTests`,
    `SetupTests`, `KitWindowModelTests`, `ExperimentDataProxyTests`, `ExperimentDataStoreTests`,
    `SolverHandoffWindowTests` and `KitWindowTests`.
  - `SetupModelTests`, `ExperimentModelTests`, `ScrollExtentTests` and `InverseConstructorTests` build no literal of
    either record, so they needed no change.
- Tests outside the slice's edit list, compile repairs only (`corrections = []` in one `Experiment` literal each):
  `KitOpeningTests`, `KitStartTests`, `KitStartWindowTests` and `LauncherTests`. See Gotchas.

## Testing state

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, `0 Warning(s)`, `0 Error(s)` (incremental); the first full build's only warnings are the 2 pre-existing vendored MathNet SYSLIB0051 | 78 s full, 22 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, suppressed 1428 (unchanged from step 311) | 200 s | 600 s |
| constructor-unit-tests | Passed!, 1527 of 1527 (1525 + 2) | 70 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 217 s | 600 s |
| ui-tests (selected: `ExperimentControlsTests`, `InverseConstructorTests`, `KitWindowTests`, `ScrollExtentTests`, `SolverHandoffWindowTests`) | Passed!, 97 of 97 | 41 s | 360 s |
| invariant-s31-\* (16) | each exit 0, `INVARIANT HOLDS` | under 1 s each | 300 s |

- The TRX shows each fact named in the slice as `Passed`: the two new ones and the three changed ones.
- No run exceeded its effective deadline. `commit_ready: true`.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0313-dev-build-1.log`: the one-error first build. `0313-dev-build-2.log`: the green build.
- `0313-dev-constructor-unit-tests.log`, `0313-dev-ui-tests.log`, `0313-dev-ui-smoke.log`,
  `0313-dev-elevation.log` and `0313-dev-invariants.log`.
- `0313-dev-mutation-loose-schema.log` and `0313-dev-mutation-loose-exclusion.log`: the two mutation runs.
- `0313-dev-kitcontentfilejson-restored.log`: the re-run after restoring the schema.
- `0313-scripts/`: the two fixture scripts, the invariant command list, and the bin-schema backup used to restore the
  copy.

## Gotchas

- **Four unlisted fixtures were edited.** `KitOpeningTests`, `KitStartTests`, `KitStartWindowTests` and
  `LauncherTests` each build an `Experiment` literal, so the build needs `corrections = []` in them.
  - The slice's rule (6) is "every fixture that builds an `Experiment` sets `corrections = []`". Its file list missed
    these four.
  - Steps 261, 288 and 291 re-pointed the same four files under the same kind of rule, recording them as compile
    repairs.
  - Each gets exactly one line and keeps every assertion.
- **`chartForRun`'s description literal also gets `corrections = []`.** The slice says "nothing else in the window
  changes", but that function builds a throwaway `Experiment` only to call `.description`, and the record needs the
  field to compile. Its behaviour is unchanged.
- **A copy keeps corrections exactly as written.** `copyExperimentsFrom` copies only the setups an experiment's
  current `setup` references.
  - A correction's `previous` setup, if it is another setup, is not copied.
  - If the copy renames a clashing setup, a correction naming it keeps the old name.
  - This is what the slice prescribes ("keeps each copied experiment's exclusions and corrections as written").
- **Point order sorts on `MappedPointNumber` itself** (structural comparison over its int), never on `.value`. This
  adds no ELEV004 read; the suppressed count is unchanged at 1428.
- **The `e1Corrected` fixture's `setup` is the correction's `corrected` reference,** so the file is coherent. The
  slice names only the exclusion and the correction.
- **FSharp.SystemTextJson names the wrong field when a list field is missing.** With `corrections` absent and no
  schema requirement, the binder reported `Missing field … sessionOpt`. The schema requirement is what gives the
  typed `SchemaValidationErr`.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

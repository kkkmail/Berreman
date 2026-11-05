# Step 0013 — IMPLEMENT — impl log

## Progress

- [x] Record change (`MaterialLibrary.fs`): `complexity : MaterialComplexity` is mandatory, and
  the `properties` field is gone. `member this.properties = this.complexity.toProperties`. The
  built-ins drop `properties = …` and `Some`. The record now has structural equality.
- [x] Constructions:
  - Storage: `MaterialImport.entryFromTabulated` / `entryOfAxis` and `Report.dtoToEntry`.
  - Ui: `MaterialEditorView.entryUnder`.
  - Fixtures: `LibraryFacetsTests.materialEntryWith`, `OutOfBandDiagnosticTests.entryOf`,
    `OutOfBandBadgeTests.dispersiveMaterial`, `DispersionModelsTests` (the minted-id fact),
    `FsxAcceptanceTests.material` and `MaterialEditorWindowTests` (the published-gyration
    fact).
  - The copy-and-updates in `MaterialProxyTests.glassPhysicsChanged` and `SampleProxyTests`
    keep only `complexity = …`.
- [x] Readers of the option:
  - `MaterialStore.payloadOf` is `VersionPayload<MaterialComplexity, MaterialMetadata>`, and the
    `MaterialMetadata` doc lost its coded-preset sentences.
  - `LibraryFacets`: the four private entry members and the anisotropy facet read the value,
    and the defensive-`None` sentence is gone.
  - `OutOfBandDiagnostic.ofEntry` is `definedSegmentsOf entry.complexity`.
  - `NkDispersionChart.previewAnisotropy` is deleted, together with the `MaterialEntry`
    extension block that held only it. The `allGyrationComponents`,
    `gyrationComponentsOfComplexity`, `gyrationChart` and `muKind` docs now say every component
    is drawn when a complexity names no gyration class.
  - `MaterialsWindowView.editableSelection` and the view panel read
    `entry.complexity.isEditable` and `entry.complexity.anisotropy`. The caption is
    ` (view-only)`.
  - The `MaterialComplexity.isEditable` doc no longer contrasts the gate with complexity
    presence.
- [x] Editor (`MaterialEditorView.fs`):
  - The case is `ViewOnlyMaterial of reason : ControlCaption * shown : MaterialComplexity`, and
    the `presetProperties` and `viewOnlyComplexity` fields are deleted.
  - `previewProperties`, `previewGyrationComponents` and `previewMuKind` read `shown`.
  - `initMaterialEditor` seeds an editable entry through `ofComplexity`, whose `Error e` gives
    `ViewOnlyMaterial (e.caption, entry.complexity)`. Every other entry gives
    `ViewOnlyMaterial (ControlCaption "this entry's physics is not editable term data — view-only", entry.complexity)`.
  - The Save and view binders take `(reason, _)`, and `complexitySummary` says `view-only`.
  - The module, `EditorMode` and model docs, the init comments and the
    `MaterialEditorWindow.fs` doc are rewritten.
- [x] Tests:
  - Deleted `every built-in carries Some complexity after the step-004 re-seed` and
    `spec 0040 006: a coded preset with no stored tree still renders all components`.
  - Renamed the three facts, as the slice names them.
  - The option readers read the value (AC-B7, the MaterialImport step-1/2 helpers and formula
    facts, `anisotropyOf …`, the coded-preset out-of-band fact, `ofComplexity round-trips …`,
    `init seeds …` and the Save facts).
  - Every `complexity = None/Some` comment and failure message in the edited files is rewritten.
- [x] Local observation: build, constructor-unit-tests, ui-smoke, ui-tests (step filter),
  elevation and the s02 invariants.
- [x] **Attempt 2.** The gate engine failed `constructor-unit-tests` with
  `constructor_unit_tests count 736 < baseline 737`. Diagnosed as an authority conflict (see
  Testing state). No product or test file was edited in this attempt.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OutOfBandDiagnostic.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs` (doc only)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/Report.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/NkDispersionChart.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialEditorWindow.fs` (doc only)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryFacetsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialComplexityTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OutOfBandDiagnosticTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SampleProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExportImportTests.fs`. This file is
  **not** in the slice's edit list; see Decisions.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/FsxAcceptanceTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OutOfBandBadgeTests.fs`

## Decisions

- **Deviation: `ExportImportTests.fs:159` was edited although the slice does not list it.** The
  fact `a metadata-only library file re-imports each entry as vacuum data` asserted
  `Assert.Equal(Some vacuumPlaceholderComplexity, e.complexity)`. Once the field is no longer an
  option, that line no longer type-checks, so the build (and with it every gate) would be red.
  The repair drops the `Some` and nothing else, and the fact's subject is unchanged. It is a
  consumer repair forced by the authorized `MaterialEntry.complexity change_shape` contract
  ("the record and all its consumers change in this one step, so the build stays green").
- **`init seeds …` (change_expectation).** The view-only leg now also asserts that the case
  carries the complexity it shows: `ViewOnlyMaterial (_, shown)` with
  `shown = siliconEntry.complexity`. That is the new shape's observable. The glass leg compares
  `Ok glass.complexity` directly.
- **`Save UPDATES an existing editable entry in place …` (ui-smoke).** Its `Some _` match no
  longer compiled. It now asserts that the metadata-only edit kept the complexity unchanged
  (`existing.complexity = updated.complexity`). This is the "reads the value" rule applied to
  the third Save fact, and its name is unchanged.
- **`MaterialEditorModel` stays `[<ReferenceEquality>]`.** Its last function-carrying field
  (`presetProperties`) is gone, but structural equality would change which `update` results the
  Elmish gate treats as a change. The doc now gives the reference-compare reason without the
  deleted field.
- **`MaterialEntry.dispersion` binds `this.properties` once** instead of reading the member
  three times. Each read is now a `toProperties` composition.
- **Small doc fixes in edited files.** These follow the slice's rule that the view-only case is
  "a complexity that is not editable term data":
  - `MaterialEditSnapshot` and `previewPane` no longer say "preset preview" / "view-only
    preset".
  - The `verbsRow` doc says "view-only entry".
  - The Langasite fact's comment says published laws, not "an opaque evaluated closure".
  - The `MaterialImport` docs drop the "`properties` IS `complexity.toProperties` (the seed
    invariant)" clause, because construction no longer sets `properties`.

## Testing state

These are local observations. The arc-runner's gate engine runs and records the gates.

- `build`: exit 0, 0 errors. The only warnings are the two vendored MathNet SYSLIB0051 ones that
  were already on the start tree (`.artifacts/0013-build.log`).
- `constructor-unit-tests` (whole suite): **736/736**. That is 737 at step 10's checkpoint, less
  the deleted `every built-in carries Some complexity after the step-004 re-seed`
  (`.artifacts/0013-constructor-unit-tests.log`, TRX under
  `.artifacts/0013-constructor-unit-tests-results/`).
- `ui-smoke` (whole suite): **208/208**, unchanged. The TRX records both renamed facts as Passed:
  `acceptance: Save round-trips a NEW entry through MaterialProxy with its complexity` and
  `a non-editable entry opens view-only with no Save affordance and no ladder`
  (`.artifacts/0013-ui-smoke.log`).
- `ui-tests` (the step's five-class filter): **93/93**, which is 94 less the deleted
  `spec 0040 006: a coded preset with no stored tree still renders all components`. The TRX
  records `Save adds a NEW material with its complexity, updates an EXISTING one, and Cancel
  writes nothing` and `init seeds …` as Passed (`.artifacts/0013-ui-tests.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 8 project(s), 149s` (3 changed projects,
  expanded to 8 with their dependents; ELEV004 excluded) (`.artifacts/0013-elevation.log`).
- s02 invariants: `elev-list-empty`, `no-vstest`, `no-kit-kit-json` and `no-eps-axis-evaluated`
  each find nothing (`.artifacts/0013-invariants.log`).
- LF check: `git diff --numstat` equals `--ignore-cr-at-eol`, and every changed file is `w/lf`.

### Attempt 2: authority conflict on the `constructor-unit-tests` floor (commit_ready: false)

**Measured finding** (gate engine, 14:50:12, tree `969e23357ffd`): `constructor-unit-tests`
reported `Test run summary: Passed!`, total 736, succeeded 736, failed 0. It still failed with
`constructor_unit_tests count 736 < baseline 737`. Build, review (green) and elevation had
passed. The gate short-circuited there, so ui-smoke, ui-tests and the invariants did not run.

**Diagnosis.** The drop is exactly the deletion this step authorizes:

- **The only fact that left is the deleted one.** Against `HEAD`, the eight edited
  `OpticalConstructor.Tests` files keep their `[<Fact>]` / `[<Theory>]` counts, except
  `MaterialComplexityTests.fs`, which goes from 11 to 10. The single removed test name is
  ``every built-in carries Some complexity after the step-004 re-seed``.
- **737 is the floor from before this step.** It is checkpoint key `"10"` (slice 0011). Slice
  0012 (key `"11"`) did not run this gate and did not touch `OpticalConstructor.Tests`, so the
  suite entered this step at 737. 736 is 737 less the one authorized delete.
- **The engine accepted the identities.** In `evaluate_gate`, `check_observed_tests` runs before
  the count. It passed, so the 736 observed identities equal the accepted post-state census
  exactly: the deleted fact is absent, and nothing else is missing or unexpected.

**Why the floor did not drop.** The engine credits an authorized deletion in
`ai_strategy_generator/executability/runtime_tests.py:authorized_count_credits` (lines 80–110),
which `evaluate_gate.py:408-412` applies to the baseline. That function skips every suite whose
census `granularity` is not `"row"`. This arc's `.repository-inventory-json` records
`constructor-unit-tests` as `"granularity": "method"`, so the credit for
`effects.tests[0]` (`op: delete`) is 0 and the floor stays 737. The older waiver
(`run_arc_runner.py:3889-3901`, `BASELINE-WAIVED`) needs a `DELETE_TEST` entry under `actions`
and no executable profile. This step has no `actions`, and the arc has a profile. So no path
lowers the floor for this deletion.

**Conflicting obligations.**

- The step's own effect `{"op": "delete", "suite": "constructor-unit-tests", "id": "…every
  built-in carries Some complexity after the step-004 re-seed", "obligations": ["S12-MANDATORY"]}`.
- Its acceptance: "constructor-unit-tests MUST pass, one fact fewer for the deleted
  MaterialComplexityTests fact".
- Against both: the `count_at_least` floor `constructor_unit_tests = 737`.

The slice requires 736. The gate requires at least 737.

**Missing authority.** Every edit that would turn the gate green is outside this worker's scope:

- **Lowering the floor** means writing `specs/0050/.checkpoints-json`. The arc-runner maintains
  that file, and the project prompt says "You never author baselines yourself".
- **Crediting method-granularity deletions** is a change to ASG, which is read-only from here.
- **Declaring the deletion another way** (an `actions: [DELETE_TEST …]` line, or an operator
  test effect) means editing the published slice, `.manual/` or `.operator-test-effects-jsonl`.
  Those belong to the operator.
- **Padding the suite to 737** is ruled out:
  - an added fact is not a declared effect, and `check_observed_tests` would refuse it as an
    unexpected identity;
  - splitting a fact into theory rows would raise the count without a new identity, but that is
    gaming the floor;
  - both contradict the acceptance's "one fact fewer".
- **Keeping the deleted fact** breaks the `S12-MANDATORY` delete, and the census would again
  refuse it as unexpected.

**What resolves it (operator):** any one of these:

- lower the `constructor_unit_tests` floor to 736 for this step;
- make `authorized_count_credits` credit an exact deleted method identity in a
  `"granularity": "method"` suite;
- accept the current slice as it stands.

Note the second gotcha below: an acceptance alone leaves the floor at 737.

## Artifacts

- `.artifacts/0013-build.log`
- `.artifacts/0013-constructor-unit-tests.log`, `.artifacts/0013-constructor-unit-tests-results/`
- `.artifacts/0013-ui-smoke.log`, `.artifacts/0013-ui-smoke-results/`
- `.artifacts/0013-ui-tests.log`, `.artifacts/0013-ui-tests-results/`
- `.artifacts/0013-elevation.log`
- `.artifacts/0013-invariants.log`

## Gotchas

- **Three `complexity = None/Some` mentions remain, all in files outside this step:**
  - `MaterialsWindowTests.fs:459`;
  - `NkDispersionChartTests.fs:27`;
  - `OpticalConstructor.Ui.Tests.fsproj:124`.

  They are step 14's sweep ("the last mentions of the optional complexity go"). This step did
  not touch them.
- **`previewPane` still classifies a view-only entry's optional tabs from the assembled tensors**
  (`assembledActivity` / `assembledMagnetic`), and it draws the n/k chart with
  `m.editor.anisotropy`, not with the shown complexity. Neither is in this step's scope, so
  neither changed.
- **Attempt 1 reported constructor-unit-tests as "pass" from its local run.** That run read only
  `Passed!` and the 736 count. It did not compare the count with the engine's floor, which is
  where the gate failed.
- **Accepting this step leaves the floor at 737.** `write_checkpoint_counts`
  (`checkpoint_record.py`) refuses a lower count unless the step has `allow_lower` or a credit.
  Neither applies here, for the same granularity reason. It logs `CHECKPOINT-COUNT-REGRESSION`
  and keeps 737. Every later step that runs the whole constructor suite would then fail at 736
  in the same way. The floor needs correcting, not only this step's acceptance.
- **ui-tests also deletes a fact in this step, but its floor is not affected.** ui-tests runs
  under the step's five-class filter, and a filtered gate drops the whole-suite floor
  (`evaluate_gate.py`, step 1). ui-smoke only renames facts, so it stays at 208.

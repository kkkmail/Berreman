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
- [x] Attempt 1, local observation: build, constructor-unit-tests, ui-smoke, ui-tests (step
  filter), elevation and the s02 invariants.
- [x] **Attempt 2.** The gate engine failed `constructor-unit-tests` with
  `constructor_unit_tests count 736 < baseline 737`. This was diagnosed as an authority conflict
  (see Testing state). No product or test file was edited.
- [x] **Relaunch after the operator reply (2026-11-05, task `0013-attempt-01-task.txt`).** The
  operator committed the tree as `d8a56f5 0050-0013-manual` and landed the ASG credit fix. The
  note says: "Make no product or test edit. Confirm the tree against the slice, refresh the
  state-of-the-world and the impl-log, and hand the tree to the gates."
  - **No product or test file was edited.** Only this impl-log, the state-of-the-world and the
    impl-plan's relaunch note were written.
  - The tree was checked against the slice; the results are in Testing state.
  - The note was checked against ASG and holds; see Testing state.

## Files modified

All of these are committed in `d8a56f5` (on top of step 12's `f53174e`). This relaunch edited
none of them.

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

Relaunch (spec files only): `0013-impl-log.md`, `0013-state-of-the-world.md` and
`0013-impl-plan.md` (relaunch note).

## Decisions

- **Deviation: `ExportImportTests.fs:159` was edited although the slice does not list it.** The
  fact `a metadata-only library file re-imports each entry as vacuum data` asserted
  `Assert.Equal(Some vacuumPlaceholderComplexity, e.complexity)`. Once the field is no longer an
  option, that line no longer type-checks, so the build (and with it every gate) would be red.
  The repair drops the `Some` and nothing else, and the fact's subject is unchanged. It is a
  consumer repair forced by the authorized `MaterialEntry.complexity change_shape` contract
  ("the record and all its consumers change in this one step, so the build stays green"). The
  code-judge accepted it.
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
- **Relaunch: no gate was run.** Under Invariant 6 the gate engine is the only gate authority,
  and the operator's note hands the tree to it. Attempt 1's local runs are on this same product
  tree; Testing state keeps them as history, not as this round's result.

## Testing state

### Relaunch: the tree against the slice (read-only checks, no gate run)

The product tree is `HEAD` (`d8a56f5`). `git status` shows no product or test change, only the
arc-runner's own `specs/0050` state files. Against `f53174e` (step 12):

- **Byte-identical to the judged tree.** I saved `git diff --binary f53174e d8a56f5 -- Berreman/`
  to `.artifacts/0013-relaunch-step-diff.txt` and hashed it with ASG's own
  `core/file_extraction.py:cleaned_sha256`, the hash behind `pipeline/state.py:tree_fingerprint`.
  The result is `969e23357ffdabcbfb7e6f02b042dfd0672f6ae7002799903925d378e89ddf6e`, the exact
  fingerprint the code-judge reviewed green (`code_judge/0013-01-code-judge-task.txt:57`) and the
  engine measured at 14:50:12. At attempt 2 the worktree diff against `f53174e` was these
  product files only: the step's other dirty files were excluded state, and its spec markdown
  was untracked. So the committed tree is the judged tree, unchanged.
- **Record.** `MaterialLibrary.fs:262` is `complexity : MaterialComplexity`, and `:267` is
  `member this.properties : OpticalPropertiesWithDisp = this.complexity.toProperties`. The
  record has no `properties` field.
- **Retired and deleted symbols.** `previewAnisotropy`, `presetProperties` and
  `viewOnlyComplexity` occur nowhere under `Berreman/`.
- **Editor case.** `MaterialEditorView.fs:69` is
  `| ViewOnlyMaterial of reason : ControlCaption * shown : MaterialComplexity`. `:352` opens
  every non-editable entry with the slice's caption and `entry.complexity`.
- **Readers.** `MaterialStore.fs:74` `payloadOf` returns
  `VersionPayload<MaterialComplexity, MaterialMetadata>`. `OutOfBandDiagnostic.fs:87` reads
  `ReachableMaterial.definedSegmentsOf entry.complexity`.
- **Test census.**
  - `[<Fact>]`/`[<Theory>]` attributes go from 708 to 707 in `OpticalConstructor.Tests` and
    from 733 to 732 in `OpticalConstructor.Ui.Tests`.
  - The removed names are the two authorized deletes plus the three old names.
  - The added names are exactly the three successors:
    - `MaterialEditorWindowTests.fs:514` `Save adds a NEW material with its complexity, …`;
    - `:752` `acceptance: Save round-trips a NEW entry through MaterialProxy with its complexity`;
    - `:833` `a non-editable entry opens view-only with no Save affordance and no ladder`.
- **Clause (7).** None of the 22 changed files contains `complexity = None`,
  `complexity = Some` or `Some complexity`.
- **LF.** `git diff --numstat` equals `--ignore-cr-at-eol` for the step's diff, and every changed
  file is stored `i/lf`.

### Relaunch: the operator's ASG claim, checked

The claim holds. `ai_strategy_generator/executability/runtime_tests.py:authorized_count_credits`
now skips only suites whose granularity is neither `"row"` nor `"method"` (line 106), and its
doc states that a deleted method of a `"method"` suite earns one unit.

Both consumers read it:

- the gate baseline (`evaluate_gate.py:408-412`, `credited_counts`);
- the checkpoint write (`run_arc_runner.py:4826-4831`, `allowed_decreases`).

For this step's single `delete` in `constructor-unit-tests`, the floor is credited to 737 − 1 =
736 and the checkpoint can record 736. The relaunch's `BASELINE` line still logs the uncredited
737, because the credit is applied inside the gate evaluation, not at baseline resolution.

### Attempt 1: local observations on this product tree (history)

- `build`: exit 0, 0 errors. The only warnings were the two vendored MathNet SYSLIB0051 ones
  already on the start tree (`.artifacts/0013-build.log`).
- `constructor-unit-tests` (whole suite): **736/736**. That is 737 at checkpoint `"10"` (slice
  0011), less the deleted `every built-in carries Some complexity after the step-004 re-seed`
  (`.artifacts/0013-constructor-unit-tests.log`).
- `ui-smoke` (whole suite): **208/208**, unchanged. Both renamed facts passed under their new
  names (`.artifacts/0013-ui-smoke.log`).
- `ui-tests` (the step's five-class filter): **93/93**, which is 94 less the deleted coded-preset
  fact (`.artifacts/0013-ui-tests.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 8 project(s)`
  (`.artifacts/0013-elevation.log`).
- The four s02 invariants found nothing (`.artifacts/0013-invariants.log`).

### Attempt 2: the gate engine's measurement and the authority conflict (history, now resolved)

On tree `969e23357ffd` at 14:50:12, the engine measured build and elevation green, and
`constructor-unit-tests` as `Passed!` with 736/736 and its identity census accepted. It failed
the gate anyway with `count 736 < baseline 737`, because `authorized_count_credits` then skipped
`"method"` suites, and Berreman's suites are all `"method"`. No worker edit could satisfy both
the slice's "one fact fewer" and the floor, so the round reported `commit_ready: false`. The
code-judge (cycle 2) and the resolver confirmed it, and the post-mortem (`0013-01.postmortem-md`)
assigned the defect to ASG. The operator's ASG fix and relaunch resolve it.

The gate engine never ran ui-smoke, ui-tests or the four invariants on this tree. This relaunch
leaves them to the engine.

## Artifacts

- `.artifacts/0013-build.log`
- `.artifacts/0013-constructor-unit-tests.log`, `.artifacts/0013-constructor-unit-tests-results/`
- `.artifacts/0013-ui-smoke.log`, `.artifacts/0013-ui-smoke-results/`
- `.artifacts/0013-ui-tests.log`, `.artifacts/0013-ui-tests-results/`
- `.artifacts/0013-elevation.log`
- `.artifacts/0013-invariants.log`
- `.artifacts/0013-relaunch-step-diff.txt`: the step's product diff (`f53174e..d8a56f5`,
  `Berreman/`), whose `cleaned_sha256` is the judged fingerprint
- `.slices/0013-01.postmortem-md` (operator-side post-mortem of the escalation)

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
  `Passed!` and the 736 count. It did not compare the count with the engine's floor. A local run
  shows a count; only the engine's credited floor decides the gate.
- **Correction to attempt 2's accept gotcha.** Attempt 2 wrote that an accept would leave the
  floor at 737 because `write_checkpoint_counts` refuses 736. The post-mortem corrects this: an
  `--accept-current-slice` never reaches the checkpoint write at all. The floor would have
  stayed at 737 because `resolve_baseline_counts` walks back to key `"10"`. With the ASG fix and
  a green engine run, the checkpoint records 736 for this step, and step 15 (the next
  whole-suite run) starts from it.
- **The tree fingerprint changed because of the operator's commit, not because of an edit.** The
  boot logged "state-file worktree fingerprint drift: invalidated prior attestations and forced
  full review". The product content is still the tree the judge reviewed as `969e23357ffd`:
  hashing the commit's product diff reproduces that fingerprint exactly (see Testing state).
  This relaunch edited none of it.
- **ui-tests also deletes a fact in this step, but its floor is not affected.** ui-tests runs
  under the step's five-class filter, and a filtered gate drops the whole-suite floor. ui-smoke
  only renames facts, so it stays at 208.

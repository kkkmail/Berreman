# Step 0252 — impl log

## Relaunch after manual 016, 2026-11-10

This round is the normal profile-1 resume that manual 016 asks for. It makes no product edit.

1. Read the two system prompts, the project prompt, the slice, the operator note and
   `specs/0050/.manual/016-step-0252-recommit-fixture.md`.
2. Confirmed that HEAD holds the whole step:
   - `73c2b10` holds the worker's implementation.
   - `fae11f6` holds both operator repairs:
     - `ItemEvaluation.sequenced` is public, and `chartForRun` calls it four times.
     - The `InverseConstructorTests` fixture binds the plate and records its three items with
       `KitLibraryContext.recordItem`.
   - The working tree differs from HEAD only in the supervisor's own state files.
3. Checked every slice requirement against the code. All are in place:
   - `RunStep` has two cases, `ElementStep` and `ObjectStep`.
   - `SourceEmission.emittedInto` exists.
   - `detectorStokes` and `read` take the emission.
   - The docs of `DetectorIntensity` and `PolarizerBehavior` are updated, as is AGENTS.md's gotcha.
   - In `chartForRun`:
     - polarizers are evaluated through `BoundPolarizerPreset`;
     - samples are bound once through `BoundOpticalElement.ofStackSample`;
     - every chart reads the default emission;
     - the two failure charts are in place.
   - The four new `ExperimentRunTests` facts and the renamed one are present.
   - Both `behaviorInputStokes` facts are gone, and the new UI fact is present.
   - A grep for `behaviorInputStokes`, `runInputStokes`, `PolarizerStep`, `SampleStep` and `inOrder` over every
     `.fs` file under `OpticalConstructor/` finds nothing.
4. Compared the product tree with the hashes the operator verified (`.artifacts/0252-relaunch/tree_check.py`, result
   in `tree-check.json`):
   - 1014 product files were checked: `Berreman/`, `MathNetNumerics/`, `Elevation/` and the root build files.
   - None differs and none is missing.
   - No F# source or project file is outside the verified set.

   So the operator's measurements in `.artifacts/0252-operator-repair/` describe exactly this tree:
   - 19 of 19 gate evaluations passed;
   - constructor tests 1401, smoke 245 and selected UI 85, all with exact censuses and no failures or skips;
   - the whole non-smoke UI suite passed 674;
   - elevation was clean over 12 projects from `f395597`.
5. Ran no gate (Invariant 6). The arc-runner's gate engine runs the 19 gates after this round and publishes
   checkpoint 251. No checkpoint, counter or acceptance file was touched.

## Operator recovery, 2026-11-10

Manual 016 authorizes the formerly out-of-scope fixture repair and Result collector reuse. The fixture now
binds the initial inverse sample to the glass plate and stores its three assigned physical items through
`KitLibraryContext.recordItem`, preserving all assertions.
`ItemEvaluation.sequenced` is exposed and replaces all four calls to the deleted local `inOrder` collector.

All 19 frozen gates passed: constructor 1401, smoke 245, selected UI 85; exact accepted censuses and no
failures/skips. The additional whole non-smoke UI suite passed 674. Elevation found no issues across all 12
projects from the arc origin. Evidence: `.artifacts/0252-operator-repair/`. The original worker history below
describes the escalation before this recovery. Its not-ready statements are superseded by these results.

Wait for operator commits, then resume normally with `--executability-profile 1` and manual 016 as the
operator reply. The supervisor must produce checkpoint 251; no accept-current-slice or checkpoint fabrication.

## Progress

1. Read the protocol, the project prompt, the slice, AGENTS.md, manuals 013 and 015, and 0251's state of the
   world.
   - Surveyed `ExperimentRun.fs`, `Propagation.fs`, `ItemEvaluation.fs`, `ItemModels.fs` and the run section of
     `TableAndElementRotationView.fs`.
   - Read the three test files.
2. `ExperimentRun.fs` rewritten:
   - `RunStep` = `ElementStep of mueller` | `ObjectStep of mueller`, with `member this.mueller`. It is declared
     before `SourceEmission`, so `emittedInto` is an intrinsic member.
   - `SourceEmission.emittedInto steps` is `stokes` times r. r = 1 / S₀ of `Propagation.unpolarizedStokes` through
     the path's leading `ElementStep`, when that S₀ is above zero; otherwise r = 1.
   - `detectorStokes emission steps` folds `step.mueller * sv` from `emission.emittedInto steps`.
   - `read kind emission steps` reads the result as before.
   - The `(StokesVector * bool * bool)` fold and its first-polarizer arm are gone.
   - The module doc is two lines citing spec 0046 and spec 0050 S8.1.
3. `Propagation.fs`:
   - `behaviorInputStokes` deleted with its doc.
   - `DetectorIntensity`'s doc is now relative to the source's intensity reference (spec 0050 S8.1).
   - The Part F section comment no longer says `ComputedIdeal` synthesizes through `inputStokes`.

   `ElementId.fs`: the `PolarizerBehavior` doc names `Propagation.analyzerMueller` (spec 0050 S8.1).
4. `TableAndElementRotationView.fs`:
   - **Removed:**
     - `runInputStokes`, which nothing read.
   - **The run section header and `chartForRun`'s doc:** rewritten, 2 and 3 lines, citing spec 0050 S8.1.
   - **`StepBuilder.SweptStep`:** now answers `Result<RunStep, ItemEvaluationError>`.
   - **`builderFor`:**
     - A polarizer is `BoundPolarizerPreset p.behavior`, evaluated with `muellerAt` at
       `{ R1 or the swept x; normal incidence; the path step's branch }` → `ElementStep`.
     - A sample is `BoundOpticalElement.ofStackSample model.materials (ParameterValues.defaultsOf
       sample.parameterAxes) sample`, bound once per path step. It is evaluated at
       `{ spin; drawn incidence or the swept x; branch }` and the point's λ → `ObjectStep`.
   - **Every chart reads with** `SourceEmission.ofValues (ParameterValues.defaultsOf
     Library.SourcePreset.lightSourceAxes)`.
   - **Failure charts:**
     - `SampleMaterialErr e` keeps "Sample materials failed to resolve" with `e.runFailureCaption`.
     - Every other case, listed explicitly, shows "The run cannot be evaluated" with its `describe`.
   - **Opens** `OpticalConstructor.Domain.ItemModels` and `.ItemEvaluation`, which the extension members
     (`lightSourceAxes`, `Sample.parameterAxes`, `SourceEmission.ofValues`) need. They are placed right after
     `OpticalConstructor.Domain`, so every later open still shadows them as before.
5. `AGENTS.md`: the gotcha's clause is replaced word for word as the slice dictates. Nothing else in the file
   changed.
6. Tests:
   - **`ExperimentRunTests`:**
     - `lp` is now `ElementStep (analyzerMueller IdealLinear θ)` and `clear` is
       `ObjectStep identityMueller`.
     - A file-local `defaultEmission` comes from `ofValues (defaultsOf lightSourceAxes)`.
     - One fact is renamed and two keep their numbers.
     - Four new facts.
     - The module doc is updated.
   - **`PolarizerBehaviorTests`:** the two `behaviorInputStokes` facts are deleted, and with them the
     `stokesComponents` helper only they used. Every other fact is unchanged.
   - **`ExperimentControlsTests`:** `two bound samples in series are both solved on the detector's path`.
     It also opens `Berreman.Constants`, `Berreman.Geometry` and `Berreman.Fields`.
7. **Build 1:** three FS0039 errors in the UI test (`WaveLength` and `IncidenceAngle` live in `Berreman.Fields`).
   Fixed by opening `Berreman.Fields`. **Build 2** is clean: 0 warnings, 0 errors.
8. **constructor-unit-tests:** 1401 / 1401. **Selected ui-tests:** 84 / 85. The one failure is pre-existing; see
   Testing state.
9. ui-smoke and elevation ran concurrently over the same build. The fourteen invariant greps and the LF check were
   clean.
10. **Red by mutation** (`0252-edit-scripts/mutate.py`; source hashes in `0252-mutation-sources.sha256`).
    - The mutations: the intensity reference forced to 1, and the host skipping every non-varied sample.
    - What went red: six `ExperimentRunTests` facts (all four new ones, the renamed one and
      `a polarizer AFTER the sample analyzes`) and the new UI fact.
    - The ellipsometer fact is scale-invariant and stays green, as expected.
    - Both sources were restored and verified by SHA-256, the backups deleted, the solution rebuilt, and the final
      runs repeated.

## Files modified

All of these are in the slice's authorized `edits`:

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentRun.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Propagation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentRunTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PolarizerBehaviorTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`
- `AGENTS.md`

## Decisions

- **"The first `ElementStep` in front of the first `ObjectStep`" is the path's head when it is an `ElementStep`.**
  Any element before the first object comes after the head, so the head is the first of them. A path opening on
  an object, or an empty path, has r = 1.
- **The emitted vector is scaled component by component** (`StokesVector.create [ r · Sᵢ ]`). That is `stokes`
  times r literally; `StokesVector` has no scalar product.
- **`inOrder`, local to `chartForRun`, collects `Result`s (first error wins).**
  - It serves the builders, the steps at a point, the points and the captured branches.
  - `ItemEvaluation.sequenced` does the same job but is private, and `ItemEvaluation.fs` is outside this step's
    edits. `inOrder` is not a copy of its body: it uses `tryPick` / `choose`, the shape `chartForRun` already used
    for its builder errors.
- **A polarizer's orientation uses the path step's branch,** as the slice says. A polarizer only transmits, so the
  branch is always `TransmittedBeam` today. A reflected one would surface as `UnmodelledBranchErr` under "The run
  cannot be evaluated".
- **The UI fact reads its expected samples from `BuiltinCatalogue.sample`** (the same shipped entries the scene's
  library imports), not through the host's own `boundEntry`, so the expectation does not reuse the code under test.
  - It asserts both samples are on the detector's path before comparing.
  - It folds the expected matrices in the order the path lists them.

## Testing state

Development runs (no gate verdict is claimed, Invariant 6). Logs are under `specs/0050/.artifacts/0252-*`.

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)` (`0252-build-2.log`). Build 1 (`0252-build-1.log`) recompiled the
  vendored MathNet C#, which printed its two pre-existing `SYSLIB0051` warnings, and stopped on the three FS0039
  errors fixed in progress 7. It printed no F# warning.
- **constructor-unit-tests,** the gate's command:
  - `Passed!`, 1401 succeeded, 0 failed, 0 skipped, 68 s.
  - That is 1399 + 4 new − 2 deleted.
  - Every new and renamed identity is `Passed` in the TRX, and no `behaviorInputStokes` identity remains.
- **ui-tests over the six selected classes,** the gate's command and selector (`0252-ui-tests.log`):
  - 85 total, 84 passed, **1 failed**.
  - The new `two bound samples in series are both solved on the detector's path` is `Passed`.
  - **The failure:** `InverseConstructorTests.recommitting a copied experiment preserves its assigned physical ids`
    (InverseConstructorTests.fs:509). The kit status is "Not saved to the kit — the items could not be copied: the
    kit has no item of that id: 3c9e1f47-…, assigned at the element 'Linear polarizer #4' … of E1".
  - **It predates this step and is deterministic** (`0252-ui-recommit-alone.log`, run alone):
    - The fixture assigns three hard-coded `ItemId`s (`pieceA`, `pieceB`, `samplePiece`) that it never writes into
      the kit.
    - Since step 0227, `recordExperiment` refuses a missing piece before writing.
    - Step 0242 recorded the same red and reproduced it on an unmodified `git archive HEAD` copy
      (`0242-head-check-test.log`). This step's diff does not reach the kit-copy or record path.
  - **The conflict:**
    - The slice's acceptance says "ui-tests MUST pass over the selected classes", and its gate selection includes
      `OpticalConstructor.Ui.Tests.InverseConstructorTests`.
    - The repair is a fixture change in `InverseConstructorTests.fs` (write the assigned pieces through the kit
      before recommitting), or a product change on the record path. **Missing authority:** neither file is in this
      step's authorized `edits`, and the slice declares no test effect on that fact.
    - Step 0261 is the step authorized to rework this fact (`recommitting a copied experiment preserves its
      assigned physical ids` MUST retain the copied setup's item assignments).
  - **commit_ready is therefore false.** The rest of this step is complete.
- **ui-smoke,** the gate's command: `Passed!`, 245 / 245, 214 s against the 600 s deadline (`0252-ui-smoke.log`),
  re-run over the final rebuild (`0252-ui-smoke-final.log`).
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 193 s. Known
  findings: 0 (`0252-elevation.log`).
- **Final repeat after the mutation restore:**
  - build: 0 warnings, 0 errors (`0252-build-final.log`);
  - constructor-unit-tests: 1401 / 1401 (`0252-constructor-unit-tests-final.log`);
  - selected ui-tests: 84 / 85, the same inherited red (`0252-ui-tests-final.log`).
- **The fourteen s25 invariants,** by their greps: all find nothing (`0252-invariants.log`).
- **LF:** `git diff --numstat` equals `--ignore-cr-at-eol`, and no edited file holds a CR.

## Artifacts

- `specs/0050/.artifacts/0252-relaunch/tree_check.py`, `tree-check.json`: the relaunch's comparison of the tree
  with the operator-verified hashes
- `specs/0050/.artifacts/0252-operator-repair/`: the operator's measured gate, census and whole-UI evidence
- `specs/0050/.artifacts/0252-build-1.log`, `0252-build-2.log`
- `specs/0050/.artifacts/0252-constructor-unit-tests.log`
- `specs/0050/.artifacts/0252-ui-tests.log`, `0252-ui-recommit-alone.log`
- `specs/0050/.artifacts/0252-ui-smoke.log`, `0252-ui-smoke-final.log`, `0252-elevation.log`, `0252-invariants.log`
- `specs/0050/.artifacts/0252-red-mutation-build.log`, `0252-red-mutation-tests.log`, `0252-mutation-sources.sha256`
- `specs/0050/.artifacts/0252-build-final.log`, `0252-constructor-unit-tests-final.log`, `0252-ui-tests-final.log`
- `specs/0050/.artifacts/0252-edit-scripts/` — the one-shot edit script for `chartForRun` and the mutation script

## Gotchas

- **The `InverseConstructorTests` red is resolved by manual 016.** `recommitting a copied experiment preserves its
  assigned physical ids` had been red since step 0227 (see Testing state). The operator has repaired its fixture and
  committed the repair in `fae11f6`.
  - Steps 0254, 0259, 0260 and 0261 must keep its three stored items and their bindings.
- **`ItemEvaluation.sequenced` is now public; its doc was left as committed.** The doc still names only the
  evaluator's own uses (a black box's elements, a compound's components, a sample's layers). Its contract, "every
  result in order, or the first error", is accurate for the host's use too. Editing it would have changed the tree
  the operator verified.
- **`WaveLength` and `IncidenceAngle` are `Berreman.Fields` types,** not `Berreman.Geometry` types; a UI test needs
  both opens.
- **Opening `ItemModels` / `ItemEvaluation` in the UI file is order-sensitive for record-label inference.** They are
  opened before `Kit` and the rest, so later opens keep their precedence, and the new orientation records are
  annotated `ElementOrientation`.

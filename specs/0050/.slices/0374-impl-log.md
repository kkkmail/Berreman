# Step 0374 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and its gates snapshot. No operator note or
  ruling is attached.
- Read the seams: step 373's `StudyForward.fs` and its tests, `ExperimentRun.fs`, `ItemEvaluation.fs` (`SlotRunParam`,
  `runElementWith`, `runStepOf`, `runEmissionOf`, `BoundOpticalElement.muellerAt`), `Setups.fs` (`slotsIn`,
  `drawnWith`, `inLightOrder`, `boundSourceIn`, `SlotTurn`), `BeamLayout.fs` (`layout`, `PathStep`, `incidenceOn`,
  `propagationBranch`), `Kit.fs` (`ItemDial`, `SetupError`), `ItemModels.fs` (`DialSign.physicalAngleOf`, detector
  forms), `MuellerReconstruction.DarkMean`, and the UI's `chartForRun` (the table's own path walk, same precedence of
  settings). Confirmed none of the new names exists.
- `ExperimentRun.fs`: `RunStep.productOf` after `read`.
- `StudyForward.fs`: three `StudyForwardError` cases and renders; after `assignmentFor`, `ParameterValue.darkMean`,
  `DetectorReadout`, `BoundPathStep`, `BoundExperiment`, `Experiment.boundWith`, `BoundExperiment.readingAt`. New
  opens: `Berreman.Constants`, `Berreman.Geometry`, `ExperimentRun`, `BeamLayout` (the last for the `incidenceOn` /
  `propagationBranch` extensions). Domain built clean (first attempt missed the `BeamLayout` open: FS0039).
- `ExperimentRunTests.fs`: `a path's Mueller matrix is the product of its steps in light order`.
- `StudyForwardTests.fs`: `open ExperimentRun`; the error-render fact widened by the three cases; the bench fixtures
  and the four forward facts appended.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentRun.fs` (+7).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs` (+242).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentRunTests.fs` (+20).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs` (+374 / -1, the module doc line).

No project file changed. All four are LF (`git diff --numstat` equals `--ignore-cr-at-eol`; a byte count finds no CR).
`specs/0050/.manifest.state.json` was already modified at round start (supervisor state) and is untouched by this round.

- Built the solution, ran the whole constructor suite, the elevation gate from the arc base, the twenty s37 invariants
  and the selected ui-tests (below).

## Testing state

All run locally after the final edit (Release x64):

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`; the 12 warning lines are all NU1902 advisories (exempt); the Domain, built alone after its last edit, shows `0 Warning(s)` | 46 s | 600 s |
| constructor-unit-tests | `Test run summary: Passed!`, 1634 succeeded, 0 failed, 0 skipped (checkpoint 373: 1629; +5); the five new facts and the widened error-render fact are `Passed` in the TRX under exactly the authorized ids | 74 s | 180 s |
| ui-tests (`OpticalConstructor.Ui.Tests.FsxAcceptanceTests`, the gate's own command and selector) | `Test run summary: Passed!`, 14 succeeded, 0 failed, 0 skipped (count unchanged) | 76 s | 360 s |
| elevation (arc base `f395597c`) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0; Domain `findings=0` | 239 s | 600 s |
| twenty s37 invariants | each `section_invariants --find-nothing` exits 0 | a few seconds each | 300 s |

The five new facts reference symbols this step creates (`RunStep.productOf`, `boundWith`, `readingAt`, the new error
cases), so before the production code they could not compile (their red); they were written after the Domain code and
were not separately observed failing. Each fact carries a discriminating check: C · B · A against A · B · C, the
analyzer at 30° against 20° (the assignment's dial zero against the item's own), the sample at 40° against 0°, and the
200 nm film at 450, 500 and 632.8 nm.

Every requirement of the slice landed this round; nothing is deferred.

## Artifacts

- `specs/0050/.artifacts/0374-build.log`
- `specs/0050/.artifacts/0374-constructor-unit-tests.log`
- `specs/0050/.artifacts/0374-ui-tests.log`
- `specs/0050/.artifacts/0374-elevation.log`
- `specs/0050/.artifacts/0374-invariants.log`

## Gotchas

- **S₀ and Ψ, Δ through `detectorStokes`, not `read` (a recorded deviation from the how-to's wording).** The how-to
  names S₀ as "the intensity `ExperimentRun.read Intensity this.emission steps` gives" and Ψ, Δ as what
  `read Ellipsometer` gives. `read` is `detectorStokes` projected to `.intensity` or `.psiDelta`. `readingAt` uses those
  two projections directly. Calling `read` would need an unreachable arm per kind, and the ellipsometer's arm would
  re-wrap `MeasuredIntensity s0.value`, the `Ctor other.value` shape ELEV001 refuses. The numbers are identical by
  construction, and the tests compute both through `ExperimentRun.read`, so they pin the equality.
- **The dial zero comes from the assignment.** `BoundPathStep.dialZero` is the assignment's `dialZero` value (`valuesOf`)
  of a `DialMounted` item. A fixed item, or a slot without an item, has `FixedMounting` and `Angle.zero`. A dial-mounted
  item without a `dialZero` value answers `ForwardSlotErr (SlotEvaluationErr (MissingParameterValueErr …))`. That can't
  happen to an item `ItemDraft.toItem` made, because the dial publishes the axis.
- **Precedence among settings.** If several settings name one variable on one slot, the last one wins (`tryFindBack`),
  as `chartForRun` reads them. For a rotation, `VaryR1` beats `VaryDialReading`, which beats the recorded `DialTurn`,
  which beats the drawn R1, in the how-to's order. A `VaryWaveLength` setting is read whatever slot it names.
- **The detector.** A detector slot whose item the kit lacks, or whose model is not a detector form, is passed over,
  so a setup with only such slots answers `StudyDetectorErr` (reason: `<setup reference>, which <experiment> references`).
  A path slot whose item the kit lacks is a `ForwardSlotErr (SlotItemErr …)` from `runElementWith`, before its dial is
  read.
- **Order of refusals.** The setup is resolved first, then the emission, then the detector, its readout, and last the
  path steps in light order. The first refusal answers.
- **The wavelength test needs a dispersive element.** The built-in 1 mm glass plate (n = 1.52 constant, thick plate)
  reads the same at every λ. So the retune assertions run over both the plate (as the how-to names it) and the built-in
  200 nm free-standing glass film (`SampleIds.glassFilm200Id`), whose interference makes λ tell.
- **Bench fixtures.** Slots are `emptySlot` placed through `SetupSlot.withItem` (`placing`), so the source slot is bound
  to the HeNe preset and `boundSourceIn` gives 632.8 nm. `benchContext` reuses step 373's `forwardOver` with the bench's
  `choiceOf`. The recorded-dial case sets the slot's `DialTurn` directly.
- **Line endings.** The shell's `grep -c $'\r'` reported every line as CR, but a byte count (Python) finds none. The
  files are LF.

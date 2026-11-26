# Step 0312 — impl log (WIRE_UI)

## Progress

- [x] Program.fs: `mount` composes `.withImportWizard window dispatch`
- [x] KitWindowTests.fs: ui-smoke acceptance fact
- [x] AGENTS.md: gotcha 'Lab files are read through import profiles'
- [x] Development runs of the gates

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
  - opens `OpticalConstructor.Ui.ImportWizardOpening`;
  - `ConstructorScene.mount` names the record it builds `kitBarShell`, and its `shell` is
    `kitBarShell.withImportWizard window (fun msg -> hostDispatch.Value msg)`;
  - one doc line: "Its Attach data file… opens the column-mapping wizard over this window (spec 0050 S9.2)."
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
  - opens `OpticalConstructor.Ui.ImportWizardOpening`;
  - adds `phoneRoiBytes` and `labFileOutsideKits`;
  - adds the ui-smoke fact `acceptance: Attach data file maps a picked lab file through the wizard into the kit and validates it`.
- `AGENTS.md`: the six-line gotcha after 'An experiment is a scan over a setup revision'.

## Decisions

- **The dispatch is deferred through the ref.** `hostDispatch` holds `ignore` until the init command stores the Elmish
  dispatch, so the shell gets `fun msg -> hostDispatch.Value msg`. Passing `hostDispatch.Value` directly would capture
  `ignore`. The archive job host already defers its reads the same way.
- **"Loaded into `initInverse`" is the class's `kitWindowModel … InverseConstructor`.** That is `initInverseWithKit`
  over the AppContext's stores, then `loadKit opened`. `initInverseWithKit` is `initInverse`'s seed over a supplied kit
  (`.manual/010`: no scratch kit). `InverseConstructorWindow` composes the same seed before `mount`, and every other
  fact in the class uses this helper.
- **E1 is recorded as step 310's facts record it**: a linear polarizer is added, `ExpChooseElement`, then `ExpCommit`.
  The fact asserts that E1 is `ExperimentId 1` with the one axis `VaryR1` over `VariableRange.forVariable VaryR1`
  (0…360°), and that it holds no data yet.
- **The model is held in a ref.** The shell is set after the model is recorded, as `mount` sets `kitShell` in its init.
  This is needed because the dispatch closes over the ref.
- **The lab file is written outside every kit**: `<class test dir>/Lab/<guid>/phone-roi.csv`, a sibling of `Kits/`.
- **The read-back uses a fresh `KitProxy.create folder`**, so it reads the disk and not the window's context.

## Testing state

These were development runs of each gate's own command over the final tree. The gate engine runs the gates themselves.

- **build**: exit 0, 0 errors, 77 s (deadline 600 s). The only 2 warnings are the pre-existing vendored MathNet
  SYSLIB0051.
- **The new fact alone** (`--filter-method`): Passed, 1 of 1, 7 s.
- **elevation**: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 205 s (deadline 600 s). Known findings 0,
  ELEV004 suppressed 1428, unchanged from step 311.
- **constructor-unit-tests**: Passed!, 1525 of 1525, 73 s (deadline 180 s). Unchanged.
- **ui-smoke**: Passed!, 253 of 253, 219 s (deadline 600 s). That is 252 + 1, and the TRX shows the new fact `Passed`.
- **ui-tests** (selected `KitWindowTests`, non-smoke): Passed!, 16 of 16, 24 s (deadline 360 s). Unchanged.
- **invariant-s30-\* (16)**: each exit 0, `INVARIANT HOLDS`.
- **LF only**: 0 CR bytes in the three touched files, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- No run exceeded its effective deadline.

## Artifacts

All of these are under `specs/0050/.artifacts/`:

- `0312-build.log`
- `0312-new-fact.log`
- `0312-elevation.log`
- `0312-constructor-unit-tests.log` and `.trx`
- `0312-ui-smoke.log` and `.trx`
- `0312-ui-tests.log` and `.trx`
- `0312-invariants.log`

## Gotchas

- **The AGENTS.md bullet needs 124 columns to fit in six lines.** At the file's usual 120 columns, the slice's text
  needs at least seven lines, whether or not a code span may break. The slice's binding constraint is six lines, so the
  bullet is wrapped at the narrowest width that fits: its longest line is 124 columns, and no code span is split.
  AGENTS.md already has lines of 123 and 130 columns (lines 44 and 47).

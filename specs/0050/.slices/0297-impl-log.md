# Step 0297 — impl log

## Progress

- 2026-11-11: read the task, the system prompts, the project prompt, the slice, the prior SoWs (0284, 0296) and the
  step-284 / step-296 facts. Wrote the impl plan.
- **The fact.** `KitWindowTests` gained the private helper `typeCommitted` and the ui-smoke fact
  `a two-variable experiment recorded through the bay keeps its scan, role, session and notes in its kit file`, after
  step 284's acceptance fact.
- **First run: red on the fixture, not the product.** `ExperimentSessionField must be in the window`: the bay's
  committing fields carry their id as `Name` (`committingField`, `TextBox.name id.value`), not as an AutomationId.
  `typeCommitted` now finds the field through the file's `namedControl` (by `Name`), as the file already finds the
  table canvas and the ribbon panes. Rebuilt, rerun: green.
- **Mutation check (red).** `SetExperimentNotes` in `TableAndElementRotationView.fs` was made to keep the held notes
  (`ignore text; held`). The new fact failed with `Expected: ExperimentNotes "LR re-seated"`, `Actual: ExperimentNotes ""`.
  The file was restored from the copy byte for byte (SHA-256 `88447678…cd704` before and after) and the solution
  rebuilt; the Ui DLL is newer than the restored source.
- **AGENTS.md.** One five-line bullet, **An experiment is a scan over a setup revision**, directly after "Names come
  from the kit's naming template". Nothing else in the file changed.
- LF only in both files (`git diff --numstat` equals `--ignore-cr-at-eol`; no CR in either file).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
- `AGENTS.md`

No product file changed (the mutation was reverted and hash-checked).

## Decisions

- **The kit folder is `TestAppScope.freshKitFolder owner`** (`<BaseDirectory>/KitWindowTests/<GUID>`), the class's test
  directory, as step 284's fact makes it.
- **The sample is `AddElementBoundTo (Sample, Some glassPlate1mmId.toElementValueId)`.** `addElement` places it on the
  main beam at the source's -0.3 m mark, between the seeded `src` and `det`; the fact asserts the x order rather than
  the list order (the sample is appended to the list after `det`).
- **The Experiments bay is opened by `ShowFlowStep ExperimentsFlowStep`**, which also selects the Experiments ribbon bay;
  the window draws the bay pane there (step 242's flow strip).
- **Variable, role and badge ids come from their members** (`ExperimentControls.VaryR2.uiKey`,
  `Experiments.CalibrationRun.uiKey`, `ExperimentId.uiKey`), not from literal strings; they render as the slice's
  `ExperimentVariable_r2`, `ExperimentRole_calibration` and `ExperimentBadge_1` (pinned by step 296's id fact).
- **No kit write may be refused**: inside the window the fact fails with the kit status text if one shows, so a refused
  write names itself instead of surfacing as a missing file.

## Testing state

The final runs are over the final build, with the gates' own commands:

- **build:** exit 0, 0 warnings, 0 errors, 16 s incremental.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 206 s. It ran before a
  test-only rename (`alongOf` → `slotOfKind`, the inner `sample` → `sampleSlot`); test projects are outside its scope.
- **ui-tests** (`KitWindowTests`): 16 / 16, unchanged, 23 s.
- **ui-smoke:** 251 / 251 (250 + 1, the new fact `Passed` in the TRX), 223 s.
- **The 16 s29 invariants:** each finds nothing.

No run went over its deadline. Commit-ready.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0297-build-dev-0{1,2}.log` and `0297-gate-build.log`;
- `0297-dev-newfact-0{1,2}.log` (the fixture's red, then green);
- `0297-mutation/` (the original product file, the hashes, the mutated build and its red run);
- `0297-gate-elevation.log`, `0297-gate-ui-smoke.log`, `0297-gate-ui-tests.log`, `0297-trx/`;
- `0297-gate-invariants.log` and `0297-invariants.b64` (the 16 invariant commands as the snapshot encodes them).

## Gotchas

See the SoW.

# Step 0312 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K). Step 308 gave the wizard its
pure MVU and step 309 its view. Step 310 gave the kit window its messages (`OpenImportWizard`, `SaveImportProfile`,
`AttachDataFile`), and step 311 gave the wizard its own window plus the shell member `withImportWizard`. This step closes
the section's wiring. The App's `ConstructorScene.mount` now composes `.withImportWizard window dispatch`, so a
constructor window's Attach data file… opens the real wizard over itself. A ui-smoke acceptance fact proves the path end
to end through a real folder kit, and AGENTS.md records the section's gotcha.

## What's working

- `ConstructorScene.mount` composes the import wizard into the constructor windows' shell. It uses the window's
  captured Elmish dispatch, so Attach data file… opens the real wizard over the window (spec 0050 S9.2).
- `KitWindowTests` gains the ui-smoke acceptance fact. In a read-write folder kit, a Phone ROI file picked from outside
  the kit goes through the wizard: Phone ROI CSV, then Attach. The fact then checks four results:
  - the file is copied into `data/` byte for byte;
  - the mapping is recorded on E1 and read back from its kit file;
  - E1 is validated with 2 points;
  - the wizard is closed.
- AGENTS.md gains the gotcha 'Lab files are read through import profiles'.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the gates
themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`; the only 2 warnings are the pre-existing vendored MathNet SYSLIB0051 | 77 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1428, unchanged from step 311) | 205 s | 600 s |
| constructor-unit-tests | Passed!, 1525 of 1525, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 253 of 253, 0 failed, 0 skipped | 219 s | 600 s |
| ui-tests (selected: `KitWindowTests`) | Passed!, 16 of 16, 0 failed, 0 skipped | 24 s | 360 s |
| invariant-s30-\* (16) | each exit 0, `INVARIANT HOLDS` | under 1 s each | 300 s |

- **ui-smoke: 253**, which is step 311's 252 + 1, as the slice says. The TRX shows `acceptance: Attach data file maps a
  picked lab file through the wizard into the kit and validates it` as `Passed` (3.8 s). Run alone first, it passed
  in 7 s.
- **ui-tests: 16** over `KitWindowTests`'s non-smoke facts, unchanged. The new fact carries the `ui-smoke` trait.
- **constructor-unit-tests: 1525**, unchanged. This step adds no domain fact.
- The suppressed ELEV004 count stayed at 1428, so the App's new line reads no `.value`.
- constructor-unit-tests ran while elevation was running; ui-smoke ran alone afterwards.
- No run went over its effective deadline.
- Every touched file is LF only (0 CR bytes), and `git diff --numstat` equals the `--ignore-cr-at-eol` count.
- Logs and TRX reports are in `specs/0050/.artifacts/0312-*`.

```yaml
gates:
  constructor_unit_tests: 1525
  ui_smoke_tests: 253
  ui_tests: 16
```

## Architecture

- **No new behaviour, only composition.** The shell of `mount` is the kit bar's shell, renamed `kitBarShell`, followed
  by `.withImportWizard window (fun msg -> hostDispatch.Value msg)`.
- **The dispatch read is deferred.** `hostDispatch` holds `ignore` until the init command stores the Elmish dispatch.
  The lambda reads the ref when the wizard's Attach fires, as the archive job host's callbacks already do.
- **The acceptance fact composes the shell as `mount` does**: `KitWindowShell.inert.withImportWizard host dispatch`. It
  runs over a model held in a ref and dispatched through `updateTableAndElementRotation`. This drives the real
  `ImportWizardWindow` by id, through pointer presses at control centres. It does not drive the App window class, which
  this fact does not need.
- No stored format, schema, version or package changed.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **The AGENTS.md bullet is six lines at up to 124 columns.** At the file's usual 120 columns, the slice's text needs at
  least seven lines. Six lines is the slice's binding limit, so the bullet is wrapped at the narrowest width that fits,
  and no code span is split. AGENTS.md already has 123- and 130-column lines.
- **"Loaded into `initInverse`" is read as the class's `kitWindowModel … InverseConstructor`.** That is
  `initInverseWithKit`, `initInverse`'s seed over the supplied kit, with no scratch kit (`.manual/010`). It is the seed
  `InverseConstructorWindow` hands `mount`.
- **Attach closes the wizard on pointer-press.** The class's `pressAtCentre` already releases only while the pressed
  window is visible.
- **The lab file lives beside the kits, never in one**: `<class test dir>/Lab/<guid>/phone-roi.csv`.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0312: the constructor windows open the import wizard; the S9.2 path is proved end to end through a
  folder kit; AGENTS.md records 'Lab files are read through import profiles'.

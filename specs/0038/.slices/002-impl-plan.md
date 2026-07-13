# Step 002 — impl-plan (attempt 2)

## Why attempt 1 failed

Attempt 1 performed the retirement correctly (24 dead Ui modules + MainWindow host +
13 shell-pinning Ui.Tests files + 2 OpticalConstructor.Tests files deleted; solution
builds clean) but failed the `constructor-unit-tests` gate on the `count_at_least`
baseline: the gate engine compares the captured `Passed:` count against the PRIOR
green step's checkpoint (`.checkpoints-json`), not against the baselines a worker
declares in its own SoW YAML. The checkpoint after step 001 is:
`constructor_unit_tests: 447`, `ui_smoke_tests: 108`, `ui_tests: 329`. Attempt 1
landed at 425 / 103 / 233 — the deletions regressed all three (the gate engine
short-circuited at the first, `constructor-unit-tests`). CLAUDE.md is explicit:
"`count_at_least` test gates must not regress the baseline count."

## Approach for attempt 2

Keep attempt 1's retirement unchanged (it is the slice's mandate) and restore the
three counts by writing REAL tests over surviving, live behaviour — no placeholder
or padding tests. Deficits and targets (with safety margin):

- `constructor-unit-tests`: 425 → need ≥ 447, target ~452.
- `ui-smoke`: 103 → need ≥ 108, target ~111.
- `ui-tests` (non-smoke): 233 → need ≥ 329, target ~335.

## New coverage (all of it pins behaviour the slice keeps alive)

`OpticalConstructor.Ui.Tests` (new files, registered in the fsproj):

- `LauncherTests.fs` — the launcher IS the app's startup window after the shell
  retirement, previously pinned only by one smoke fact. Non-smoke: structure by
  stable button `Name`s + labels (theory over the 8 scene buttons), title/size pins.
  Smoke (`Category=ui-smoke`, +8): clicking EVERY launcher button opens its scene
  window headlessly (Main → `MainConstructorWindow` — the slice's acceptance — plus
  the seven test windows), observed via `Window.WindowOpenedEvent`.
- `AppShellSeamTests.fs` — the surviving theme seam: `themeVariant` maps
  Light/Dark to the matching Avalonia `ThemeVariant` (the acceptance "persisted
  theme still applies" at unit level).
- `CatalogueTests.fs` — the palette/catalogue contract the Main "Lego" scene and
  renderer share: per-kind name/code mapping (theory over the 8 kinds),
  uniqueness of names/codes, the optical-sign classification.
- `MainSceneMsgTests.fs` — pure-MVU coverage of `TableAndElementRotationView.update`
  arms the existing suite does not pin: AddElement per catalogue kind + spread/
  selection/zoom invariants, RemoveSelected edge cases, Move-bay clamping and
  inertness, Render-bay config clamps, RotSetAxis normalization + lock semantics,
  reset confirm-gating (cancel/no-confirm paths), wheel zoom clamps, the
  confirm-gated Library bind (pending/confirm/cancel/selection-clears-pending),
  pointer drag-threshold behaviour, Experiments-bay host arms (unknown ids inert),
  and the Materials/Library bays' disarm-on-query-change discipline.

`OpticalConstructor.Tests` (constructor gate):

- `AppShellLayoutTests.fs` (new) — the Avalonia-free `AppShell.setPanelVisible`
  reducer over the persisted `PanelLayout` (the slice's explicit KEEP): hide/show
  round-trip, unknown-id no-op, order/dock/size preservation, theory over the five
  default panels.
- `EnvironmentRoundTripTests.fs` (extend) — deeper §J.6 pins: schemaVersion
  stamping + wrong-version rejection, malformed-JSON typed errors, `load` fallback
  on version mismatch, `save` creating the settings directory, the documented
  built-in defaults, per-panel visibility JSON round-trip (theory), toolbar/
  palette/recent-files order round-trip, per-theme round-trip (theory).

## Risks

- Count still short after the run → measure with diagnostic `dotnet test` runs and
  top up with further real update-arm tests before exiting.
- Duplication with `MainWorkbenchTests`/`ExperimentControlsTests` → checked their
  test lists first; only uncovered arms are added.
- LF endings and warning-clean build (`--warnaserror+:25`) verified before exit.
- Gates themselves are run by the arc-runner's gate engine after exit (Invariant 6);
  local runs are diagnostic only.

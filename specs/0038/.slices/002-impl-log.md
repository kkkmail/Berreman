# Step 002 — impl-log (attempt 2)

## Progress

- [x] Diagnose attempt-1 gate failure: `count_at_least` baselines come from
      `.checkpoints-json` (447 / 108 / 329), not from the worker's SoW YAML;
      attempt 1's mandated deletions regressed the counts to 425 / 103 / 233.
- [x] Ui.Tests — LauncherTests.fs (launcher structure `ui-tests` + every button
      opens its scene window `ui-smoke`).
- [x] Ui.Tests — AppShellSeamTests.fs (the surviving theme seam).
- [x] Ui.Tests — CatalogueTests.fs (the palette/renderer catalogue contract).
- [x] Ui.Tests — MainSceneMsgTests.fs (74 pure-MVU tests over uncovered update arms).
- [x] Ui.Tests — LocalizationTests.fs extended (+8 loader/lookup edge pins).
- [x] OpticalConstructor.Tests — AppShellLayoutTests.fs (setPanelVisible reducer, 14).
- [x] OpticalConstructor.Tests — EnvironmentRoundTripTests.fs extended (+18 envelope/
      typed-error/defaults/round-trip pins).
- [x] Register the new files in both fsprojs.
- [x] Diagnostic build + test runs: every suite green, every count at/above baseline.

## Files modified

Attempt 1's retirement (deletions in App/Ui/Ui.Tests/Tests, the trimmed AppShell.fs
and SmokeTests.fs, the refreshed Program.fs) is kept unchanged — see the attempt-1
entries in this file's history (superseded text now lives in the SoW's Architecture/
Gotchas). This attempt only ADDS tests over surviving behaviour:

`OpticalConstructor.Ui.Tests` (4 new files, 1 extended, fsproj updated):

- `LauncherTests.fs` (NEW) — the launcher contract. Structure (`ui-tests`): title +
  fixed size, a theory pinning all 8 scene buttons by stable `Name` + label, Main
  first. Behaviour (`ui-smoke`, 8 facts): clicking each button opens its scene window
  headlessly — Main → `MainConstructorWindow` (the slice's acceptance) and the seven
  diagnostic test windows — observed via the `Window.WindowOpenedEvent` seam
  (the WireUiCompositionTests precedent).
- `AppShellSeamTests.fs` (NEW) — `AppShell.themeVariant` maps Light/Dark to the
  matching (distinct) Avalonia `ThemeVariant` (the KEEP seam, unit level).
- `CatalogueTests.fs` (NEW) — per-kind name/code theory, code/name uniqueness, the
  opticalSign curved-optics classification.
- `MainSceneMsgTests.fs` (NEW) — 74 windowless tests over `TableAndElementRotationView.update`
  arms the existing suite left unpinned: AddElement per catalogue kind (theory) +
  spread/invariance, RemoveSelected edges, Move-bay accumulation/clamps/inertness,
  Render-bay config clamps + scene invariance, RotSetAxis normalization + table/element
  R3-lock semantics, normalizeDegrees theory, confirm-gated resets (arm/cancel/no-op),
  ToggleR3Lock round-trip, wheel zoom clamps (element/table min-max, zoom-all,
  independence), the confirm-gated Library bind (request/confirm/cancel/selection-
  clears-pending/direct-bind), pointer drag-threshold, Experiments host guards
  (unknown/non-numeric ids inert), and the Materials/Library bays' disarm-on-query-
  change discipline (incl. the refused-remove message lifecycle).
- `LocalizationTests.fs` (EXTENDED, +8) — the documented loader tolerance: non-object
  entries / non-string values skipped, non-object root typed error, empty-value
  fallback (active → English → key), sorted completeness report, copyable
  describeError messages.

`OpticalConstructor.Tests` (1 new file, 1 extended, fsproj updated):

- `AppShellLayoutTests.fs` (NEW, 14) — the Avalonia-free `setPanelVisible` reducer:
  hide/show round-trip, unknown-id no-op, order/dock/size preservation, other-panels
  untouched, empty layout, idempotence, duplicated-id semantics, per-default-panel theory.
- `EnvironmentRoundTripTests.fs` (EXTENDED, +18) — schemaVersion stamped + mismatch →
  typed `SchemaValidationError` + `load` fallback, malformed JSON → `JsonParseError`,
  `validate` message payload, `save` creates the settings directory, the documented
  built-in defaults (panel arrangement / preferences / board+theme+language), per-panel
  visibility JSON round-trip (theory), per-theme round-trip (theory), toolbar/palette/
  recent-files/last-folders order round-trips. New opens: `System.Text.Json[.Nodes]`,
  `OpticalConstructor.Storage.Errors`.

## Testing state

Gate execution belongs to the arc-runner's gate engine after this worker exits
(IMPLEMENT Invariant 6). Diagnostic verification only, not gate authority:

- `dotnet build Berreman.slnx -c Release` — **Build succeeded, 0 errors**; the only
  warnings are the pre-existing ones step 001 catalogued in untouched files (FS1125
  SeriesDataTests, FS3873 Dispersion, NU1701 Wolfram.NETLink). No warning originates
  from any file this attempt added or touched.
- `OpticalConstructor.Tests`: **457/457 passed** (baseline 447 → margin +10).
- `OpticalConstructor.Ui.Tests --filter Category=ui-smoke`: **111/111 passed**
  (baseline 108 → margin +3).
- `OpticalConstructor.Ui.Tests --filter Category!=ui-smoke`: **339/339 passed**
  (baseline 329 → margin +10).
- `BerremanTests`: **119 passed, 5 skipped** (the pre-existing skips; baseline 119).
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol`; the five NEW files
  carry zero CR bytes (pure LF).

## Artifacts

None required (no captured logs / screenshots this round; the diagnostic runs are
summarised above).

## Gotchas

- **A worker cannot reset a `count_at_least` baseline.** Attempt 1 declared
  zero-baselines in its SoW YAML expecting them to govern; the gate engine compares
  against the PRIOR green step's checkpoint (`.checkpoints-json`). When a slice
  mandates deleting tests, the same round must land compensating real tests in the
  same suites. This is the whole delta of attempt 2.
- **The compensating tests pin surviving behaviour only** — the launcher (now the
  sole startup surface), the KEEP seams (`themeVariant`, `setPanelVisible`, the
  environment spine), and the Main scene that replaced the shell. Duplication was
  checked against `MainWorkbenchTests` / `ExperimentControlsTests` /
  `TableAndElementRotationTests` before writing; only unpinned arms were added.
- **`RotSetAxis` on an element honours the element's own `r3Locked`** via
  `Placement.withR3` (the table branch checks `tableR3Locked` explicitly) — pinned,
  since the asymmetry is easy to break in a refactor.
- Attempt 1's carried-over gotchas remain valid (touches-list omission of
  OpticalConstructor.Tests; CieView pre-orphaned; AppShell shrank to two functions;
  Ui package references deliberately left; warning sweep deferred to Part N).
- **No operator note was in flight** (the project prompt's Operator note section is
  empty; the retry hint named only the failed gate).

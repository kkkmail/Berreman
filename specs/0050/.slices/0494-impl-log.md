# Step 0494 — impl log

## Progress

- Read the task, both system prompts (shared base and `wire_ui_worker`), the project prompt, rulings R0001–R0005 (each
  confined to steps 0427–0436; the behaviours they preserve — series dispatch, the circular analyzer, the element-id
  provider, the single evaluation and concurrent residual, the single `rhoT` read and the server collector — are not
  touched here) and the slice.
- Read the seams the facts drive: `LauncherWindow` and `KitConstructorWindows.showConstructorIn` (`Program.fs`),
  `AppContext.create`, `KitStartWindow`, `KitWindowView.kitWindowView`, `FlowStripView`, `ConstructorFlowStep.stepsOf`,
  `DataLocationsView.rows`, `TestAppScope` (`fileLocations`, `freshKitFolder`, `kitPickerOf`, `openCommittedReadOnly`,
  `assertCommittedUnchanged`, `readKitResult`), `LauncherTests.clickAndObserve` / `clickBorder`,
  `LibraryWindowTests.clickOn`, `KitWindowTests.withKitWindow` / `pressBox`, `KitAcceptanceTests.closeCommittedReadOnly`.
- (1) Docs, no code. `Program.fs`: the file doc's first paragraph (spec 0038's launcher narrative, six lines) is now four
  lines citing spec 0050 Close: `Startup` reads the settings and locations once, `AppContext.create` composes over them,
  `LauncherWindow` is shown, its Main / Inverse open `KitStartWindow` and its footer `DataLocationsWindow`, a chosen kit
  opens its mode's constructor window (`KitConstructorWindows.showConstructorIn`), which runs the kit window. The FuncUI
  clone paragraph is unchanged. `Startup.workbenchSettings, dataLocations` and `Startup.context` docs are one and two
  lines. `AppContext.fs`: the type doc (the file's only top doc; lines 1–27 are the namespace and opens) is now the one
  app scope composed at the root over the configured locations, injected into the launcher, the start screen and every
  window (spec 0050 Close), plus its unchanged reference-identity line. Neither file says in-memory store, re-seed,
  experiment collection, scene store or a `STORE_XDUO_*` seam (grep: nothing; the remaining "seed" words are
  `ConstructorScene.mount`'s initial-model parameter, which is live code).
- (2) `AGENTS.md`: (a) the `## Build & test` test-locations sentence; (b) `## Project layout`: Domain's and Storage's notes,
  and `BerremanTestSupport/`, `Catalogue/Builtin/`, `OpticalConstructor.Database`, `data/` in the block's column layout;
  (c) the three Gotchas bullets after "The Main scene's Library is LIVE" (6, 6 and 5 lines); (d) the flow-strip
  sentence on "Main ribbon". Every name the new text cites was checked to exist (`LibraryEntryEnvelope`,
  `OpticalConstructorSqlite`, `library.db`, `LibrarySchemaMismatchErr`, `UnsupportedSchemaVersionErr`,
  `KitProxy.create (folder : KitFolder)`, `KitContext`, `KitHistoryLimit`, the five id modules, …).
- (3) `WireUiFinalCompositionTests.fs`: module doc gains two lines citing spec 0050 Close; three `ui-smoke` facts with the
  slice's exact names, over private helpers (`matchesUiId`, `tryControlOf` / `controlOf`, `pressBoxIn` — mouse down/up at
  a box's centre after `BringIntoView`, no release when the press closes the window — `windowsOpenedBy`,
  `clickLauncherButton`, `flowSurfaces`, `pressEveryFlowStep`, `showCommittedReadOnly`). The three earlier facts keep
  their names and assertions.
- Build green; the class runs 6 of 6 in 21.8 s.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` (docs only)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs` (docs only)
- `AGENTS.md`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`

## Testing state

Local runs over the final tree, each with its gate's own command, one after another. The arc-runner's gate engine is
the authority (Invariant 6); these are advisory.

| Run | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; warnings only the exempt `NU1902` and the vendored MathNet's pre-existing `SYSLIB0051` | 89 s | 600 s |
| `WireUiFinalCompositionTests` (class filter) | `Passed!`, 6 of 6 (3 earlier facts + 3 new) | 22 s | — |
| ui-smoke (whole, the gate's selection) | `Passed!`, 274 of 274 = 271 (checkpoint 492) + 3 new | 238 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 (no base, so all 13) | 241 s | 600 s |
| 21 × `invariant-s47-*` | each decoded grep exits 1, finding nothing | — | 300 s |
| 001 §R greps (`ocproj`; the eleven removed names) | both find nothing | — | — |

Not run locally, for the gate engine: constructor-unit-tests, ui-tests, unit-tests, kit-acceptance and elevation-full.
This step changes no code they exercise (two product files change comments only; the test edits are one `ui-smoke`
class), and unit-tests (~52 min) and kit-acceptance (~30 min) whole would only repeat the engine's own runs. No run
exceeded its effective deadline. The four edited files hold no CR byte; `git diff --numstat` equals
`--ignore-cr-at-eol`; `git status` shows nothing under `data/` but the ignored `data/chroma/`.

## Artifacts

- `specs/0050/.artifacts/0494-build.log` — the build.
- `specs/0050/.artifacts/0494-class-run.log` — `WireUiFinalCompositionTests`, 6 of 6.
- `specs/0050/.artifacts/0494-ui-smoke.log` — the whole `ui-smoke` suite, 274 of 274 (TRX under the test project's
  `TestResults/ui-smoke.trx`).
- `specs/0050/.artifacts/0494-elevation.log` — the elevation gate.

## Gotchas

- **The start-up import awaits confirmation on a new library** (`CatalogueImportContext.importOnStart` →
  `CatalogueAwaitsChoices`, spec 0050 §3.7): `AppContext.create` over a fresh file library writes no entry, so
  `tryGetMaterial MaterialIds.silicon` answers `Ok None` straight after it. The slice orders the silicon check before
  the launcher; the code makes the confirmation the launcher's own first-open import dialog. Chosen: fact (a) shows the
  launcher over the unchanged `Ok ctx` (as the slice names it), presses the dialog's Apply (`importApplyButton`), then
  asserts `Ok (Some (BuiltinCatalogue.material MaterialIds.silicon))` — the app's real first-start path, with no
  fixture settling (`TestAppScope.settlePendingImports` would have left the launcher re-offering an already settled
  catalogue).
- `KitAcceptanceTests.closeCommittedReadOnly` is private and `KitAcceptanceTests.fs` / `TestAppScope.fs` are outside
  this step's edits, so `showCommittedReadOnly` writes the same two-line close (`try kit.close` / `finally
  assertCommittedUnchanged`) rather than calling it.
- The read-only Inverse model is mounted with `kitShell = KitWindowShell.inert` set explicitly, as `KitWindowTests`
  mounts its screens; its window is closed by the fact, since the inert shell's `requestClose` does nothing.
- `TableAndElementRotationView` is reached through a module alias `Scene` (the `LibraryWindowTests` precedent) rather
  than an `open`, so the existing facts' names cannot be shadowed.
- A grep over `Berreman/OpticalConstructor` with `-r` walks `bin/` and `obj/` and takes minutes; use `git grep` or the
  Grep tool with a glob.

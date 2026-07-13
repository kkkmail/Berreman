# Step 015 — impl log (ADD_COMPONENT UICOMP_XDUO_0010 LibraryWindow)

## Progress

- [x] Read the task file, the ADD_COMPONENT worker prompt + shared base, the
  project prompt, the slice spec, CLAUDE.md.
- [x] Context: MaterialsWindow{,View}.fs (step-013 precedent),
  FacetedTreeControls.fs (step 012), Facets.fs / FacetBuckets.fs /
  LibraryFacets.fs (steps 009–011), ElementId.fs (LibraryProxy / SampleProxy /
  EntryProtection), WindowLauncher.fs (LibraryWindowKey already declared),
  TableAndElementRotationView.fs (samplesBay / BayNames / EditorLaunchers),
  AppContext.fs, App/Program.fs, the affected Ui.Tests files.
- [x] `LibraryWindowView.fs` (pure MVU): live corpus (SampleProxy samples +
  LibraryProxy presets), the step-011 catalogue + text filter, the seeded
  "By kind" representation, the numeric film-thickness facet (step-010 buckets
  in offers AND tree, manual min–max range), the verbs, the typed
  `LibraryWindowError` refusals, the view panel.
- [x] `LibraryWindow.fs` (HostWindow composition root — Sample editor through
  `WindowLauncher` under `SampleEditorKey`; optional threshold/cap ctor args).
- [x] fsproj compile entries (both files before the workbench scene).
- [x] Workbench: `samplesBay` + `BayNames.library` + the `Smp…` messages/arms
  + the bay-only model fields + `RemoveConfirm<'id>` + the substrate helpers
  + the bay confirm/message/view rows REMOVED; `EditorLaunchers` reshaped
  (`openSampleEditor` moved into the window; `openLibraryWindow` added);
  `OpenLibraryWindow` message + the right-aligned `Library…` strip button.
- [x] App `Program.fs` comment touch-up (the factory threads through
  `defaults` — no code change; step 47 owns the composition acceptance).
- [x] `LibraryWindowTests.fs`: 22 pure + 8 headless proofs (see Testing state).
- [x] Reworked the bay-coupled tests: MainWorkbenchTests (rewritten around the
  strip buttons + roster), MainSceneMsgTests (Smp disarm block retired — the
  window twins live in LibraryWindowTests), LibraryControlsTests /
  ExperimentControlsTests / LayerBandsControlsTests (rosters),
  AppContextTests + WireUiCompositionTests (two-surface / composition proofs
  drive the Library WINDOW), WindowLauncherTests (the sample-Add acceptance
  composes its Browse launcher directly).
- [x] Diagnostic build + all four suites green; LF check clean.

## Files modified

- NEW `Berreman/OpticalConstructor/OpticalConstructor.Ui/LibraryWindowView.fs`
- NEW `Berreman/OpticalConstructor/OpticalConstructor.Ui/LibraryWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` (comment only)
- NEW `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/{MainWorkbenchTests,
  MainSceneMsgTests, LibraryControlsTests, ExperimentControlsTests,
  LayerBandsControlsTests, AppContextTests, WireUiCompositionTests,
  WindowLauncherTests}.fs`

## Decisions

- Corpus read = LIVE `SampleProxy.listSamples` (samples) + `LibraryProxy`
  `entriesForKind` over the four non-sample kinds deduped by `entryId`
  (sources/detectors/polarizers; a compound polarizer serves both polarizer
  kinds, hence the dedup). The slice pins "LibraryProxy … plus SampleProxy for
  the sample verbs"; reading samples through the STATIC read-only proxy would
  make a sample Add invisible to the window, defeating the "verbs operate over
  the shared stores" acceptance — recorded interpretation.
- ONE seeded representation (`by-kind`, the step-011 catalogue order: kind →
  polarizer category → lifted material facets → sample-structural facets). The
  slice names only "By kind"; inventing a second ordering is out of scope.
- The bay's View verb is subsumed by selection: selecting an entry leaf shows
  the view panel (display name — kind, a "protected built-in" note, the
  `fullDescription` prose). The slice enumerates the rewired verbs as Add
  sample / Make multilayer / Edit / Remove only; the Details bay keeps the
  band view (`sampleBandsState` stays in the workbench).
- Remove on a `ProtectedBuiltIn` entry refuses AT THE VERB CLICK (the confirm
  gate never arms for an un-removable entry) with the typed
  `ProtectedEntryRefused reason` naming the entry; a UserManaged non-sample
  (unreachable today) refuses with `PresetNotRemovable` so the protection
  match is total without a throw; a store refusal on confirm surfaces as
  `SampleRemoveRefused of SampleError`.
- Numeric facet (film thickness — the FIRST `FacetedTreeControls` host to
  exercise the numeric path): offers are the step-010 buckets +
  `ManualRangeOffered`; the TREE's numeric branches are buckets too (§D.0 —
  never raw magnitudes); bucket value codes are "lo:hi" invariant round-trip
  strings (':' — an exponent's '-' can never split wrong); the manual box
  accepts "lo-hi" / a single exact value, unparseable commits are no-ops;
  range display text reuses the step-010 `NumericBucket.label` unit switch
  (stripped of its count) rather than re-deriving nm/µm formatting.
- The selected entry rides the `entryId` STRING — the Library domain's uniform
  entry-identity seam (`LibraryEntry.entryId` / `tryGetEntry`; the workbench
  `pendingEntry` precedent) — resolved through the LIVE corpus per pass.
- `EditorLaunchers` reshaped, not paralleled (the step-013 discipline):
  `openSampleEditor` moved into `LibraryWindow`'s own composition;
  `openLibraryWindow : LibraryProxy -> SampleProxy -> MaterialProxy -> unit`
  added; `RemoveConfirm<'id>` deleted with its last instantiation.
- Strip-button order: Materials… docks right FIRST (keeps its step-013
  rightmost position); Library… docks second, immediately left of it.
- `WindowLauncherTests`' sample-Add acceptance now composes the SAME Browse
  launcher the window bakes (factory → `SampleEditorKey minted`) instead of
  the removed `defaults.openSampleEditor` — the proof (launcher + freshness
  routing under the upfront id) is unchanged.

## Testing state

Gates are executed by the arc-runner's deterministic gate engine after this
worker exits (ADD_COMPONENT Invariant 6 — the worker acts, it runs no checks).
The roster: `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`,
`ui-tests`.

Diagnostic verification (not gate authority):

- `dotnet build Berreman.slnx -c Release` — 0 errors, **no MSB3277**, zero
  warnings from the touched projects (Ui / App / Ui.Tests); only the
  step-001-catalogued pre-existing set in untouched files (NU1701 Wolfram ×2,
  SYSLIB0051 vendored MathNet ×2, FS3873 Dispersion, FS1125 SeriesDataTests
  ×4). Log: `015-diag-build.log`.
- ui-smoke **135/135** (checkpoint 131; −4 retired bay headless proofs, +8 new
  window proofs: strip-button single-instance with close-reopen, the by-kind
  tree with per-kind counts, Add-sample end-to-end through the real editor,
  the protected-remove refusal changing nothing, confirm-gated sample remove
  in the same pass, Edit + Make-multilayer through the real editor, the view
  panel, Show/Search gating with zero rows). Log: `015-diag-ui-smoke.log`.
- ui-tests **379/379** (checkpoint 367; −10 retired bay pure tests
  (MainWorkbenchTests ×5, MainSceneMsgTests ×5), +22 new window pure tests).
  Log: `015-diag-ui-tests.log`.
- OpticalConstructor.Tests **571/571** (== checkpoint). Log:
  `015-diag-constructor-tests.log`.
- BerremanTests **119 passed / 5 pre-existing skips** (== checkpoint). Log:
  `015-diag-unit-tests.log`.
- LF check: `git diff --numstat` == `--ignore-cr-at-eol` (no CRLF churn); all
  new files LF-only. (The `.manifest.state.json` CRLF style is
  supervisor-written, pre-existing.)

Nothing deferred; every slice requirement landed this round.

## Gotchas

- **The task file's system-prompt path was stale again** — the ADD_COMPONENT
  worker prompt lives under
  `src/ai_strategy_generator/multistep/add_component_worker.system-md` in the
  tool repo; located and read in full together with the shared base
  `arc-runner.system-md` (the step-007..014 gotcha recurred). The slice's
  "today TestWindows/TableAndElementRotationView.fs:2340" and
  "ElementId.fs:276-281" line references were stale too (the workbench moved
  to Ui at step 003; the Domain file grew) — resolved by symbol, not line.
- **`AppContext.library` / the mock `LibraryProxy` close over the STATIC
  `seedEntries`** — they never see the live samples store. The window's corpus
  therefore merges live samples with proxy presets (see Decisions); any later
  consumer wanting "all live entries" should reuse
  `LibraryWindowView.liveEntries` rather than `entriesForKind` alone.
- **Never pin a numeric assertion on a store thickness magnitude** — layer
  thicknesses round-trip through meters and can sit a few ulp off their
  nominal nm value (the step-011 gotcha); the manual-range tests use ranges
  strictly between rungs ("160-260") and the degenerate-value parse is pinned
  purely, never against the store.
- **`EditorLaunchers` changed shape again**: any out-of-tree substitute record
  must drop `openSampleEditor` and add `openLibraryWindow` (the step-013
  "record changed shape" class of break); `RemoveConfirm<'id>` is gone from
  the workbench module.
- **`LibraryWindowKey` is app-global registry state in tests** — every test
  that opens the real window (directly or via the strip button) must CLOSE it,
  or a later strip click ACTIVATES a stale window over the wrong stores (the
  step-008/013 registry gotcha; the reworked AppContextTests two-surface proof
  closes the first surface's window before the second opens its own).
- **No registry edit was needed**: `.contract-ids/XDUO-json` and
  `specs/0038/.contracts-json` already carry UICOMP_XDUO_0010 (declared,
  step 15) — the supervisor maintains both (the step-007 precedent).
- Step 002–014 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the FuncUI Elmish host skips a
  structurally-equal model — tests refresh via a CHANGING commit; the
  appsettings.json write-back into test output copies is expected).

## Artifacts

- `specs/0038/.artifacts/015-diag-build.log`
- `specs/0038/.artifacts/015-diag-ui-smoke.log`
- `specs/0038/.artifacts/015-diag-ui-tests.log`
- `specs/0038/.artifacts/015-diag-constructor-tests.log`
- `specs/0038/.artifacts/015-diag-unit-tests.log`

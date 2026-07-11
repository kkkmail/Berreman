# Step 015 — impl plan (ADD_COMPONENT UICOMP_XDUO_0010 LibraryWindow)

## Goal

The single-instance Library WINDOW over the WHOLE `LibraryEntry` corpus
(samples ∪ sources ∪ detectors ∪ polarizers) — the step-012
`FacetedTreeControls` instantiated a second time (the step-013 Materials-window
precedent), seeded default representation "By kind" (kind facet first, the
step-011 sample facets, the polarizer-category facet). Verbs rewired from the
retired Library bay: Add sample / Make multilayer / Edit / Remove (editors
through the step-008 `WindowLauncher`; Remove keeps its confirm gate and typed
blocks and REFUSES a `ProtectedBuiltIn` entry with a typed reason). The Library
bay leaves the ribbon; the tab-strip row gains the right-aligned `Library…`
button under `LibraryWindowKey` (already declared in the step-007 `WindowKey`
DU). The window factory threads through `EditorLaunchers.defaults`, so the App
composition is mechanical (step 47 owns the composition acceptance).

## Approach

1. **`LibraryWindowView.fs` (new, pure MVU)** — mirror `MaterialsWindowView`:
   - Corpus per projection pass: LIVE samples from `SampleProxy.listSamples`
     (mapped to `SampleItem`) + the non-sample entries from `LibraryProxy`
     (`entriesForKind` over the four non-sample kinds, deduped by `entryId`) —
     the read must be live for "sample verbs operate over the shared stores"
     to be observable; the static seeded samples inside the read-only
     `LibraryProxy` are excluded in favour of the live store.
   - Facets: `LibraryFacets.libraryFacets` over the LIVE material corpus
     (`MaterialProxy.listMaterials`) + a `textFilterDef` over `displayName`.
   - One seeded representation `by-kind` (the step-011 catalogue order).
   - NUMERIC facet support (first host to exercise it): film-thickness offers
     = step-010 buckets (`bucketsFor`) with `ManualRangeOffered`; the tree's
     numeric facet branches are the buckets, not raw magnitudes; a bucket /
     manual "lo-hi" entry applies as an ordinary `NumericRangeSelection` chip.
   - Verbs: Add sample / Make multilayer (mint `SampleId` at dispatch), Edit
     (samples only — resolved live), Remove (protected → typed
     `ProtectedEntryRefused`, sample → confirm gate → `removeSample`, store
     refusal surfaced inline). Window-local `LibraryWindowError` DU.
   - View panel: the selected entry's metadata (name — kind, protection note,
     `fullDescription`); the bay's View verb is subsumed by selection.
2. **`LibraryWindow.fs` (new, HostWindow composition root)** — mirrors
   `MaterialsWindow`: bakes the sample-editor launcher through
   `WindowLauncher.create` under `SampleEditorKey`, optional
   `TreeAutoBuildThreshold` / `ThicknessBucketCap` ctor overrides.
3. **Workbench (`TableAndElementRotationView.fs`)** — remove `samplesBay`,
   `BayNames.library`, the `Smp…` messages/arms, the bay-only model fields
   (`sampleQuery` / `selectedSample` / `sampleRemoveConfirm` / `samplesError` /
   `viewedSample`), the now-unused `RemoveConfirm<'id>`, the substrate-facet
   helpers, the bay confirm/message/view rows; `EditorLaunchers` drops
   `openSampleEditor` (it moves into the window's context) and gains
   `openLibraryWindow`; `Msg` gains `OpenLibraryWindow`; the strip row gains
   the right-aligned `Library…` button (`WorkbenchIds.openLibraryButton`).
   `sampleBandsState` STAYS (the Details bay uses it).
4. **App** — no code change (the factory rides `defaults`); comment touch-up.
5. **Ui.Tests** — new `LibraryWindowTests.fs` (pure + headless: ids contract,
   by-kind tree with per-kind counts, live-corpus sample verbs over shared
   stores end-to-end, protected-remove refusal, numeric buckets + manual
   range, single-instance strip button). Rework the bay-coupled tests:
   `MainWorkbenchTests` (roster, launcher seam), `MainSceneMsgTests` (drop the
   `Smp…` disarm block), `LibraryControlsTests` / `ExperimentControlsTests` /
   `LayerBandsControlsTests` (bay rosters), `AppContextTests` +
   `WireUiCompositionTests` (drive the Library WINDOW instead of the bay),
   `WindowLauncherTests` (the sample-Add acceptance builds its launcher
   directly instead of `defaults.openSampleEditor`).

## Files to modify

- NEW `OpticalConstructor.Ui/LibraryWindowView.fs`, `LibraryWindow.fs`
  (+ fsproj compile entries before `TableAndElementRotationView.fs`).
- `OpticalConstructor.Ui/TableAndElementRotationView.fs` (bay removal, strip
  button, launcher seam).
- `OpticalConstructor.App/Program.fs` (comment only).
- NEW `OpticalConstructor.Ui.Tests/LibraryWindowTests.fs` (+ fsproj entry).
- `OpticalConstructor.Ui.Tests/{MainWorkbenchTests,MainSceneMsgTests,
  LibraryControlsTests,ExperimentControlsTests,LayerBandsControlsTests,
  AppContextTests,WireUiCompositionTests,WindowLauncherTests}.fs`.

## Risks

- **Baseline counts must not regress** (ui-smoke 131 / ui-tests 367): several
  bay tests are retired — the new window tests must more than replace them in
  BOTH filter buckets. Count after the diagnostic run.
- **The window registry is app-global test state** — every test that opens the
  Library window (directly or via the strip button) must CLOSE it (the
  step-013 gotcha).
- **Numeric range codes must round-trip** through the domain-free control's
  string tokens (use InvariantCulture round-trip formatting, ':' separator so
  exponent minus signs can never split wrong).
- **LF line endings**; zero new warnings under `--warnaserror+:25`.

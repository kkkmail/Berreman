# Step 018 — impl plan (IMPLEMENT, attempt 1)

## Goal

Spec 0038 Part G pre-binding (operator Q7/Q28) + the silent ideal-analyzer retirement:

1. **Pre-bind every new table element except a sample** on the ADD path: the add path binds
   LightSource → `src-600`, Detector → `det-intensity`, LinearPolarizer → `pol-lp`,
   CircularPolarizer → `pol-cp-left` (the generic default); the palette exposes THREE polarizer
   buttons — LP (`pol-lp`), CPL (`pol-cp-left`), CPR (`pol-cp-right`) — with `CatalogueKind`
   UNCHANGED (LP → LinearPolarizer, CPL/CPR → CircularPolarizer); only label + pre-bound entry
   differ. Samples stay UNBOUND (the inverse hook); lens / mirrors have no Library entries to
   bind (not bindable).
2. **Retire the silent IdealLinear fallback**: `runAnalyzerKind`'s
   `Option.defaultValue Library.IdealLinear` tail becomes a typed two-case resolution
   (`ResolvedAnalyzer of PolarizerKind | NoAnalyzerPresent`); the VaryR1+Intensity experiment
   surface reports a typed "no analyzer present" status chart when the scene holds no bound
   polarizer.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `SeedEntryIds` constants module (the five seed ids, centralized).
  - `defaultSeedEntry : CatalogueKind -> string option` (public, testable).
  - `addElement` gains the prebind parameter; new placement gets `valueId = prebind`.
  - `Msg` + `AddElementBoundTo of CatalogueKind * string option`; `AddElement kind` DELEGATES to
    it with `defaultSeedEntry kind` (all ~40 existing test dispatch sites keep compiling and gain
    the pre-bound semantics); the palette handler dispatches `AddElementBoundTo` uniformly.
  - `PaletteButton` record + `paletteButtons : Model -> PaletteButton list` (public): expands the
    model's `CatalogueKind` palette to buttons — `CircularPolarizer` expands to the CPL/CPR pair;
    every other kind keeps its `kindCode`/`kindName` button with the kind's seeded default.
    `paletteState`/`paletteHandlers` rebuilt over it (ids `PaletteAdd_LP/CPL/CPR/...`).
  - `AnalyzerResolution` DU + `runAnalyzerKind` public, typed tail; `noAnalyzerTitle` /
    `noAnalyzerStatus` constants; the VaryR1+Intensity chart arm reports the typed status.
- `OpticalConstructor.Controls/ElementPaletteControls.fs` — expected NO change (the control is
  generic over `AddItem`s; the host grows the buttons). Confirm and record.
- Tests (`OpticalConstructor.Ui.Tests`):
  - `MainSceneMsgTests.fs` — pure: each `AddElement` kind lands pre-bound to its seed id (and the
    id resolves through the live proxy for the kind); sample + lens/mirrors stay unbound;
    `AddElementBoundTo` explicit CPR path; `paletteButtons` mapping (three polarizer buttons,
    unchanged kinds, distinct codes, sample unbound).
  - `ExperimentControlsTests.fs` — pure: `runAnalyzerKind` typed resolutions (varied bound
    polarizer / last scene polarizer / none → `NoAnalyzerPresent`); the VaryR1+Intensity chart
    reports the typed no-analyzer status with NO series; a circular-only scene resolves circular
    (never IdealLinear).
  - `TableAndElementRotationTests.fs` — headless (ui-smoke): the palette renders the three
    polarizer buttons with stable ids; clicking CPL/CPR/LP/Sample adds elements with the right
    kind + pre-bind (sample unbound).

## Risks

- Pre-binding changes the semantics of every existing `AddElement LinearPolarizer` test fixture
  (input Stokes becomes polarized; analyzer resolves from the bound entry). Reviewed all suites:
  no exact-value curve assertions depend on the unbound default; the step-017 acceptance tests
  select the SEEDED detector (untouched by this step — the seeded initial elements are NOT
  re-bound; the slice scopes pre-binding to the ADD path).
- `experimentState`/`experimentResult` now yields the typed status chart for a VaryR1+Intensity
  draft in a polarizer-less scene (previously a silent Malus curve); reviewed the tests touching
  that path — none assert a series there except via bound-polarizer fixtures.
- New `Msg` case: `update` gains the arm; no other exhaustive matches over `Msg` exist.

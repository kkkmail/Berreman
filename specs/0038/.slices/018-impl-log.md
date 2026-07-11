# Step 018 — impl log (IMPLEMENT, attempt 1)

## Progress

- [x] Recon: slice, spec Part G, step-017 SoW + machinery, palette/add path, runAnalyzerKind,
      full test-fallout sweep (AddElement fixtures, readout/boundName assertions).
- [x] Production: TableAndElementRotationView.fs (pre-binding, palette buttons, analyzer DU).
- [x] Tests: MainSceneMsgTests (+10), ExperimentControlsTests (+5), TableAndElementRotationTests (+1).
- [x] Diagnostic build + all four suites (build clean; 119 / 571 / 149 / 413).
- [x] SoW + exit.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `SeedEntryIds` constants module — the five seeded-entry ids (`src-600`, `det-intensity`,
    `pol-lp`, `pol-cp-left`, `pol-cp-right`) centralized as `[<Literal>]`s so the add path, the
    palette buttons, and the tests never repeat the literals.
  - `defaultSeedEntry : CatalogueKind -> string option` (public) — the pre-binding map:
    LightSource → src-600, Detector → det-intensity, LinearPolarizer → pol-lp,
    CircularPolarizer → pol-cp-left (the generic default; the CPR button overrides), Sample →
    None (the inverse hook), Lens / FlatMirror / CurvedMirror → None (no Library entries exist
    for those kinds — not bindable).
  - `addElement` gained the `prebind : string option` parameter; the fresh placement is
    `{ ElementPlacement.create … with valueId = prebind }`.
  - `Msg` + `AddElementBoundTo of CatalogueKind * string option`; the `AddElement` arm DELEGATES
    to it with `defaultSeedEntry kind` (the step-017 `BindValueIdTo` delegation precedent —
    `update` was already `let rec`), so every existing `AddElement` dispatch site gains the
    pre-bound semantics with no signature churn.
  - `PaletteButton` record + public `paletteButtons : Model -> PaletteButton list` — expands the
    model's `CatalogueKind` palette to buttons: `CircularPolarizer` expands to the CPL / CPR pair
    (codes `CPL` / `CPR`, labels "Circular polarizer (L)/(R)", explicit `pol-cp-left` /
    `pol-cp-right` pre-binds, kind UNCHANGED); every other kind keeps one `kindCode`/`kindName`
    button carrying its seeded default. `paletteState` / `paletteHandlers` rebuilt over it; the
    handler dispatches `AddElementBoundTo (b.kind, b.prebind)` uniformly. Button automation ids
    are `PaletteAdd_LP` / `PaletteAdd_CPL` / `PaletteAdd_CPR` / `PaletteAdd_<kindCode>`.
  - `AnalyzerResolution` DU (`ResolvedAnalyzer of Library.PolarizerKind | NoAnalyzerPresent`) +
    `noAnalyzerTitle` / `noAnalyzerStatus` literals; `runAnalyzerKind` is PUBLIC now and returns
    the DU — the varied element's bound polarizer kind, else the last bound scene polarizer,
    else `NoAnalyzerPresent`; the `Option.defaultValue Library.IdealLinear` tail is GONE (no
    `IdealLinear` fallback remains anywhere in the Ui project — grep-verified).
  - The VaryR1 + Intensity chart arm matches the resolution: `NoAnalyzerPresent` yields
    `{ ExperimentChart.empty with title = noAnalyzerTitle; description = noAnalyzerStatus }`
    (the material-error-status precedent — no series synthesized from a fallback), which the
    Experiments bay surfaces through `experimentState.description`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MainSceneMsgTests.fs`
  - step-018 section: theory — each `AddElement` kind (S/D/LP/CP) lands pre-bound to its seed id
    AND the id resolves through the live Library seam to an entry valid for the kind; theory —
    Sample/Lens/FlatMirror/CurvedMirror stay unbound; `AddElementBoundTo` explicit CPR path;
    `paletteButtons` mapping (LP/CPL/CPR trio over unchanged kinds, unique codes, unbound sample
    button, every non-circular button carries `defaultSeedEntry`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`
  - step-018 section: varied-element-wins resolution (LP varied while CPR is last in scene);
    circular-only scene resolves circular (never an assumed linear); no bound polarizer →
    `NoAnalyzerPresent` (varied + un-varied); the VaryR1+Intensity chart reports the typed
    status with NO series (and the bay state carries the same status text); the pre-bound
    polarizer add still yields the Malus curve.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/TableAndElementRotationTests.fs`
  - step-018 headless (ui-smoke): the palette's LP/CPL/CPR/Sa buttons are located by their
    stable `PaletteAdd_` automation ids and CLICKED; the four adds land with the unchanged
    kinds `[LinearPolarizer; CircularPolarizer; CircularPolarizer; Sample]` and the bindings
    `[pol-lp; pol-cp-left; pol-cp-right; unbound]`.
- `OpticalConstructor.Controls/ElementPaletteControls.fs` — NO change needed: the control is
  generic over `AddItem`s (id + label) and the host grows the buttons; the buttons are already
  keyed (`View.withKey`) with AutomationIds and label-keyed re-subscription, so the two extra
  same-kind buttons are safe under FuncUI recycling.

## Decisions

- **Pre-binding lives on the ADD path, not the seeded initial scene.** The slice scopes it to
  "the palette add path"; `initMainWith`'s seeded src/det elements stay unbound. Rationale: the
  step-016/017 acceptance tests (strip / Choose… flows) prove their bind flows over the seeded
  UNBOUND detector — retro-binding it would weaken those acceptance proofs — and the unbound
  seeded elements behave identically through the experiment runners (600 nm / Intensity
  defaults). "Every new table element" = elements created through the add path. Recorded as the
  chosen interpretation (spec G0's sentence is realized by the add-path mapping).
- **`AddElement` delegates, `AddElementBoundTo` commits** — one add site (`addElement`), the
  step-017 one-commit-arm precedent. The generic `AddElement CircularPolarizer` defaults to the
  LEFT seed (`pol-cp-left`, the seed order); only the palette's CPR button carries the explicit
  right-circular pre-bind.
- **The palette expansion is model→buttons data** (`paletteButtons`), not a Controls change: the
  Model keeps `palette : CatalogueKind list` (zero churn on `initWith`/fixtures), and the
  CPL/CPR pair exists only at the button layer — exactly "only the palette label and the
  pre-bound entry differ".
- **`AnalyzerResolution` is a named two-case DU** (no `option`): `NoAnalyzerPresent` IS the typed
  'no analyzer present' status; the chart surfaces it via the material-error-status precedent
  (title + description on an empty chart). `runAnalyzerKind` went public so the
  no-silent-fallback discipline is provable without a window (the `selectorOffer` precedent).
- **Pre-bind ids are committed blind** (no proxy validation at add time): `boundEntry` already
  treats an unresolvable `valueId` as unbound everywhere downstream, and the seed ids are
  guaranteed by the Domain seeds the composition re-seeds at every start (spec §0.2). The pure
  theory asserts the ids resolve through the live seam, so drift would fail tests, not users.

## Testing state

Diagnostic verification only — gate execution belongs to the arc-runner's deterministic gate
engine after this worker exits (IMPLEMENT Invariant 6).

- `dotnet build Berreman.slnx -c Release`: 0 errors, no MSB3277; the only warnings are the
  step-001-catalogued pre-existing set in untouched files (NU1701 Wolfram ×2, SYSLIB0051
  vendored MathNet ×2, FS3873 Dispersion, FS1125 SeriesDataTests ×4, FS0044 ChartWindow).
  Zero warnings from the touched projects (Ui / Ui.Tests).
- ui-tests: **413/413** (checkpoint 398, +15). ui-smoke: **149/149** (checkpoint 148, +1).
- OpticalConstructor.Tests: **571/571** (== checkpoint). BerremanTests: **119 passed /
  5 pre-existing skips** (== checkpoint).
- Line endings: `git diff --numstat` ≡ `--ignore-cr-at-eol --numstat` (no CRLF churn;
  read-only check). The `.manifest.state.json` CRLF warning is the arc-runner's own
  pre-existing working-copy state, untouched by this round.

## Artifacts

- `specs/0038/.artifacts/018-diag-build.log`
- `specs/0038/.artifacts/018-diag-ui-tests.log`
- `specs/0038/.artifacts/018-diag-ui-smoke.log`
- `specs/0038/.artifacts/018-diag-constructor-tests.log`
- `specs/0038/.artifacts/018-diag-unit-tests.log`

## Gotchas

- The task file's system-prompt path was stale AGAIN (`C:\GitHub\AI-Strategy-Generator\
  implement_worker.system-md` does not exist); the IMPLEMENT worker prompt lives at
  `src/ai_strategy_generator/multistep/implement_worker.system-md` — the step-007..017 gotcha
  recurred.
- The slice's line references are stale; resolved by symbol: `seedEntries` is
  `ElementId.fs:602-611` (not 535-540), `runAnalyzerKind` lived at the relocated
  `Ui/TableAndElementRotationView.fs:1444-1464` (not `TestWindows/...:1407-1427`);
  `Placement.fs:93-101` (`CatalogueKind`) is still accurate.
- **Pre-binding changes the semantics of every `AddElement LinearPolarizer` fixture** in the
  suites (the input Stokes becomes polarized through the bound `pol-lp` behaviour; the analyzer
  resolves from the bound entry). Swept all suites before implementing: no exact-value curve
  assertion depended on the old unbound defaults, and the step-016/017 acceptance fixtures bind
  the SEEDED detector (still unbound at init — see Decisions).
- **A VaryR1+Intensity draft in a polarizer-less scene now yields the typed status chart**
  (empty series + `noAnalyzerTitle`/`noAnalyzerStatus`) where it silently produced an
  IdealLinear Malus curve before. `openChartWindowHook` guards on non-empty series, so the
  pop-out window correctly refuses to open for it; a test pinning a series there must bind a
  polarizer first (the pre-bound add does it by default).
- A `ConstantMueller`-behaved polarizer still resolves to no ideal kind (`idealKindOf` = None,
  the documented step-014 skip) — with this step it falls through to `NoAnalyzerPresent` instead
  of the silent linear default; no seed or editor can produce one yet (Part G's recorded
  future-rework note on `idealKindOf` stands).
- Step 002–017 carried-over gotchas remain valid (baselines from `.checkpoints-json`; the FuncUI
  Elmish host skips structurally-equal models; a Select-window test must close its window; the
  appsettings write-back into test output copies is expected).

# Impl log — spec 0033, slice 023 (ADD_COMPONENT UICOMP_XDUO_0004 MaterialEditorWindow)

## Progress

- [x] Read task file, worker system prompt (add_component_worker + the
      arc-runner base), project prompt, slice spec, the 022 SoW/impl-log
      (ADD_COMPONENT precedent), spec-md Part F, MaterialLibrary.fs
      (MaterialComplexity/MaterialEntry/MaterialProxy), DispersionModels.fs
      (DispersionModel/toEpsAxis + the published FB/BB coefficient sets),
      Berreman/Dispersion.fs (eps/rho/mu value trees), OpticalProperties/
      Active.fs (gyration assembly), Ui/Validation.fs:92, NkDispersionChart.fs,
      ExperimentChart.fs (ChartStyle.dataBounds), ExperimentControls.fs (the
      inline chart-canvas precedent), SampleEditorView/Window + tests,
      SampleStackEditor.fs (the Domain edit-model precedent), TestApp.fs, the
      four fsprojs.
- [x] Red: MaterialEditorWindowTests.fs + its fsproj entry added first; the
      build fails with 100 × FS0039 naming the missing
      `MaterialComplexityEditor` / `MaterialEditorView` / `MaterialEditorWindow`
      symbols (`023-red-tdd.log`).
- [x] Domain: MaterialComplexityEditor.fs (pure ladder state + message DU +
      Result apply + toComplexity/ofComplexity + the constrained gyration
      offering + the restated gain rule) — compiles clean, 0 warnings.
- [x] TestWindows: MaterialEditorView.fs (pure MVU + FuncUI view, inline
      dual-axis preview canvas) + MaterialEditorWindow.fs (HostWindow
      composition root) + the fsproj entries.
- [x] Green: all five gates pass in the advisory local runs; 27 new tests
      (18 pure `ui-tests` + 9 headless `ui-smoke`), no test edits after red.
- [x] Contract registry: `UICOMP_XDUO_0004` is supervisor-maintained (the
      016/022 precedent) — nothing to write.
- [x] LF policy verified (see Gotchas).
- [x] State-of-the-world written.

## Decisions

1. **The ladder is lossless BY CONSTRUCTION.** The Domain state stores every
   facet independently of its unlock toggle, and `toComplexity` reads a facet
   only while its toggle is on. Unchecking therefore restores that aspect's
   default without touching anything else, and re-checking restores the
   user's edits — the acceptance is a structural property, not a special
   case. Pinned both purely (derived-complexity equality across four toggle
   cycles) and headlessly (the summary/digest readout returns to its initial
   text).
2. **Three axis slots, always stored.** `index1..3 : ComplexRefractionIndex`
   and three per-segment model slots exist regardless of the anisotropy
   choice; the choice decides how many are READ (isotropic 1; uniaxial
   (ordinary, extraordinary) from 1–2; biaxial 1–3). Switching anisotropy
   never destroys entered values, and `ofComplexity` seeds unused slots
   sensibly (uniaxial seeds slot 3 = ordinary, the engine's (o, e, o) shape).
3. **Non-lowerable models are pickable; the derivation is honest.** The
   mandated picker includes ForouhiBloomer / BrendelBormann, but those are
   NOT finite term sums, so `toEpsAxis` cannot lower them into the
   serializable segment tree (`EpsAxisDispersion` is term DATA; a complexity
   cannot carry a closure). Rejecting the pick would discard the user's
   entry, so the pick is ACCEPTED and `toComplexity` returns the typed
   `SegmentNotLowerable` carrying the engine's `NotAFiniteTermSum` reason —
   surfaced through the derived-model readout, blocking Save with the reason
   (Part G's unsupported-formula precedent: an honest negative, not a stub).
   The raw `SumOfTerms` escape hatch is the identity under lowering.
4. **`ofComplexity` seeds VERBATIM; only messages snap.** Enabling activity
   (or switching anisotropy under an unlocked activity rung) snaps an
   un-offered gyration class to the choice's first offered class — the
   Part F "the anisotropy choice constrains the gyration-class picker" rule.
   Seeding never snaps (no clamping, §0.4), so
   `toComplexity (ofComplexity c) = Ok c` value-identically for every
   editable built-in (pinned over glass152 / uniaxial / biaxial / EUV-Mo /
   activeCrystal — the last carrying `PlanarActive`, a class the picker does
   not offer but the state preserves).
5. **The gyration offering follows the slice enumeration exactly:**
   isotropic ⇒ `CubicActive` (23/432); uniaxial ⇒ `UniaxialActive` (the
   diagonal two-component form shared by 3/4/6 and 32/42/62); biaxial ⇒
   `Orthorhombic222` / `Monoclinic2` / `MonoclinicM` / `Triclinic1`.
   `PlanarActive` is not offered (the slice enumerates the offerings
   exhaustively); seeded values are preserved per Decision 4. The "activity
   toggle REMOVED for non-rotating choices" mandate is implemented
   structurally: the toggle renders only while `availableGyrationClasses`
   is non-empty — every current choice offers rotating classes (the engine's
   `GyrationClass` deliberately holds only rotation-producing classes), so
   the rule is a live constraint with no dead branch to exercise today.
6. **The gain rule is RESTATED in Domain, not imported and not moved.** The
   spec (Part F) wants the Ui validation helpers to "move with the editors
   where project layering requires", but Ui is not in this slice's
   `touches`, and neither Domain nor TestWindows may reference Ui (the 021
   `validateRepeatCount` precedent; Ui would also drag WebView2/Plotly/
   ScottPlot into TestWindows). `imaginaryIndexGainWarning` is restated in
   `MaterialComplexityEditor` with identical semantics (finite ∧ k < 0) and
   message text, doc-linked to Ui/Validation.fs:92; the real-move is left
   for a slice that touches Ui.
7. **The preview embeds the step-19 chart MODEL, rendered inline.** The
   view builds `NkDispersionChart.nkDispersionChart` (over
   `complexity.toProperties`, 400–700 nm, nm display unit) paired with
   `nkDispersionStyle`, and renders it as an inline dual-axis canvas — the
   `ExperimentControls` inline-chart-canvas approach — with per-side y-bounds
   from the 018 `ChartStyle.dataBounds` (n on the left axis, k on the
   right, so neither flattens the other). The pop-out `ChartWindow` is a
   native ScottPlot surface and cannot be embedded in a re-rendering FuncUI
   view. The gain warning reads the chart's sampled k series.
8. **View-only INSIDE the component too.** MaterialsControls (015) already
   removes the Edit verb for view-only rows; the window itself also opens
   `complexity = None` entries (silicon/langasite/vacuum) — and entries
   whose complexity cannot seed (dispersive rho/mu formulas, typed
   `UnsupportedComplexity`) — in a view-only mode carrying the WHY: no
   ladder, no Save affordance (REMOVED, not greyed), preview over the
   entry's own preset properties, Close only.
9. **Save re-derives both fields.** `complexity = Some model` with
   `properties = model.toProperties` — the seed-time sync invariant restated
   at save time; `NewMaterial` mints via `newMaterialId ()`
   (`MaterialId.create`), `ExistingMaterial id` updates in place; the window
   closes only on `Ok`, a proxy `Error` surfaces its typed reason.
10. **Mandated-literal placement.** Segment 0's model picker carries the
    mandated `DispersionModelPicker` literal; later segments carry the
    derived `DispersionModelPicker_<i>` family (the 022 `RepeatCountStepper`
    precedent of one mandated literal + a derived family).
11. **AutomationId on every ladder control, `Name` only on the fixed
    header/readout controls** — the ladder subtrees change membership on
    every toggle and FuncUI cannot rename a recycled styled control (the
    022 precedent). All rows are WrapPanels (label+box pairs as single wrap
    items) — the 022 headless-font-metrics lesson, applied from the start;
    no off-screen-click failures occurred.
12. **Component declared, not wired** (ADD_COMPONENT obligation): no parent
    view, launcher, or Ribbon bay references MaterialEditorWindow; the
    headless tests construct the window directly over fresh in-memory
    proxies.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs`
  (new) — the pure ladder: `Anisotropy` / `Transparency` / `DispersionChoice`
  / `ActivityChoice` / `MagneticChoice` / `MuKind` / `PrincipalAxisSlot` DUs,
  `EditSegment` (interval + three axis-model slots),
  `MaterialComplexityEditState`, the typed
  `MaterialComplexityEditError`, the 17-arm `MaterialComplexityMsg` +
  `applyMaterialComplexityMsg`, `toComplexity` / `ofComplexity`,
  `availableGyrationClasses` + class/model codes + labels,
  `defaultModelChoices` (published FB a-Si / BB gold sets), defaults, and
  the restated `imaginaryIndexGainWarning`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
  — the `<Compile>` entry after SampleStackEditor.fs.
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorView.fs`
  (new) — `UiIds` (13 mandated literals + derived option/box families),
  `EditorTarget` / `EditorMode` / `MaterialEditorContext` / `Model` / `Msg`,
  `init` / `update` / `complexitySummary` (case + aspects + structural
  digest), the FuncUI view (identity rows, anisotropy options, unlock
  toggles, index/k fields, per-segment editor with bounds + model picker +
  add/remove, constrained gyration panel + handedness switch, Polder-mu
  panel with axis picker, the inline dual-axis n/k preview canvas, gain
  warning / summary / status readouts, Save/Cancel), and the view-only
  branch (note + preview + Close).
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorWindow.fs`
  (new) — the `HostWindow` composition root: injects the context
  (`requestClose = this.Close`), sets the `MaterialEditorWindow`
  Name/AutomationId, runs the Elmish loop.
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/OpticalConstructor.TestWindows.fsproj`
  — the two `<Compile>` entries after SampleEditorWindow.fs.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`
  (new) — 18 pure contract tests (`ui-tests`): the mandated UiIds; the
  default derivation; biaxial-absorbing / uniaxial constant cases; the
  dispersive toggle round trip; segment add/edit/remove + typed index
  errors; Sellmeier lowering = `toEpsAxis`; FB/BB pickable-but-typed-error;
  SumOfTerms identity; the constrained class offering; the activity snap +
  handedness; the Polder-mu derivations; the four-toggle lossless
  acceptance; the built-in `ofComplexity` round trip; the gain rule; init
  seeding (new / existing / view-only); Save add/update/Cancel through
  recording stubs; failing-save keeps open. 9 headless proofs (`ui-smoke`)
  driving the REAL window by UiIds: every mandated id present (ladder ids
  after their rungs unlock); the four slice acceptance criteria; Save
  updating an existing entry in place; the gain warning appearing/clearing;
  view-only silicon (no Save, no ladder); the segment editor + non-lowerable
  pick surfacing its reason.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — `<Compile Include="MaterialEditorWindowTests.fs" />` after
  SampleEditorWindowTests.fs.

## Testing state

TDD: red first (100 × FS0039 naming the missing production symbols,
`023-red-tdd.log`), then the Domain module, then the view + window, then
green — no shipped test was edited after red. All five gates in the slice
roster pass in the worker's local ADVISORY runs (per Invariant 6 the
arc-runner gate engine is the sole gate authority and re-runs them after
exit):

- `build` — `dotnet build Berreman.slnx -c Release` exit 0, 0 errors (the
  93 warnings are the pre-existing NU190x/NU1701/MSB3277/SYSLIB0051 noise
  plus the pre-existing engine Dispersion.fs:205 FS3873; none from the new
  files).
- `unit-tests` — BerremanTests 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 407 passed, 0 failed (= baseline; the new
  Domain module's tests live in Ui.Tests, the 022 precedent).
- `ui-smoke` — 76 passed, 0 failed (67 baseline + 9 new headless proofs).
- `ui-tests` — 294 passed, 0 failed (276 baseline + 18 new pure tests).

Acceptance checks (inside the runs): choosing biaxial exposes three
principal-index fields (1 → 2 → 3 boxes across the anisotropy options);
enabling activity on a uniaxial medium offers ONLY the uniaxial gyration
class option (cubic/planar/222/monoclinic/triclinic absent); unchecking the
absorbing and dispersive toggles restores the initial derived-model readout
(and the pure test pins derived-complexity equality across all four toggle
cycles, including re-check restoring the edits); Save round-trips through
MaterialProxy — a new biaxial (1.6/1.7/1.8) entry lands with
`complexity = Some` (eps case + stored tensors verified), and an existing
glass entry updates in place keeping `complexity = Some`.

## Artifacts

- `specs/0033/.artifacts/023-red-tdd.log` — red build (FS0039 × 100).
- `specs/0033/.artifacts/023-build.log` — solution build.
- `specs/0033/.artifacts/023-unit-tests.log` — BerremanTests run.
- `specs/0033/.artifacts/023-constructor-unit-tests.log` — constructor tests.
- `specs/0033/.artifacts/023-ui-smoke.log` — ui-smoke run.
- `specs/0033/.artifacts/023-ui-tests.log` — ui view tests.

## Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` does not
  exist (same drift as slices 015–022); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\add_component_worker.system-md`
  and was read from there.
- No `## Operator note` content is in flight for this attempt (the project
  prompt's heading is present but empty).
- **ForouhiBloomer / BrendelBormann cannot land in a `MaterialComplexity`.**
  The serializable eps tree holds term DATA and those models are
  transcendental (`toEpsAxis` → `NotAFiniteTermSum`). The mandated picker
  offers them; picking one is accepted and the typed reason surfaces through
  the derived-model readout, blocking Save honestly (Decision 3). A future
  slice that wants them editable needs a closure-carrying complexity case —
  a spec change, not a worker liberty.
- **"The activity toggle is REMOVED for non-rotating choices" is currently
  vacuous-but-live**: every anisotropy choice offers at least one rotating
  class, so the toggle always renders today; the removal rule is
  data-driven off `availableGyrationClasses` (Decision 5), not a dead code
  path someone must remember to add.
- The spec's "validation helpers move with the editors" (Part F) could not
  be honoured as a REAL-MOVE this round: Ui is outside `touches`. The gain
  rule is restated in Domain (Decision 6) — flagging for the operator in
  case a later slice should perform the move and re-point Ui.
- **Headless summary equality needs a structural digest.** The lossless
  acceptance observes the `MaterialComplexitySummary` readout, which embeds
  `hash complexity` — two states with the same summary text denote the same
  derived complexity (in-process structural hash; deterministic within the
  test run, not across runs — fine for an equality probe).
- **Line endings:** the three edited fsprojs show ZERO CRLF churn
  (`git diff --numstat` identical with and without `--ignore-cr-at-eol`);
  the four NEW `.fs` files are LF on disk (0 CRLF sequences), matching the
  `.gitattributes` policy.
- `.manifest.state.json` (modified) and the untracked `.claude/` folder are
  the arc-runner's / harness's own files (same as slices 001–022), left
  alone.

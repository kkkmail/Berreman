# Impl plan — spec 0033, slice 022 (ADD_COMPONENT UICOMP_XDUO_0003 SampleEditorWindow)

## Goal

Declare the Sample editor window component in `OpticalConstructor.TestWindows`:
a FuncUI/Elmish window over the step-21 `SampleStackEditor` (Domain) that edits
a sample's name / description / `SubstrateKind` / film stack, chooses materials
by `MaterialId` from `MaterialProxy.listMaterials`, and persists through
`SampleProxy` (add for new — minting `SampleId.create` — / update for
existing). Ship it with headless semantic-tree tests that drive the window by
its stable UiIds. Do NOT wire it into any parent view / launcher (ADD_COMPONENT
obligation).

## Approach

Follow the TestWindows View + Window pair convention (ElementMovementView /
SnapToBeamWindow precedent), with the Controls-layer styling idioms
(SampleLibraryControls clickBox / verbButton / AutomationId-in-variable-lists).

1. **`SampleEditorView.fs`** (new, `module OpticalConstructor.TestWindows.SampleEditorView`)
   - `UiIds` module: the 13 slice-mandated `[<Literal>]` ids plus derived-id
     prefix functions (`SampleLayerRow_<i>[_<j>]`, `SampleGroupRow_<i>`,
     `SampleGroupExpander_<i>`, `RepeatCountStepper{Plus|Minus}[_<i>]`,
     `SampleMaterialOption_<guid>`, `SampleLayerThickness_…`,
     `SampleLayerOrientation_…`, `SampleSubstrateKind_…`, plus my own fixed
     ids for the thickness box, description box, φ/θ/ψ boxes, films-count
     readout, QWOT readout, status line, clear-selection and move buttons).
   - `SampleEditorContext` (`[<ReferenceEquality>]`): `samples : SampleProxy`
     + `requestClose : unit -> unit` — the functional-proxy/Context seam; the
     pure update calls through it at Save/Cancel (tests substitute stubs).
   - `EditorTarget = NewSample | ExistingSample of SampleId` (no naked bool).
   - `Model`: context, target, name, description, substrate, `editor :
     SampleStackEditState`, materials list, chosen material, collapsed-group
     set, thickness/QWOT/φ/θ/ψ texts, fold count (toolbar stepper, min 1),
     status (typed-error reason surface).
   - `init context materials (existing : Sample option)`; `toSample id model`.
   - Pure helpers: `isAnisotropicEntry` (evaluate the entry's tensors at a
     600 nm reference via the engine `getEps`/`getProperties` path — eps/mu
     off-diagonal or unequal diagonal, or ANY nonzero rho), `refractionIndexAt`
     (n = Re[√ε₁₁], the NkDispersionChart extraction), `qwotThickness`
     (t = λ/(4n) through `Thickness.nm` — canonical metres), `thicknessLabel`,
     `filmsCount` (expandedFilms length).
   - `update : Msg -> Model -> Model` — routes the bulk verbs through
     `applySampleStackMsg`; typed `SampleStackEditError` reasons land in
     `status`. View-level arms: row-click toggle multi-select (Set add/remove —
     the Domain DU has no per-position deselect), AddLayer (structure append —
     the Domain DU has no Add arm; Domain is not in this slice's `touches`),
     per-layer orientation editor (single-position `SetOrientationOfSelected`
     with the selection restored), group expander toggle, steppers, Save
     (add/update through the context; `Ok` → `requestClose`), Cancel
     (`requestClose`, no write).
   - `view` — DockPanel: name/description + substrate facet + material picker
     (top); bulk toolbar, QWOT row, status, Save/Cancel row (bottom; Save
     positive green / Cancel negative red, one row); centre = the stack table:
     each `Repeated` group is ONE collapsible super-row (rotating-triangle
     expander — RotateTransform 90° when expanded) with an inline repeat-count
     stepper (`SetRepeatCount`) and its unit-cell layers nested (indented)
     beneath; single layers are plain rows; every row shows material /
     thickness cell / orientation summary; the per-layer orientation editor
     (φ/θ/ψ degree boxes) is INCLUDED only for anisotropic materials — absent,
     not greyed, for isotropic ones; plus a films-count readout
     (`SampleFilmsCount`) so "the structure expands to 2*K films" is visible in
     the semantic tree.

2. **`SampleEditorWindow.fs`** (new): `type SampleEditorWindow(materials :
   MaterialProxy, samples : SampleProxy, existing : Sample option)` inheriting
   `HostWindow`; sets Name + AutomationId `SampleEditorWindow`; resolves the
   material choices ONCE via `materials.listMaterials` (composition root);
   builds the context with `requestClose = this.Close`; runs
   `Program.mkSimple init update view |> Program.withHost this`.

3. **fsproj**: register both files at the end of the TestWindows compile list.

4. **`SampleEditorWindowTests.fs`** (new, Ui.Tests) + fsproj entry — TDD red
   first (FS0039 on the missing module). Pure contract tests (`ui-tests`):
   UiIds literals, init shapes, toSample, toggle-select, AddLayer, fold-count
   clamp, typed-error → status, isAnisotropicEntry over the built-ins, QWOT
   derivation, Save/Cancel through recording stub contexts (add for new /
   update for existing / cancel writes nothing). Headless tests (`ui-smoke`):
   mount the REAL window over in-memory proxies and drive by UiIds —
   (a) every mandated id present; (b) acceptance: add 2 layers, select both,
   step the fold count to K=3, MakeRepeatBlock → films readout 6 = 2*K, then
   the group's inline stepper 3→4 → 8; (c) acceptance: select-by-material +
   bulk set-thickness updates ONLY matching rows' thickness cells;
   (d) acceptance: Save persists a NEW sample through `SampleProxy.addSample`
   and closes; (e) Save UPDATES an existing sample in place (count unchanged);
   (f) Cancel discards and closes; (g) the orientation editor exists for an
   anisotropic layer and is ABSENT for an isotropic one; (h) QWOT entry derives
   the read-only canonical-metres thickness and SetLayerHeight applies it.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/SampleEditorView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/SampleEditorWindow.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/OpticalConstructor.TestWindows.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`

Contract registry: `specs/0033/.contracts-json` already records
`UICOMP_XDUO_0003` as `declared` / `declaringStep: 22` (supervisor-maintained;
the 016 precedent) — nothing for the worker to write.

## Risks

- FuncUI recycling ("Cannot set Name … already styled") on the
  variable-membership stack rows — mitigated by AutomationId (never `Name`) on
  every row-level control, the SampleLibraryControls / ExperimentControls
  precedent; the Elmish window re-renders on every message, so this is load-bearing.
- Headless text entry has no in-repo precedent; the tests set the found
  TextBox's `Text` property (fires the property-changed subscription FuncUI's
  `onTextChanged` binds), which stays "drive by UiIds".
- One `RepeatCountStepper` literal but two stepper surfaces (toolbar fold count
  for MakeRepeatBlock; inline per-group for SetRepeatCount): the toolbar
  stepper carries the mandated literal; the inline ones carry the derived
  `RepeatCountStepper…_<group>` family. Recorded as a decision.
- Anisotropy for dispersive presets (silicon / langasite, `complexity = None`)
  is only decidable by evaluating tensors at a reference wavelength — 600 nm,
  the repo's common reference; recorded as a decision.

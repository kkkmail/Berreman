# Step 010 — impl-log

Slice: `010.slice-md` — IMPLEMENT. Expose the sample's geometry-constrained
`supportedEmission` (step-9 Domain field) in the Sample editor (spec 0040 Part D.2):
a `Plate` editor gets R/T checkboxes constraining emission to R-only or T-only
(never neither, marking dirty); a `ThinFilm` editor shows R fixed on + disabled and
no T control.

## Progress

- [x] Read system prompt, project prompt, slice spec, surrounding code + step-9 SoW.
- [x] Confirmed the FuncUI 2.0 CheckBox/ToggleButton/Button/ContentControl/InputElement
      DSL surface by reflection (all generic base-module attrs; `ContentControl.content`
      also proven in-repo at `EmbeddedChart.fs:59`).
- [x] Model `supportedEmission` field + `SampleEditSnapshot`/`currentEdit` + `init`
      seed/normalise + `toSample` carry.
- [x] `SetSubstrate` re-constraint + `SetReflectedEmission`/`SetTransmittedEmission`
      Msg + update arms (route through `Emission.withReflected`/`withTransmitted` then
      `constrainEmission`).
- [x] `emissionCheck` / `emissionRow` view + wired into the top facet panel.
- [x] `UiIds.SampleEditor.emitReflectedCheck` / `emitTransmittedCheck`.
- [x] Pure `update` tests + headless `ui-smoke` tests + id-contract assertions.
- [x] State-of-the-world.
- [x] **Attempt 02 — build fix.** The first attempt failed the `build` gate on two
      `FS0039` errors (`InputElement.isEnabled`, `ToggleButton.isChecked` not defined).
      Root cause + fix below.

## Files modified

- `OpticalConstructor.Ui/SampleEditorView.fs` — Model `supportedEmission`,
  `SampleEditSnapshot`/`currentEdit` (5-tuple), `init` (seed + `constrainEmission`
  normalise + `initialEdit`), `toSample` (carry `m.supportedEmission`), `SetSubstrate`
  re-constraint, `SetReflectedEmission`/`SetTransmittedEmission` Msg + update arms,
  `emissionCheck` + `emissionRow` view, wired after `substrateRow`.
- `OpticalConstructor.Controls/UiIds.fs` — `emitReflectedCheck` / `emitTransmittedCheck`.
- `OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs` — `clickControl` / `checkBox`
  helpers, `threeFilmPlate` fixture, 2 new id-contract asserts, 4 pure `update` facts,
  2 headless `ui-smoke` proofs.

## Attempt 02 — the build failure and its fix

**What failed (attempt 01).** The `build` gate reported exactly two `FS0039`
errors in `SampleEditorView.fs`, both inside the new `emissionCheck` view:

```
SampleEditorView.fs(732,22): error FS0039: ... 'isEnabled' is not defined.
SampleEditorView.fs(733,22): error FS0039: ... 'isChecked' is not defined.
```

i.e. `InputElement.isEnabled` and `ToggleButton.isChecked`. The sibling lines
`ContentControl.content` (734) and `Button.onClick` (735) compiled — a decisive
asymmetry that pinned the root cause.

**Root cause.** FuncUI 2.0-preview1 does NOT expose attributes as plain module
functions; it augments each Avalonia **control class** with static members for its
own AND inherited attributes (confirmed by reading the DLL metadata: the members
compile under names like `ToggleButton.isChecked.Static`, `InputElement.isEnabled.Static`,
`Button.onClick.Static`, `ContentControl.content.Static`). To write `X.attr`, the
CLASS `X` must be IN SCOPE. This file opens `Avalonia.Controls` but neither
`Avalonia.Controls.Primitives` (home of `ToggleButton`) nor `Avalonia.Input` (home
of `InputElement`). So `ContentControl` / `Button` / `Border` / `CheckBox` (all in
`Avalonia.Controls`) resolve and carry their augmented attributes, while
`ToggleButton` / `InputElement` do not resolve as class names → `FS0039`.

**Fix.** Route all four attributes through the in-scope `CheckBox` class, which
inherits every one of them (`CheckBox : ToggleButton : Button : ContentControl`,
and `Control : InputElement`). Base-class static augmentation members surface
through a derived class name — the live `Border.isEnabled` precedent (line 634,
`isEnabled` from the `InputElement` base) proves it. So:

- `InputElement.isEnabled enabled` → `CheckBox.isEnabled enabled`
- `ToggleButton.isChecked isOn`    → `CheckBox.isChecked isOn`
- `ContentControl.content label`   → `CheckBox.content label`   (was OK; unified for clarity)
- `Button.onClick (…)`             → `CheckBox.onClick (…)`     (was OK; unified for clarity)

No other file, type, test, or behaviour changed — the model / snapshot / `init` /
`toSample` / update arms / `emissionRow` and the tests were all already correct and
compiled; only the four attribute qualifiers were wrong.

**Verification.** `dotnet build Berreman.slnx -c Release` → `Build succeeded. 0 Error(s)`.
The only two warnings are `NU1701` for the third-party `Wolfram.NETLink` package
(exempt per CLAUDE.md); zero warnings originate in our code. The edited file is pure
LF (`od -tx1 | grep ' 0d'` → 0 CR bytes).

## Testing state

Per IMPLEMENT worker Invariant 6, gate certification belongs to the arc-runner's
deterministic gate engine, which runs `build` / `unit-tests` /
`constructor-unit-tests` / `ui-smoke` / `ui-tests` after exit. This retry
additionally BUILT the solution to confirm the reported `FS0039` build failure is
resolved (a build failure cannot be addressed blind):

- **build** — VERIFIED green: `dotnet build Berreman.slnx -c Release` →
  `Build succeeded. 0 Error(s)`, only the exempt `NU1701` Wolfram advisory.
- **unit-tests** — solver core untouched; count unaffected.
- **constructor-unit-tests** — Domain/Storage untouched; count unaffected.
- **ui-smoke** — 2 net-additive headless facts (`[<Trait Category=ui-smoke>]`);
  count rises. The R/T boxes are loop-free by construction (one-way `CheckBox.isChecked`
  from the model + user-only `CheckBox.onClick`; no `onIsCheckedChanged` feedback path),
  so the [[funcui-numberbox-render-loop]] hazard does not apply.
- **ui-tests** — 4 net-additive pure facts + 2 id-contract asserts on an existing test;
  count rises.

`commit_ready: true`.

## Artifacts

None (UI + test change; no captured logs/screenshots).

## Gotchas

- **FuncUI attribute qualifiers must name an IN-SCOPE control class (the attempt-01
  build break).** FuncUI 2.0-preview1 augments each Avalonia control *class* with static
  members for its own + inherited attributes; `X.attr` only resolves when the class `X`
  is opened. This file opens `Avalonia.Controls` (→ `CheckBox` / `Border` / `Button` /
  `ContentControl` resolve) but NOT `Avalonia.Controls.Primitives` (`ToggleButton`) or
  `Avalonia.Input` (`InputElement`). So write CheckBox attributes as `CheckBox.isChecked`
  / `CheckBox.isEnabled` / `CheckBox.content` / `CheckBox.onClick` — NOT
  `ToggleButton.isChecked` / `InputElement.isEnabled`. Base-class attributes surface
  through the derived class name (the `Border.isEnabled` precedent). Do not `open` the
  extra namespaces just to name a base module — it invites `Vector3`-style collisions.
- **Real FuncUI `CheckBox`, wired loop-free.** The known ui-smoke hazard is a two-way
  `isChecked`/`onIsCheckedChanged` binding that re-fires on the render's programmatic
  `IsChecked` set (the [[funcui-numberbox-render-loop]] class of bug). Mitigation:
  `ToggleButton.isChecked` is bound ONE-WAY from the model and the dispatch rides
  `Button.onClick`, whose `Click` fires ONLY on user activation (pointer/keyboard), never
  on a programmatic set. The handler dispatches `not <render-time value>`, so there is no
  feedback path. `CheckBox` declares only `create`; all its attrs come from generic
  base-module DSL functions — the `ContentControl.content` precedent is live at
  `EmbeddedChart.fs:59`.
- **`SetSubstrate` re-constrains emission (a deliberate scope decision).** The slice does
  not name this, but without it a Plate constrained to T-only then flipped to ThinFilm
  would leave the R box unchecked — violating "ThinFilm shows R fixed on" and desyncing
  the dirty snapshot from what `toSample` persists. `SetSubstrate` now routes the emission
  through `constrainEmission kind`, keeping the model the single consistent source. A pure
  test pins the flip.
- **`init` normalises the seed emission.** `EditSample s` runs `s.supportedEmission`
  through `constrainEmission s.substrate` at load, so an inconsistent input sample (a
  ThinFilm carrying `EmitBoth`, e.g. the step-8 `plateMissingSubstrate`-style fixture) is
  corrected on open rather than shown in a false state.
- **`Placement.Emission` is qualified, not opened.** `Placement` is NOT opened in the view
  (`open System.Numerics` already brings `Vector3` in, which `Placement.Vector3` would
  collide with). The static setters are `Placement.Emission.withReflected` /
  `withTransmitted`, matching the existing `Placement.CatalogueKind.Sample` precedent;
  instance members (`.emitsReflected` / `.emitsTransmitted`) need no qualifier. `Library`
  IS opened, so `defaultSupportedEmission` / `constrainEmission` stay unqualified.
- **Emission excluded from the versioning payload (unchanged from step 9).** `Sample` is an
  in-memory library value, not a serialized contract, and `SampleStore.payloadOf` derives
  nothing from `supportedEmission`; carrying the edited value through `toSample` needs no
  store or schema change.

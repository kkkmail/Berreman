# Step 011 — impl-plan

## Slice

Spec 0040 Part D.4 (step 011, IMPLEMENT). Let a **film-less Plate** set its
substrate plate by picking a material through the Materials window in **Select**
state, targeted at the substrate slot. Depends on step 10.

The gap: the substrate plate (`SampleStructure.substrate`) could previously only
be set "from chosen" — the toolbar's chosen material. A film-less Plate has **no
film rows**, and the per-layer *Choose material…* verb is the only way to reach
the Materials window and choose a material — so a film-less Plate could never
pick a substrate material. This slice gives the substrate slot its own pick verb.

## Approach / files

1. **`OpticalConstructor.Domain/WindowMode.fs`** — add a third `SelectionTarget`
   case `SampleSubstrateTarget` beside `SampleLayerTarget`. The substrate is a
   single, un-positioned slot, so the case carries **no payload** (a sample holds
   ONE substrate plate). Extend the DU doc comment.

2. **`OpticalConstructor.Ui/MaterialsWindowView.fs`** + **`LibraryWindowView.fs`**
   — both `targetText` functions match `SelectionTarget` exhaustively; add the
   `SampleSubstrateTarget` banner-prose arm to each (else FS0025 breaks the build).

3. **`OpticalConstructor.Ui/SampleEditorView.fs`**
   - `Msg`: replace `SetSubstrateClicked` (its only trigger, the substrate
     'Set from chosen' button, is being replaced) with
     `BindMaterialToSubstrate of MaterialId` — the TARGETED return of a substrate
     Select session (mirrors `BindMaterialToLayer`, minus the position).
   - `update`: remove the `SetSubstrateClicked` arm; add the
     `BindMaterialToSubstrate` arm — build the plate from the picked material's
     current version (`MaterialVersionId.firstOf`, `defaultLayerThickness`,
     `PrimaryAxes`) and apply `SampleStackMsg.SetSubstrate (Some layer)`; the
     picked id also becomes the toolbar's chosen material (the `BindMaterialToLayer`
     precedent). Setting a substrate always succeeds — the totality's Error arm is
     the defensive status-line path; a closed editor makes the dispatch a no-op.
   - Add `chooseMaterialForSubstrate` (mirrors `chooseMaterialForLayer`): compose a
     `SelectionContext<MaterialEntry>` with `target = SampleSubstrateTarget`,
     `onSelected` baking `BindMaterialToSubstrate entry.id`, both outcomes
     re-querying via `RefreshMaterials`; call `m.context.openMaterialsSelect`.
   - Add `setSubstrateVerb` (mirrors `chooseMaterialVerb`): a verbButton-styled
     Border that resolves the owner window from the click's visual tree and opens
     the Select session. It reuses `UiIds.SampleEditor.setSubstrateButton` and is
     ALWAYS enabled (it IS the pick — no `hasChosenMaterial` gate).
   - `halfSpacesRow`: replace the substrate 'Set from chosen' `verbButton` with
     `setSubstrateVerb m dispatch`. The lower half-space row is untouched (out of
     scope — it keeps its chosen-gated button).

4. **`OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`**
   - Headless (`ui-smoke`): a film-less **Plate** opens the Materials Select window
     from the substrate Set… verb (banner names the substrate), picks glass 1.75,
     and the substrate summary shows it.
   - Pure (`ui-tests`): `BindMaterialToSubstrate` sets `structure.substrate` to the
     picked material and records it as the chosen material.
   - Extend the UiId contract fact with `setSubstrateButton`.

## Risks

- **Exhaustive-match build break.** Adding a `SelectionTarget` case breaks BOTH
  `targetText`s (FS0025 → warning-as-error class). Both fixed in step 2.
- **Owner-window resolution.** The verb must resolve its owner `Window` at click
  time from the visual tree (`TopLevel.GetTopLevel`) exactly like
  `chooseMaterialVerb`; a plain `verbButton` (unit-onClick) can't. Modelled on the
  proven precedent.
- **Dead-code removal.** `SetSubstrateClicked` is referenced nowhere but its
  (replaced) button; removing it with its arm keeps `update` exhaustive and avoids
  a dead message. Verified no test references it.

## Gates

build, unit-tests, constructor-unit-tests, ui-smoke, ui-tests — all in the 011
roster. Touched: Domain + Ui + Ui.Tests.

# Impl plan — spec 0033, slice 021 (IMPLEMENT)

## Goal

A pure, Avalonia-free Domain edit model `SampleStackEditor` over
`SampleStructure` (ElementId.fs:103), mirroring the message-DU discipline of
`StackEditor.StackMsg` / `applyStackMsg` (OpticalConstructor.Ui/StackEditor.fs:161,176)
but at the material-id level: a state record `{ structure : SampleStructure;
selection : Set<LayerPosition> }` and one message DU whose arms are the eleven
operations the slice enumerates. Distinct from the untouched
`StackEditor.groupLayers` (StackEditor.fs:69) — the existing `OpticalSystem`
editor is not modified.

## Approach

1. **NEW `OpticalConstructor.Domain/SampleStackEditor.fs`** (compiled after
   `Propagation.fs`; depends only on `ElementId.fs`'s `Library` module,
   `MaterialLibrary.MaterialId`, and `Berreman.Media.Thickness`):
   - `LayerPosition = AtSingleLayer of itemIndex : int | AtCellLayer of
     itemIndex : int * cellIndex : int` — a selection address is either a
     top-level `SingleLayer` slot or ONE cell slot of a `Repeated` group
     (a repetition adds no position: editing the cell edits every repetition).
   - `SampleStackEditState = { structure; selection }` with
     `ofStructure` (empty selection).
   - `SampleStackEditError = InvalidRepeatCount | SelectionNotFoldable |
     NotARepeatGroup`, each `of reason : string` (errors as values, typed,
     reason-carrying). `InvalidRepeatCount` enforces the SAME `count >= 1`
     rule as `Validation.validateRepeatCount` (Ui/Validation.fs:56) — Domain
     cannot reference Ui, so the rule is restated here, not imported.
   - `SampleStackMsg` with the eleven arms from the slice, and
     `applySampleStackMsg : SampleStackMsg -> SampleStackEditState ->
     Result<SampleStackEditState, SampleStackEditError>` (uniform Result;
     selection/edit arms always `Ok`, the two repeat arms carry the typed
     rejections).
   - Move up/down via the standard pinned-frontier sequential-swap algorithm,
     per container (top-level `films` list; each selected group's `cell`),
     returning a full old→new index permutation so the selection follows the
     moved layers (and `AtCellLayer` group indices survive top-level moves).

2. **NEW `OpticalConstructor.Tests/SampleStackEditorTests.fs`** — every
   message arm unit-tested without a window, including both acceptance
   criteria: `MakeRepeatBlock` over 2 selected layers with count K expands to
   `2*K` films (`SampleStructure.expandedFilms`), and `SelectByMaterial` →
   `SetThicknessOfSelected` updates exactly the matching layers.

3. Register both files in their `.fsproj` compile lists.

## Semantics decisions (recorded in the impl-log too)

- `SelectLayer` ADDS a valid position (multi-select needs accumulation;
  `ClearSelection` resets); an invalid position is a no-op, per the
  StackEditor out-of-range precedent. `SelectByMaterial` REPLACES the
  selection with all matching positions (films single layers + period cells).
- Structure edits keep the selection valid: set-thickness/material/orientation
  preserve shape; `RemoveSelected` and `MakeRepeatBlock` clear the selection;
  moves remap it; `SetRepeatCount` leaves cell shape (positions stay valid).
- `RemoveSelected` on the last cell layer of a group drops the whole group
  (an empty cell expands to nothing).
- `MakeRepeatBlock` folds only a contiguous run of TOP-LEVEL single layers;
  cell positions / groups / gaps / empty selection → `SelectionNotFoldable`.
- Selection scope is the films stack only — the message set has no
  substrate/lower arms (deferred, as in the slice's vocabulary).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/SampleStackEditor.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (register)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SampleStackEditorTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (register)

## Risks

- Move-with-selection remapping across mixed (top-level + in-cell) selections
  is the only nontrivial algorithm — covered by dedicated tests (pinned-at-top
  no-op, selection-follows, cell moves affecting every repetition).
- Baselines to not regress: berreman 119 / constructor 377 / ui-smoke 59 /
  ui-tests 263. This slice only ADDS constructor tests.

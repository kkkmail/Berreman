# Impl plan — slice 008 (IMPLEMENT — full-surface workbench bays, reordered LAST)

## Goal

Materials and Library become **full-surface** bays: when active, each fills the whole
area below the ribbon tab strip with its own content and shows **no table canvas / no
pan-zoom-rotate gestures**; every other ("table") bay keeps its canvas + gestures. Also
move Materials and Library to be the **last two** bays in `mainBays` and `BayNames.all`,
retiring the Details-LAST pin and the spec-0033 G2 deferred-reorder note (now safe because
step 007 made ribbon pane hosting order-independent — one keyed slot per active bay).

## Approach

Carry a **two-case content-mode DU** (`BayContentMode = InRibbonPane | FullSurface`,
never a bool) on the generic `Ribbon.Bay` — the slice's first sanctioned option. It is a
LAYOUT mode (where a bay's content is placed), not a coupling to any control's state/message
types, so the ribbon stays generic.

- `InRibbonPane`: the ribbon hosts the bay's content in its keyed content pane below the tab
  strip (the small "control" bays — unchanged step-007 behaviour).
- `FullSurface`: the ribbon shows ONLY the bay's tab (no in-ribbon pane); the HOST fills the
  surface below the strip with the bay's content. This is why the content is rendered exactly
  ONCE (no "styled element already styled" double-realization).

## Files to modify

- `OpticalConstructor.Controls/Ribbon.fs` — add `BayContentMode` DU + `mode` field on `Bay`;
  in `view`, render the keyed pane only for an `InRibbonPane` active bay.
- `OpticalConstructor.TestWindows/TableAndElementRotationView.fs` —
  - `BayNames.all`: reorder Materials & Library LAST; drop the G2/Details-LAST comment.
  - `mainBays`: reorder to match + tag each bay's `mode` (Materials/Library `FullSurface`,
    rest `InRibbonPane`).
  - `mainControlBar`: take the pre-built `bays` (built once in `mainView`).
  - `mainView`: build bays once; below the strip render the table canvas + gestures for an
    in-pane bay, or the active full-surface bay's content (no canvas/gestures) for a
    full-surface bay.
- `OpticalConstructor.Ui.Tests/RibbonPaneTests.fs` — the synthetic bays get `mode = InRibbonPane`.
- `OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs` — the literal bay-order assertion
  follows the new order.
- `OpticalConstructor.Ui.Tests/LayerBandsControlsTests.fs` — the Details-LAST assertion becomes
  "Materials & Library LAST; Details still immediately after Experiments".
- `OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs` — NEW headless ui-smoke proof: a table bay
  keeps its canvas; Materials/Library are the last two, full-surface, with no table canvas.

## Risks

- Double-realization crash if a full-surface bay's content were rendered in BOTH the ribbon
  pane and the host fill area → avoided: the ribbon renders NO pane for a full-surface bay.
- Order-pinning tests (`ExperimentControlsTests`, `LayerBandsControlsTests`) must be updated to
  the new order — they are in `touches`. `LibraryControlsTests` only checks the first 5 bays
  (unchanged) so it needs no edit.
- Adding a `mode` field to `Ribbon.Bay` is a breaking record change → update both construction
  sites (`mainBays`, `RibbonPaneTests`).

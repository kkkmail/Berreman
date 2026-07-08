# State of the world — slice 008 (IMPLEMENT — full-surface workbench bays, reordered LAST)

## Where we are

Slice 008 is the closing IMPLEMENT step of arc 0035. Step 007 reshaped the generic
`OpticalConstructor.Controls.Ribbon` to host only the ACTIVE bay's content in one keyed slot
(order-independent pane hosting). This slice builds on that to make the two step-024 workbench
bays — **Materials** and **Library** — **full-surface** bays: when one is active it fills the
whole area below the ribbon tab strip with its own content and shows **no table canvas and no
pan/zoom/rotate gestures**, while every other ("table") bay keeps its canvas and gestures. It
also moves Materials and Library to be the **last two** bays in both `mainBays` and
`BayNames.all`, retiring the Details-LAST pin and the spec-0033 G2 deferred-reorder note (now
safe because step 007 made pane hosting order-independent). It touches
`OpticalConstructor.Controls` (the generic ribbon), `OpticalConstructor.TestWindows` (the Main
host), and `OpticalConstructor.Ui.Tests` (the headless proofs) — matching the slice `touches`.

## What's working

- Add a two-case bay content-MODE DU (`Ribbon.BayContentMode = InRibbonPane | FullSurface`,
  never a bool) on `Ribbon.Bay`; the generic ribbon renders its keyed content pane ONLY for an
  in-pane bay and shows just a tab for a full-surface bay.
- Make the Materials and Library bays full-surface; each fills the surface below the ribbon
  strip with its workbench content and wires no table gestures, in place of the table canvas.
- Move Materials and Library to the LAST two bays in `mainBays` and `BayNames.all`; retire the
  Details-LAST pin and the deferred-reorder comment.
- Wrap the ribbon tab strip (`WrapPanel`) so all nine bay tabs stay on-screen; the reorder had
  pushed the last tabs off the window's right edge, making Materials / Library unclickable.
- Key the full-surface content slot by bay name so switching Materials ⇄ Library recreates the
  content instead of recycling a styled, named row control across the two workbenches.
- Add a headless MainWorkbench ui-smoke proof: a table bay keeps its canvas, the two workbenches
  are the last two full-surface bays with no table canvas, and returning to a table bay restores
  it.

## Tests

Per the IMPLEMENT worker role (Invariant 6 — the worker acts and runs no checks), the
`build` / `unit-tests` / `constructor-unit-tests` / `ui-smoke` / `ui-tests` gates are executed by
the arc-runner's deterministic gate engine AFTER this worker exits; they were NOT run here. The
counts below are the EXPECTED post-round baselines.

- NEW `ui-smoke` proof in `MainWorkbenchTests` (`headless acceptance: Materials and Library are
  the LAST two full-surface bays with no table canvas, while a table bay keeps its canvas`) —
  drives the real Main Elmish loop by UiIds; asserts canvas presence per bay mode and the
  workbench-last order. `ui_smoke_tests` 90 → 91.
- `ExperimentControlsTests` and `LayerBandsControlsTests` order assertions updated (in place, not
  added/removed) to the reordered `BayNames.all` and the retired Details-LAST pin — they still
  pass and the `ui_tests` count is unchanged.
- `RibbonPaneTests` synthetic bays carry `mode = InRibbonPane` (the ribbon's keyed-pane property
  under test is the in-pane path); the two step-007 keyed-slot proofs are otherwise unchanged.
- `LibraryControlsTests` only asserts the FIRST five bays (`List.truncate 5`), which the reorder
  leaves untouched, so it needed no edit.
- No `berreman_unit_tests` / `constructor_unit_tests` file changed; those baselines cannot
  regress.

## Architecture

- **The content-mode is LAYOUT data on the generic `Bay`, not a coupling to any control's
  types.** `InRibbonPane` says "the ribbon hosts this content in its keyed pane"; `FullSurface`
  says "the ribbon shows only the tab and the HOST fills the surface below the strip." The ribbon
  still knows nothing of tables, rotations, or workbenches — it just honours a placement flag, so
  its genericity (the step-007 invariant) is preserved. Mapping by bay name in the host was the
  alternative; carrying the mode on `Bay` keeps the placement decision co-located with the bay
  and lets the ribbon suppress the pane, which is what avoids double-realizing a full-surface
  bay's content.
- **A full-surface bay's content is realized EXACTLY once.** The ribbon renders no in-pane slot
  for it (`paneChildren = []`), and the host places the same `IView` in the fill area below the
  strip. If both had rendered it, FuncUI would hit "Cannot set Name : styled element already
  styled" — the step-007 failure mode.
- **The full-surface fill slot is KEYED by bay name (`View.withKey`).** The reorder makes
  Materials and Library adjacent, and the sweeps switch between them directly; keying the slot
  makes FuncUI CREATE a fresh pane on a Materials ⇄ Library switch rather than patch one
  workbench's `StackPanel` (with its named rows) into the other's — the same keyed-slot
  discipline step 007 gave the ribbon pane, now applied to the host's full-surface slot. The
  table-bay canvas slot stays UNKEYED so it patches in place across table-bay switches (unchanged
  behaviour and no needless canvas rebuilds).
- **`mainBays` is built once in `mainView`** and shared by the ribbon (`mainControlBar` now takes
  the pre-built list) and the fill area, so the active bay's content is a single instance.

## Deferred

- Nothing from this slice is deferred; the reorder + full-surface behaviour fully land this
  round. There is no "round 2".
- The Main readout line stays in the ribbon strip for all bays (it is part of the top strip, not
  the full-surface area). Hiding it for workbench bays was out of scope and untested; left as-is.

## Gotchas

- **The ribbon tab strip MUST wrap (retry-02 root cause).** Moving Materials / Library to be the
  LAST two of nine bays overflowed the non-wrapping horizontal tab `StackPanel` past the window's
  right edge, so the last tab(s) were present but had their centre OFF-window and could not be
  clicked — the headless `clickOn` landed on empty space and never dispatched `SelectBay`, so the
  bay never switched and its content never rendered. This looked like a full-surface render bug but
  was purely tab reachability: Library (last) failed in every window; Materials failed only in the
  narrow 820-px `MainConstructorWindow`. Fix: the tab container is a `WrapPanel`, so tabs flow onto
  a second row and every bay tab stays on-screen. Any future bay addition is safe against overflow.
- **FuncUI diffs children by INDEX, not by key-matching across siblings.** `Differ.diffContentMultiple`
  is a `MapIndexed`, and `diff` returns `KeyDidChange` when `ViewKey` differs, which the patcher
  turns into an in-place REPLACE (`collection[i] <- create …`). So a keyed slot recreates cleanly
  whether or not it shares a children list with unkeyed siblings — the attempt-01 `belowStrip`
  (keyed full-surface bay beside the unkeyed docked strip) was already correct, and a speculative
  "isolate the keyed slot" rewrite was unnecessary and was reverted.
- **The mode is DATA on `Bay`, never derived from the bay name inside the ribbon.**
  `RibbonPaneTests` deliberately includes a synthetic bay literally named "Materials" and asserts
  its keyed pane IS realized — proving the generic ribbon must not special-case any name. Only the
  Main host (`mainBays`) maps names → modes.
- **Do not render a full-surface bay's content in both the ribbon pane and the fill area** — that
  double-realizes the same named controls and throws "styled element already styled". The ribbon's
  `paneChildren` is empty for a full-surface active bay precisely to keep it single.
- **Adding `mode` to `Ribbon.Bay` is a breaking record change**: every construction site
  (`mainBays`, `RibbonPaneTests`) must specify it — F# records have no default field value.
- **Two order-pinning tests had to move with the reorder** (`ExperimentControlsTests` full-order
  literal; `LayerBandsControlsTests` Details-LAST). They live in `OpticalConstructor.Ui.Tests`
  (in `touches`) and were updated in place, so no test-count regresses.
- **MSB3277 (WindowsBase version conflict) is pre-existing and out of scope** — it originates in
  `OpticalConstructor.Ui.fsproj`, which this slice does not touch; the edits change no project
  references, so they introduce no new MSB3277.

## Changelog

- 2026-07-07 — Added `Ribbon.BayContentMode` (`InRibbonPane | FullSurface`) + a `mode` field on
  `Ribbon.Bay`; the ribbon renders its keyed content pane only for an in-pane active bay and shows
  just a tab for a full-surface bay. Made Materials and Library full-surface bays that fill the
  surface below the ribbon strip (no table canvas / gestures), moved them to the LAST two bays in
  `mainBays` and `BayNames.all`, and retired the Details-LAST pin / G2 deferred-reorder comment.
  `mainView` builds the bays once, gates the table canvas + gestures on the active bay's mode, and
  keys the full-surface fill slot by bay name. Updated `RibbonPaneTests`,
  `ExperimentControlsTests`, `LayerBandsControlsTests`, and added a `MainWorkbenchTests` headless
  ui-smoke proof of the full-surface (no-canvas) behaviour and workbench-last order.
- 2026-07-07 (retry 02) — Fixed the `ui-smoke` gate (6 red → all green). Root cause was NOT the
  full-surface below-strip: the reorder pushed the Materials / Library tabs to the overflowing right
  end of the non-wrapping horizontal tab strip, off the window edge, so their clicks missed and the
  bays never switched. Made the ribbon tab strip a `WrapPanel` (tabs flow to a second row, always
  reachable) with a 4-px bottom margin per tab for wrapped-row spacing, and reverted the speculative
  `mainView` keyed-slot isolation back to the attempt-01 form (FuncUI's index-based child diff made
  it unnecessary). Local gates after the fix: build 0 errors, ui-smoke 91, ui-tests 324,
  constructor-unit-tests 440.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 440
  ui_smoke_tests: 91
  ui_tests: 324
```

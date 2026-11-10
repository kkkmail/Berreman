# Step 0257 — impl plan (slot roles in light order, spec 0050 S8.2)

## Approach

Derive each setup slot's role (Source, Prep k, Obj k, Analyzer k, Detector k; 001 §J) from its kind and the
path the one ray law lays it out on, never storing it.

In `OpticalConstructor.Domain/Setups.fs`, after `Setup`:

1. `SlotPosition of int` with `.value` — a slot's 1-based place within its role.
2. `SlotRole` — `SourceSlotRole | PrepSlotRole | ObjectSlotRole | AnalyzerSlotRole | DetectorSlotRole`, the last
   four carrying a `SlotPosition`; `.text` renders the chip name (`Source`, `Prep k`, `Obj k`, `Analyzer k`,
   `Detector k`). No wire tag: a role is never stored.
3. `RoledSlot = { role : SlotRole; slot : SetupSlot }`.
4. `type SetupSlot with static member inLightOrder` — lays the slots out with `BeamLayout.layout` over their
   `layoutElement`s (first slot = ray root), pairs each slot with its `PlacedElement` (layout keeps input order),
   orders them once globally: reached slots (the root, or a non-empty path) by path length (stable, so table order
   breaks ties), then unreached slots (empty path, not the root) in table order. Each role then filters that order:
   `LightSource` → source; `Sample` → object; `Detector` → detector; any other kind → analyzer when a step of its
   path is a `Sample` slot, else prep. Roles are numbered from 1 and concatenated source, preps, objects,
   analyzers, detectors.
5. `type Setup with member this.lightOrder` = `SetupSlot.inLightOrder this.slots`.

Tests in `SetupTests.fs`: the three facts the slice names, over test-local slots between
`RayModel.defaultSourcePoint` (x −1 m) and `defaultDetectorPoint` (x 1 m).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Setups.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupTests.fs`

## Risks

- ELEV: a new public signature must stay elevated (`SetupSlot list -> RoledSlot list`); `SlotPosition` is born in
  a `let` inside the numbering lambda (no call-site rewrap). New names checked unique across the tree and STL.
- The root's path is `[]` like an unplaced slot's, so "reached" must treat the root separately.

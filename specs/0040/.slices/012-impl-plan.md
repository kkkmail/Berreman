# Step 012 — impl-plan

## Goal (spec 0040 Part D.3)

A placed sample's default `Placement.emission` must derive from the **bound
sample's** geometry-constrained `supportedEmission` (step-9 Domain field), not the
generic per-kind `Placement.defaultEmission` (which gives every non-mirror element
`EmitBoth`). The experiment's default `MeasurementMode` already reads
`e.placement.emission` through `MeasurementMode.ofEmission` (`ExpChooseElement`,
`TableAndElementRotationView.fs:1142`), so once the placement's emission tracks the
bound sample, the experiment default follows for free.

Net effect required: a placed **ThinFilm** sample defaults to `EmitReflectedOnly`
and its experiment to `CaptureReflected`; a **Plate** supporting both defaults to
`EmitBoth` / `CaptureBoth`.

## Approach

The sample-binding seam is the Ui's `valueId` write. A `Sample` element is placed
UNBOUND (`defaultSeedEntry Sample = None`) and bound later, so the emission has to
be (re)seeded at BOTH `valueId`-write points:

1. `addElement` (the palette / add-to-table flow that builds the `Placement`) — for
   the pre-bound case (`AddElementBoundTo (_, Some id)`).
2. `BindValueIdTo` (the single targeted commit arm every bind path converges on,
   `TableAndElementRotationView.fs:1107`) — the realistic add-then-bind flow.

### Files to modify

- **`OpticalConstructor.Domain/ElementId.fs`** (Library module) — add a pure
  `placementEmissionForEntry (fallback : Emission) (entry : LibraryEntry) : Emission`
  that returns a bound `SampleItem`'s `supportedEmission` and keeps the `fallback`
  for every non-sample entry. Names the D.3 rule in ONE pure, unit-testable place.
- **`OpticalConstructor.Ui/TableAndElementRotationView.fs`** — a private
  `withBoundSampleEmission (m : Model) (p : ElementPlacement)` that resolves
  `p.valueId` through `m.library.tryGetEntry` and seeds the emission via
  `Library.placementEmissionForEntry`; apply it in `addElement` and `BindValueIdTo`.
  No change to the experiment path (it already reads `placement.emission`).
- **`OpticalConstructor.Tests/PlacedSampleEmissionTests.fs`** (new; registered in
  the `.fsproj`) — pure `placementEmissionForEntry` facts (ThinFilm → R-only, Plate
  → both, non-sample → fallback), the composed `MeasurementMode.ofEmission` default,
  and an end-to-end MVU drive (`AddElement Sample |> bind → ExpChooseElement`) for
  both geometries.
- **`OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`** — one headless fact:
  a placed+bound ThinFilm sample's `placement.emission = EmitReflectedOnly` and its
  experiment draft defaults to `CaptureReflected` (plus the Plate/both mirror case).

## Risks

- **Regression on explicitly-set measurements.** The existing bound-sample
  experiment tests all call `ExpChooseMeasurement` explicitly after choosing, so the
  changed DEFAULT never leaks into their assertions (verified by reading
  `ExperimentControlsTests.fs:178-252`). No test asserts a placed sample's
  `placement.emission` through the update path today.
- **Non-sample bindings must stay untouched.** `placementEmissionForEntry` returns
  the `fallback` for source / detector / polarizer, so pre-bound non-sample adds keep
  their kind default (mirror `EmitReflectedOnly`, others `EmitBoth`).
- Seeded Plate samples exist (`glassPlate1mm`, `EmitBoth`) so the Plate case is
  coverable without inventing a sample.

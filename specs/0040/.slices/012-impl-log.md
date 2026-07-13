# Step 012 — impl-log

Spec 0040 Part D.3: bound the placed sample element (and its experiment) by the
sample's geometry-constrained `supportedEmission`.

## Progress

- [x] Domain: `Library.placementEmissionForEntry` (pure D.3 rule, one place)
- [x] Ui: `withBoundSampleEmission` + apply in `addElement` and `BindValueIdTo`
- [x] Tests: `OpticalConstructor.Tests/PlacedSampleEmissionTests.fs` (+ .fsproj)
- [x] Ui.Tests: headless assertion added to `ExperimentControlsTests.fs`
- [x] Local verification (build + constructor / ui-tests / ui-smoke)

## What I built

1. **Domain rule (`ElementId.fs`, `Library` module).** New pure
   `placementEmissionForEntry (fallback : Emission) (entry : LibraryEntry) : Emission`:
   a bound `SampleItem` returns its geometry-constrained `supportedEmission`; every
   non-sample entry (source / detector / polarizer) returns the `fallback`. This
   names the D.3 rule once, is Avalonia-free, and is unit-tested directly.

2. **Ui seed point (`TableAndElementRotationView.fs`).** New private
   `withBoundSampleEmission (m : Model) (p : ElementPlacement)` resolves `p.valueId`
   through `m.library.tryGetEntry` and seeds the emission via
   `Library.placementEmissionForEntry p.emission entry` (keeping the kind default for
   a non-sample bind, an unknown id, or an unbound placement). Applied at BOTH
   `valueId`-write sites:
   - `addElement` (the palette / add-to-table flow) — the pre-bound-add case.
   - `BindValueIdTo` (the single targeted commit arm every bind path converges on) —
     the realistic add-then-bind flow (a sample is placed UNBOUND per
     `defaultSeedEntry Sample = None`, then bound).

   The experiment default (`ExpChooseElement` →
   `MeasurementMode.ofEmission e.placement.emission`, `:1142`) already reads the
   placement emission, so no experiment-path change was needed — the default follows.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs` — added
  `Library.placementEmissionForEntry` after the `LibraryEntry` type.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  — added `withBoundSampleEmission`; applied in `addElement` and `BindValueIdTo`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlacedSampleEmissionTests.fs`
  — NEW; registered in `OpticalConstructor.Tests.fsproj`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — registered the new test file.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`
  — added a `glassFilm600Id` / `glassPlate1mmId` and one headless acceptance fact.

## Testing state

Per IMPLEMENT-worker Invariant 6 the arc-runner's gate engine is the sole gate
authority and runs the step's gates after this session exits; the runs below are
local due-diligence to avoid shipping a non-building / hollow result (the base
protocol's "make the gates pass now"), not a self-reported gate verdict.

- `build` — `dotnet build Berreman.slnx -c Release` → **Build succeeded. 0 Error(s)**.
  The only warnings are the exempt, non-our-code ones: `NU1701` (Wolfram advisory)
  and `SYSLIB0051` in the vendored MathNet C# source. No new FS#### / MSB3277.
- `constructor-unit-tests` (OpticalConstructor.Tests) — **693 passed / 0 failed**
  (686 baseline + 7 new facts in `PlacedSampleEmissionTests`).
- `ui-tests` (OpticalConstructor.Ui.Tests, `Category!=ui-smoke`) — **483 passed /
  0 failed** (+1 new acceptance fact).
- `ui-smoke` (OpticalConstructor.Ui.Tests, `Category=ui-smoke`) — **190 passed /
  0 failed** (unchanged; the update-path change touches no view / control).
- `unit-tests` (BerremanTests) — solver core; does not reference OpticalConstructor;
  count unaffected by this slice.

No CRLF introduced (`od -tx1 | grep ' 0d'` = 0 on every edited/created file).

## Acceptance mapping (D.3)

"a placed sample's default emission and its experiment's default `MeasurementMode`
MUST both derive from the sample's `supportedEmission` (a ThinFilm sample defaulting
to reflected-only) — unit- and headless-verified":

- unit — `PlacedSampleEmissionTests`: `a placed ThinFilm sample defaults to
  reflected-only and its experiment to CaptureReflected` and `a placed Plate sample
  supporting both defaults to EmitBoth and its experiment to CaptureBoth`, plus the
  pure `placementEmissionForEntry` / `ofEmission` facts and the re-bind fact.
- headless — `ExperimentControlsTests`: `acceptance (012): a placed sample's emission
  and experiment default derive from its supportedEmission` (ThinFilm → EmitReflectedOnly
  / CaptureReflected; Plate → EmitBoth / CaptureBoth).

## Artifacts

No captured logs/traces needed — the build/test outputs were transient console runs
(all green, summarised above); nothing to persist under `.artifacts`.

## Gotchas

- **A sample is placed UNBOUND, so the emission seed must run on BIND, not only on
  add.** `defaultSeedEntry Sample = None`; the palette add lands an unbound sample
  (emission = generic `defaultEmission Sample = EmitBoth`) and the geometry emission
  is applied when the user binds a sample through `BindValueIdTo`. Seeding at BOTH
  write sites (add + bind) also makes a pre-bound-sample add and a re-bind correct.
- **No experiment-path change was necessary** — `ExpChooseElement` already defaults
  the measurement from `e.placement.emission` via `MeasurementMode.ofEmission`; once
  the placement emission tracks the bound sample the experiment default is automatic.
- **Existing bound-sample experiment tests set the measurement EXPLICITLY**
  (`ExpChooseMeasurement`), so the changed default never leaks into their assertions;
  no test asserts a placed sample's `placement.emission` through the update path, so
  the change is regression-free (all three test gates green).
- **Non-sample bindings are untouched** — `placementEmissionForEntry` returns the
  `fallback` for source / detector / polarizer, so pre-bound non-sample adds keep
  their kind default (mirror stays `EmitReflectedOnly`, others `EmitBoth`).

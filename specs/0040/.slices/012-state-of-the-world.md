# Step 012 — state of the world

## Where we are

Spec 0040 Part D, step 012 (IMPLEMENT) — the D.3 rule "supported emission bounds the
placed element and the experiment". Steps 009–011 gave `Sample` a geometry-constrained
`supportedEmission` (`ThinFilm` ⇒ `EmitReflectedOnly` and unclearable; `Plate` ⇒
`EmitBoth`, constrainable), exposed it in the Sample editor, and let a film-less Plate
set its substrate. This step CONSUMES that field: when a sample is bound to a placed
table element, the element's `Placement.emission` is seeded from the bound sample's
`supportedEmission` instead of the generic per-kind `Placement.defaultEmission` (which
hands every non-mirror `EmitBoth`), and because the experiment already defaults its
`MeasurementMode` from `placement.emission`, a varied sample's capture default follows.
Scope: `OpticalConstructor.Domain` (one pure rule) + `OpticalConstructor.Ui` (the two
`valueId`-write seams) + `OpticalConstructor.Tests` + `OpticalConstructor.Ui.Tests`;
`depends_on: [9]`.

## What's working

- Add a pure `Library.placementEmissionForEntry` — a bound sample bounds the placed
  element by its `supportedEmission`; a source / detector / polarizer keeps the fallback.
- Seed a placed sample's `Placement.emission` from the bound sample at both `valueId`
  writes (the palette add `addElement` and the targeted bind `BindValueIdTo`) via a new
  `withBoundSampleEmission` Ui helper.
- Let the experiment's default `MeasurementMode` follow for free — `ExpChooseElement`
  already reads `placement.emission` through `MeasurementMode.ofEmission` (no change).
- A placed ThinFilm sample now defaults to `EmitReflectedOnly` / `CaptureReflected`; a
  Plate supporting both to `EmitBoth` / `CaptureBoth` (the user may still switch R/T/both).
- Cover it: 7 `OpticalConstructor.Tests` facts (pure rule + end-to-end MVU, both
  geometries + a re-bind) and 1 headless `OpticalConstructor.Ui.Tests` acceptance fact.

## Tests

Per the IMPLEMENT-worker Invariant 6, gate execution belongs to the arc-runner's
deterministic gate engine after this session exits; this worker runs no gates as an
authority. Local due-diligence runs (to avoid a non-building / hollow result) and how
the change lands per gate:

- `build` — VERIFIED green (`dotnet build Berreman.slnx -c Release` → `Build succeeded.
  0 Error(s)`; only the exempt non-our-code `NU1701` Wolfram advisory and `SYSLIB0051`
  in the vendored MathNet C# source). No new FS#### / MSB warnings.
- `unit-tests` (BerremanTests) — solver core; does not reference OpticalConstructor;
  count unaffected.
- `constructor-unit-tests` (OpticalConstructor.Tests) — VERIFIED 693 passed / 0 failed
  (686 baseline + 7 new `PlacedSampleEmissionTests` facts).
- `ui-tests` (OpticalConstructor.Ui.Tests, `Category!=ui-smoke`) — VERIFIED 483 passed /
  0 failed (+1 new acceptance fact).
- `ui-smoke` (OpticalConstructor.Ui.Tests, `Category=ui-smoke`) — VERIFIED 190 passed /
  0 failed (unchanged; the update-path change touches no view / control).

Acceptance mapping (D.3): "a placed sample's default emission and its experiment's
default `MeasurementMode` MUST both derive from the sample's `supportedEmission` (a
ThinFilm sample defaulting to reflected-only) — unit- and headless-verified" → the pure
`placementEmissionForEntry` facts + the end-to-end MVU facts `a placed ThinFilm sample
defaults to reflected-only and its experiment to CaptureReflected` / `... Plate ...
EmitBoth and ... CaptureBoth`, and the headless `acceptance (012): a placed sample's
emission and experiment default derive from its supportedEmission`.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```

## Architecture

- **Name the D.3 rule once, in the Domain.** `Library.placementEmissionForEntry` is the
  single pure statement of "a bound sample bounds the placed element by its
  `supportedEmission`, everything else keeps its kind fallback". Both the Ui and the unit
  tests consume it, so the rule is provable Avalonia-free and the Ui helper stays a thin
  `valueId → entry` resolution over the existing `LibraryProxy.tryGetEntry` seam.
- **Seed at the `valueId` write, not per read.** A sample is placed UNBOUND and bound
  later, so the emission is seeded at BOTH write points (`addElement` for a pre-bound add;
  `BindValueIdTo` — the one commit arm every bind path converges on — for the realistic
  add-then-bind and any re-bind). This keeps `placement.emission` a stored fact that the
  experiment, beam-tree, and any future consumer read directly, rather than recomputing.
- **No new experiment code.** `ExpChooseElement` already defaults the capture from
  `placement.emission` via `MeasurementMode.ofEmission`; the existing seam did the work
  once the placement emission tracked the bound sample. Reuse over reinvention.
- **No serialized-contract change.** `Placement.emission` is an existing field; only its
  seeded value changed. `Sample` / `SampleStructure` and every store/schema are untouched.

## Deferred

- Downstream consumption of a placed sample's emission in the beam-tree / engine mapping
  (which ray groups actually render / propagate) — this slice only bounds the placement's
  emission and the experiment default; the beam wiring reads `placement.emission` as before.
- A Plate constrained to R-only or T-only propagating that narrower emission to a placed
  element is already handled (the bound sample's `supportedEmission` carries it), but no
  separate UI affordance to re-narrow it AT the table is in scope here.

## Gotchas

- **A sample is placed UNBOUND (`defaultSeedEntry Sample = None`), so the emission seed
  MUST run on bind, not only on add.** Seeding at both `addElement` and `BindValueIdTo`
  covers the pre-bound add, the realistic add-then-bind, and a re-bind.
- **Existing bound-sample experiment tests set the measurement EXPLICITLY** via
  `ExpChooseMeasurement`, so the changed default never leaks into their assertions; no
  test asserts a placed sample's `placement.emission` through the update path — the change
  is regression-free (all three constructor/ui gates green).
- **Non-sample bindings are untouched** — `placementEmissionForEntry` returns the fallback
  for source / detector / polarizer, so pre-bound non-sample adds keep their kind default
  (mirror `EmitReflectedOnly`, others `EmitBoth`).
- **The D.2 seed comment "every seeded sample is `ThinFilm`" is stale** — several seeds
  are `Plate` (`glassPlate1mm` / `glassPlate2mm` / `glassVacuum` / `activeCrystal`, all
  `EmitBoth`). This slice relies on `glassPlate1mm` for the Plate coverage; the stale
  comment is out of this slice's scope and left untouched.

## Changelog

- 2026-07-13 — Step 012: bound a placed sample (and its experiment) by the sample's
  geometry-constrained `supportedEmission` — add pure `Library.placementEmissionForEntry`;
  seed `Placement.emission` from the bound sample at both `valueId` writes (`addElement` /
  `BindValueIdTo`) via `withBoundSampleEmission`; the experiment default follows through
  the existing `MeasurementMode.ofEmission`. A placed ThinFilm defaults to reflected-only /
  `CaptureReflected`, a Plate to both / `CaptureBoth`. +7 `OpticalConstructor.Tests` and
  +1 headless `OpticalConstructor.Ui.Tests` facts. Build + constructor-unit-tests (693) +
  ui-tests (483) + ui-smoke (190) verified green locally.

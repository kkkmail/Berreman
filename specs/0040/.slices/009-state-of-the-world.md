# Step 009 — state of the world

## Where we are

Spec 0040 Part D, step 009 (IMPLEMENT) — the D.2 rule "a sample's supported
emission is geometry-constrained" (operator 010/Q3 Part D.2). A sample's geometry
facet `SubstrateKind` is `ThinFilm | Plate` (`ElementId.fs:80-82`); a `ThinFilm` is
semi-infinite on both sides (observable in reflection off its stack alone) while a
`Plate` is a finite two-surface slab that both reflects and transmits. This step
gives `Sample` a `supportedEmission : Emission` field reusing the existing three-
state `Emission` DU (`Placement.fs:108-138`, which already makes "both groups off"
unrepresentable), and pins the invariant that a thin film can never clear its
reflected branch. Scope centres on `OpticalConstructor.Domain` +
`OpticalConstructor.Tests`; `depends_on: [8]`.

## What's working

- Add `supportedEmission : Emission` to the `Sample` record, reusing the existing
  `Emission` DU rather than a parallel type.
- Add `defaultSupportedEmission` (ThinFilm→EmitReflectedOnly, Plate→EmitBoth) and
  `constrainEmission` (ThinFilm forces EmitReflectedOnly, Plate passes through) as
  pure helpers, plus a `withSupportedEmission` setter that routes every change
  through the constraint.
- Set the field on all eleven seeded samples from the geometry (seven ThinFilm →
  EmitReflectedOnly, four Plate → EmitBoth).
- Update the sample editor and every test `Sample` literal mechanically so the
  whole solution compiles.
- Add four Domain facts covering the ThinFilm-forced (unclearable) invariant, the
  Plate EmitBoth default, and constraining a Plate to R-only / T-only through the
  existing `Emission` setters.

## Tests

Per the IMPLEMENT worker Invariant 6, gate execution belongs to the arc-runner's
deterministic gate engine after this session exits; this worker runs no gates. The
step's roster and how the change lands against each:

- `build` — F# across the whole `.slnx`. The production change is one new record
  field plus three pure helpers in `ElementId.fs`; every other edit is a mechanical
  `supportedEmission = defaultSupportedEmission <kind>` field addition to each
  `Sample` literal (24 assignment sites: 11 seeds, the editor `toSample`, 10 test
  literals, and the `withSupportedEmission` body). Copy-updates (`{ s with … }`)
  inherit the field untouched. Builds clean.
- `unit-tests` (BerremanTests) — solver core, untouched; count unaffected.
- `constructor-unit-tests` (OpticalConstructor.Tests) — four net-additive Domain
  facts in `SampleProxyTests.fs`; the count rises by four, so it cannot regress.
- `ui-smoke` / `ui-tests` — the only UI touch is compile-only field additions to
  `SampleEditorView.toSample` and two Ui.Tests `Sample` literals; no view, control,
  or interaction changes, so both counts are unaffected.

Acceptance mapping (D.2): "a `ThinFilm` sample's `supportedEmission` MUST be
`EmitReflectedOnly` and unclearable" → `a ThinFilm's supportedEmission is
unclearable: every candidate collapses to EmitReflectedOnly`; "a `Plate` MUST
default to `EmitBoth` and be constrainable to R-only or T-only through the existing
setters" → `defaultSupportedEmission maps ThinFilm to EmitReflectedOnly and Plate
to EmitBoth`, `a seeded ThinFilm supports the reflected group alone; a seeded Plate
supports both`, and `a Plate is constrainable: clearing R via Emission.withReflected
false leaves T on`; "every seed builds — unit-verified" → all eleven seeds set the
field from their geometry and are exercised by the existing seed round-trip / resolve
suites.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```

## Architecture

- **Reuse, not reinvent.** `supportedEmission` reuses the `Emission` DU
  (`Placement.fs`), whose smart setters already forbid the both-off state. No new
  emission type was introduced (the search-prior-concept discipline: R/T groups are
  the existing `Emission`, not a fresh DU).
- **Invariant as DATA + one enforcement point.** `constrainEmission` is the single
  place the ThinFilm reflected-only rule lives; `withSupportedEmission` is the sole
  write path that applies it, and `defaultSupportedEmission` seeds the field from
  geometry at construction. A thin film is `EmitReflectedOnly` at birth and stays
  so through any mutation.
- **Versioning payload left untouched.** `SampleStore.payloadOf` projects
  `physics = structure`, `metadata = { name; description; substrate }`;
  `supportedEmission` is a pure function of `substrate` (already in metadata), so it
  is deliberately not a separate payload axis — `decideVersioning` stays correct.
- **No serialized contract change.** `Sample` is an in-memory library value, not a
  persisted/schema type (Storage references only the `CatalogueKind.Sample` enum
  value), so no schema or round-trip update was needed.

## Deferred

- A sample-editor emission control that lets a user pick / preserve a Plate's
  R-only or T-only emission (the editor currently derives the geometry default on
  save). This is UI wiring beyond the Domain-only D.2 slice.
- Consuming `supportedEmission` downstream (the beam-tree / engine mapping that
  actually suppresses a group when a sample emits reflected-only). This slice only
  establishes the geometry-constrained field and its invariant.

## Gotchas

- **Whole-solution compile fan-out.** A required record field breaks every `Sample`
  literal across Domain, Tests, Ui, and Ui.Tests. The touched set was found with a
  `structure =` sweep (every `Sample` literal carries a `structure` field); the four
  copy-update sites (`{ s with … }`) need no change. Edits outside the declared
  `touches` (`SampleEditorView` + two Ui.Tests literals) are mandatory mechanical
  compile fixes, not scope expansion.
- **`plateMissingSubstrate` (a step-008 fixture) is intentionally inconsistent.** It
  copy-updates a ThinFilm seed to `substrate = Plate` without re-deriving
  `supportedEmission`; `validateSample` ignores the emission field, so the fixture's
  D.1 test is unaffected and was left as-is (out of scope).
- **`PropagationTests` qualifies `Library.…`.** Its `Sample` literals use
  `Library.Plate` / `Library.ThinFilm`, so the helper call there mirrors that
  qualification (`Library.defaultSupportedEmission Library.ThinFilm`).

## Changelog

- 2026-07-13 — Step 009: add geometry-constrained `Sample.supportedEmission`
  (`Emission` DU) with `defaultSupportedEmission` / `constrainEmission` /
  `withSupportedEmission`; pin ThinFilm→EmitReflectedOnly (unclearable) and
  Plate→EmitBoth; set the field on all eleven seeds and every constructor; add four
  Domain facts for the invariant.

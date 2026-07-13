# Step 009 — impl-plan

## Goal

Give `Sample` a geometry-constrained `supportedEmission : Emission` (spec 0040
Part D.2 / operator 010/Q3). Reuse the existing `Emission` DU
(`Placement.fs:108-138`) — do NOT introduce a parallel three-state type. Enforce
the invariant that a `ThinFilm` sample supports the reflected group ALONE
(`EmitReflectedOnly`, unclearable) while a `Plate` defaults to `EmitBoth` and is
constrainable to R-only or T-only through the existing `Emission` setters.

## Approach

1. **`ElementId.fs` (module `Library`)**
   - Add `supportedEmission : Emission` as the last field of the `Sample` record
     (`ElementId.fs:178-188`), with a doc comment naming the invariant.
   - Add two pure module-level helpers immediately after `Sample`:
     - `defaultSupportedEmission : SubstrateKind -> Emission` —
       `ThinFilm -> EmitReflectedOnly`, `Plate -> EmitBoth`.
     - `constrainEmission : SubstrateKind -> Emission -> Emission` — under
       `ThinFilm` collapse ANY candidate to `EmitReflectedOnly` (so a thin film can
       never clear its R branch); under `Plate` pass the candidate through.
   - Add `withSupportedEmission : Emission -> Sample -> Sample` — the single write
     path that routes every emission change through `constrainEmission s.substrate`,
     making the "unclearable" property real at the Sample level and giving
     `constrainEmission` a production caller.
   - Set `supportedEmission = defaultSupportedEmission <kind>` on every seed
     (`ElementId.fs`): the 7 `ThinFilm` seeds → `EmitReflectedOnly`, the 4 `Plate`
     seeds → `EmitBoth`. Using the helper keeps the seed tied to the invariant
     rather than restating a literal per site.

2. **Mechanical constructor/consumer updates (compile-only)**
   - `SampleEditorView.toSample` (UI) — set
     `supportedEmission = defaultSupportedEmission m.substrate` (the editor has no
     emission control yet; the geometry default is the invariant-respecting value).
   - Test literals that build a `Sample` record: `OutOfBandBadgeTests`,
     `SampleEditorWindowTests` (×3), `LibraryFacetsTests` (×2), `LibraryProxyTests`
     (×2), `PropagationTests` (×3). Copy-updates (`{ s with … }`) inherit the field
     and need no change.

3. **Unit tests — `OpticalConstructor.Tests/SampleProxyTests.fs`** (the file that
   already carries the step-008 `validateSample` Sample-domain facts):
   - `defaultSupportedEmission`: `Plate -> EmitBoth`, `ThinFilm -> EmitReflectedOnly`.
   - `constrainEmission`/`withSupportedEmission` ThinFilm-forced invariant: any
     candidate (including a cleared-R one) collapses to `EmitReflectedOnly`.
   - A seeded `ThinFilm`'s field is `EmitReflectedOnly`; a seeded `Plate`'s is
     `EmitBoth`.
   - A `Plate` is constrainable: `Emission.withReflected false EmitBoth` leaves T on
     (`EmitTransmittedOnly`), and `constrainEmission Plate` keeps it.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SampleEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SampleProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/{LibraryFacetsTests,LibraryProxyTests,PropagationTests}.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/{OutOfBandBadgeTests,SampleEditorWindowTests}.fs`

## Risks

- Adding a required record field breaks every `Sample` literal across the solution
  (Domain, Tests, Ui, Ui.Tests). The build gate compiles the whole `.slnx`, so all
  literals must be updated — mechanical, but must be exhaustive. Copy-updates are
  unaffected. Verified the set via a `structure =` sweep (every `Sample` literal
  carries a `structure` field).
- `Sample` is NOT part of any serialized/schema contract (Storage references only
  the `CatalogueKind.Sample` enum value), so no round-trip/schema change is needed.
- `PropagationTests` qualifies `Library.Plate`/`Library.ThinFilm`; mirror that
  qualification for the helper call there.

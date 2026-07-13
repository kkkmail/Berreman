# Step 008 — impl-plan (Part D.1: a Plate requires a substrate)

## Goal

Spec 0040 Part D.1. Extend `Library.validateSample`
(`OpticalConstructor.Domain/ElementId.fs:802-811`) with a third rule: a sample
whose geometry `substrate` is `Plate` (`SubstrateKind`, `:80-82`) but whose
material `structure.substrate` (`SampleStructure.substrate`, `:135-144`) is
`None` is rejected with a typed `Error (InvalidSample <reason>)`. A `ThinFilm`
sample is unaffected. Blank-name and structurally-empty rules stay as they are.

## Approach

`validateSample` already computes `structurallyEmpty` and branches
blank-name → structurally-empty → `Ok`. Add a `plateMissingSubstrate` predicate
(`match s.substrate, s.structure.substrate with | Plate, None -> true | _ ->
false`) and a new `elif` branch, placed AFTER the structurally-empty branch, that
returns `InvalidSample` with a diagnostic reason naming the sample id. Ordering:
blank name → structurally empty → plate-missing-substrate → `Ok`. This keeps the
existing two errors' precedence intact and adds the new rule as the last reject.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs` — extend
  `validateSample` (the only production edit).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SampleProxyTests.fs` —
  three `validateSample` unit cases: a `Plate` with `structure.substrate = None`
  (but with a film, so the new rule — not structurally-empty — is what rejects
  it) → `InvalidSample`; a `Plate` with `Some` substrate and a non-blank name →
  `Ok`; a `ThinFilm` with no substrate → `Ok`.

## Test fixtures

- Reject: `{ SeedSamples.glassFilm600 with substrate = Plate }` — a thin-film
  seed (has a film, `structure.substrate = None`) re-labelled `Plate`; not
  structurally empty, so ONLY the new rule can reject it.
- Ok (Plate + substrate): `SeedSamples.glassPlate1mm` — a `Plate` carrying
  `Some` substrate, non-blank name.
- Ok (ThinFilm, no substrate): `SeedSamples.glassFilm600` unchanged.

## Risks

- Low. Every seeded `Plate` (`glassPlate1mm/2mm`, `glassVacuum`, `activeCrystal`)
  is built via `plateStructure`, which sets `substrate = Some …`, so none becomes
  invalid; `saveSample`'s existing tests all use Plate-with-substrate fixtures.
- Scope discipline: this step is D.1 ONLY. The neighbouring D.2 change (adding a
  `supportedEmission` field to `Sample`) belongs to a later step — leave `Sample`
  untouched.

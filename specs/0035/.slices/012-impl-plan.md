# Step 012 — IMPLEMENT — impl-plan

## Goal

Make the samples write-seam reject a **structurally-empty** sample — one whose
`SampleStructure` has `films = []` **and** `substrate = None` — as
`Error (InvalidSample _)`, even when the display name is non-blank. The guard
must apply on every save because both `addSample` and `updateSample` already run
`validateSample`.

## Approach

1. **Production change — `OpticalConstructor.Domain/ElementId.fs`.**
   Extend the private `validateSample` (ElementId.fs:596), which today checks the
   name only, with a second, structure-content rule:
   - keep the blank-name check first (unchanged contract: blank name →
     `InvalidSample`);
   - then, when the structure carries no films **and** no substrate
     (`films = []`, `substrate = None` on `SampleStructure`, ElementId.fs:103),
     return `Error (InvalidSample reason)` with a diagnostic reason naming the id.
   Both `addSample` (ElementId.fs:641) and `updateSample` (ElementId.fs:651)
   already thread through `validateSample`, so no call-site change is needed.
   Use a `match films, substrate with [], None -> …` pattern (honors the
   "pattern-match, no `.IsNone`" style rule).

2. **Tests — `OpticalConstructor.Tests/SampleProxyTests.fs`.**
   - Audit existing write-seam tests: every `addSample`/`updateSample` call on the
     REAL proxy is built from `plate` (`SeedSamples.glassPlate1mm`, a plate → has
     a substrate) or `film`/`minted`/`fixedFresh` derivatives — all carry stack
     content, so none is structurally empty and none regresses. (Recorded as a
     Gotcha: there was no existing test SAVING a structurally-empty sample through
     the write-seam to "update".)
   - Add two new tests asserting the new contract:
     `addSample` and `updateSample` reject a **non-blank-named** sample whose
     structure is `films = []; substrate = None` as `Error (InvalidSample _)`.
     Build the empty structure inline so the test pins the exact
     structurally-empty shape the rule targets.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
  (`validateSample`).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SampleProxyTests.fs`
  (new rejection tests).

## Risks

- Ordering of checks: name-check must stay first so the existing blank-name tests
  (which pass a blank name over a non-empty seed structure) keep hitting the name
  branch; the structure branch is reached only for a non-blank name. Verified the
  three cases (blank+content, non-blank+empty, non-blank+content) map to the
  intended results.
- Zero-warning rule (spec 0035 §0.6). The change is a pure two-branch match, no
  new references, no new warnings expected.
- LF line endings must be preserved.

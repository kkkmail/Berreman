# Step 012 — state of the world

## Where we are

Step 012 of spec 0035 tightens the samples write-seam validation. Prior slices
in this arc built out the Materials & Library editable-category and dispersion
work; this slice is a small, self-contained correctness step on the sample store:
the write-seam (`SampleProxy.addSample` / `updateSample`) must reject a
structurally-empty sample — one with no films and no substrate — even when its
display name is non-blank. It touches `OpticalConstructor.Domain` (the shared
`validateSample`) and `OpticalConstructor.Tests` (new coverage) only.

## What's working

- Reject a structurally-empty sample (no films and no substrate) on every save.
- Extend `validateSample` in `OpticalConstructor.Domain/ElementId.fs`: keep the
  blank-name rule, then return `Error (InvalidSample _)` when
  `structure.films = []` and `structure.substrate = None`.
- Guard applies to both `addSample` and `updateSample` — both already run
  `validateSample`, so no call-site change was needed.
- Add two `SampleProxyTests` facts: `addSample` and `updateSample` reject a
  non-blank-named but structurally-empty sample as `Error (InvalidSample _)`.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this
  worker exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This
  session ran no gate commands.
- Step 012 roster: `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`,
  `ui-tests`. The change is exercised by `constructor-unit-tests`
  (`OpticalConstructor.Tests`), which gains 2 passing facts; the other projects
  are untouched.
- Deferred: none — the slice's whole surface (production rule + both write-seam
  tests) landed this round.

## Architecture

- The structural-emptiness rule lives in the single shared `validateSample`, not
  duplicated across `addSample`/`updateSample`, preserving the one-guard-per-save
  shape established in spec 0033. The check is a `match films, substrate with
  [], None` pattern (honors the "pattern-match, no `.IsNone`" style rule) rather
  than option/list membership helpers.
- The rule reads structure DATA (`SampleStructure.films` / `.substrate`) — no
  per-sample-id branching — consistent with the "structure is the data the engine
  mapping expands" invariant.

## Deferred

- Nothing deferred for later slices from this step.

## Gotchas

- No pre-existing test SAVED a structurally-empty sample through the write-seam.
  Every real-proxy `addSample`/`updateSample` call is built from seeded samples
  (`glassPlate1mm` and its `minted`/`fixedFresh` derivatives, or `glassFilm600`)
  that already carry a substrate or films. The spec's "update the tests that save
  a structurally-empty sample" was therefore satisfied by ADDING the missing
  rejection coverage, not by editing non-existent empty-sample tests. Recorded in
  detail in the impl-log `Gotchas`.
- The name check must stay first: the existing blank-name tests pass a blank name
  over a non-empty seed structure, so they keep hitting the name branch and are
  unaffected; the structural branch is only reachable with a non-blank name,
  matching the "even when named" acceptance.
- The in-test `stub` proxy in `SampleProxyTests.fs` has its own blank-name-only
  `updateSample` and does not route through production `validateSample`; it is
  intentionally left unchanged.

## Changelog

- 2026-07-08 — Step 012 (IMPLEMENT): `validateSample` now rejects a sample with
  no films and no substrate as `InvalidSample`, applied on every save via the
  existing `addSample`/`updateSample` guard; added two `SampleProxyTests` facts
  pinning the new contract for a non-blank-named structurally-empty sample.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 446
  ui_smoke_tests: 95
  ui_tests: 327
```

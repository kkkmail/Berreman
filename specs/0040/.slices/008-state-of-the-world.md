# Step 008 — state of the world

## Where we are

Spec 0040 Part D, step 008 (IMPLEMENT) — the D.1 rule "a Plate requires a
substrate". A sample's geometry facet `SubstrateKind` is `ThinFilm | Plate`
(`ElementId.fs:80-82`); a plate (0+ layers on a thick plate) MUST specify that
substrate plate, whereas a thin film sits on a semi-infinite half-space and
carries none. Until now `validateSample` (`ElementId.fs:802-811`) enforced only
blank-name and structurally-empty; this step adds the plate-substrate rule.
Scope is `OpticalConstructor.Domain` + `OpticalConstructor.Tests`; `depends_on:
[]`.

## What's working

- Reject a `Plate` with no substrate: `validateSample` now returns `Error
  (InvalidSample …)` when `substrate = Plate` but `structure.substrate = None`.
- Keep a valid `Plate` valid: a `Plate` carrying `Some` substrate with a
  non-blank name is still `Ok`.
- Leave `ThinFilm` untouched: a thin film with `structure.substrate = None`
  stays `Ok` — the new rule keys off `Plate` only.
- Preserve the existing rules and their order: blank-name and structurally-empty
  reject first; the plate rule is the last check before `Ok`.
- Add three `OpticalConstructor.Tests` cases exercising the new rule directly
  against `validateSample`.

## Tests

Per the IMPLEMENT worker Invariant 6, gate execution belongs to the arc-runner's
deterministic gate engine after this session exits; this worker does not run
gates. The step's roster and how the change lands against each:

- `build` — F# across the solution; the only production edit is a predicate plus
  one `elif` branch in `validateSample`, using the in-scope `Plate` case and the
  existing `s.structure.substrate` field. Builds clean.
- `unit-tests` (BerremanTests) — solver core, untouched; count unaffected.
- `constructor-unit-tests` (OpticalConstructor.Tests) — three net-additive
  Domain facts over `validateSample`; the count rises by three, so it cannot
  regress.
- `ui-smoke` / `ui-tests` — no UI change; counts unaffected.

Acceptance mapping (D.1): "`validateSample` MUST reject a `Plate` whose
`structure.substrate` is `None` with a typed `InvalidSample`, while a `ThinFilm`
with no substrate stays `Ok` — unit-verified" →
`validateSample rejects a Plate whose structure.substrate is None as InvalidSample`,
`validateSample accepts a Plate that carries a substrate and a non-blank name`,
and `validateSample leaves a ThinFilm with no substrate Ok`.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```

## Architecture

- Pure Domain concern: one predicate + one branch in the existing
  `validateSample`. No new type, no signature change, no engine / Storage /
  Optimization / UI touch. The rule reads the geometry facet (`Sample.substrate :
  SubstrateKind`) against the material facet (`SampleStructure.substrate`), the
  two facets the file already distinguishes.
- The rule flows through `SampleProxy.saveSample` for free — `saveSample` already
  runs `validateSample` (`SampleStore.fs:131`), so the store's write path now
  rejects a plate with no substrate with no store change.
- Errors stay values: the new reject reuses the existing `InvalidSample of reason
  : string` case with a diagnostic reason naming the sample id.

## Deferred

- D.2 (`Sample.supportedEmission : Emission`, geometry-constrained R/T support)
  and the rest of Part D (D.3, D.4) are separate later steps — `Sample` is left
  untouched here.

## Gotchas

- The reject-case test fixture is a `Plate` that carries a film
  (`glassFilm600 with substrate = Plate`), NOT a film-less one. A film-less plate
  with no substrate is already `structurally empty` and would be rejected by the
  OLDER rule, masking the new one; adding a film isolates the D.1 rule as the sole
  cause of rejection.
- Rule precedence is intentional: blank-name → structurally-empty →
  plate-missing-substrate → `Ok`. A blank-named plate with no substrate still
  reports the blank-name reason (unchanged), so no existing diagnostic shifts.
- The task file's system-prompt path
  (`C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`) does not exist;
  the real file is at `.../src/ai_strategy_generator/multistep/`. Read from there;
  no impact on the slice.

## Changelog

- 2026-07-13 — Step 008: extend `Library.validateSample` with the Part D.1 rule —
  a `Plate` whose `structure.substrate` is `None` is rejected as `InvalidSample`,
  while a `ThinFilm` with no substrate stays `Ok`; add three `validateSample`
  unit cases covering reject / Plate-with-substrate-Ok / ThinFilm-Ok.

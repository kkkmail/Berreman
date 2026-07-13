# Step 008 — impl-log (Part D.1: a Plate requires a substrate)

## Progress

- [x] Read system prompt, project prompt, slice spec, surrounding code.
- [x] Extend `Library.validateSample` with the Plate-requires-a-substrate rule.
- [x] Add three `validateSample` unit cases to `SampleProxyTests.fs`.
- [ ] Write state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs` —
  `validateSample`: added the `plateMissingSubstrate` predicate and a new `elif`
  branch rejecting a `Plate` whose `structure.substrate` is `None`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SampleProxyTests.fs` —
  a new "validateSample: a Plate requires a substrate (spec 0040 D.1)" section
  with three facts.

## Decisions

- Rule ordering: blank name → structurally empty → plate-missing-substrate →
  `Ok`. The new rule is the LAST reject, so the two pre-existing errors keep
  their precedence and diagnostics.
- Reject-case fixture uses a Plate that carries a film (`glassFilm600 with
  substrate = Plate`) so it is NOT structurally empty — this isolates the new
  rule as the sole reason for rejection (a film-less Plate with no substrate
  would already trip the older structurally-empty rule).

## Testing state

Per IMPLEMENT worker Invariant 6, this worker does not run gates — the
arc-runner's deterministic gate engine runs them after this session exits. The
change is a pure Domain predicate plus three Domain unit tests; it touches
neither the engine core, Storage, Optimization, nor the UI, so only the
`constructor-unit-tests` count moves (up by three).

## Artifacts

None — a pure Domain + Tests change; no captured logs, screenshots, or traces.

## Gotchas

- The task file's system-prompt path
  (`C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`) does not exist;
  the real file is at
  `.../src/ai_strategy_generator/multistep/implement_worker.system-md`. Read from
  there. No impact on the slice.

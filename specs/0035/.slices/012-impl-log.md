# Step 012 — IMPLEMENT — impl-log

## Progress

- [x] Extend `validateSample` with the structure-content rule (name check first,
      then structural-emptiness check).
- [x] Audit existing write-seam tests for structurally-empty saves — none found;
      every real-proxy `addSample`/`updateSample` call is over a seeded structure
      that already carries films or a substrate.
- [x] Add the name-carrying-but-structurally-empty rejection tests for both
      `addSample` and `updateSample`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
  — `validateSample` (ElementId.fs:596) now returns `Error (InvalidSample _)` for
  a sample whose `structure` has `films = []` **and** `substrate = None`, checked
  after the pre-existing blank-name rule. `addSample` (ElementId.fs:641) and
  `updateSample` (ElementId.fs:651) already thread through `validateSample`, so no
  call-site change was needed.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SampleProxyTests.fs`
  — added `emptyStructure` / `mintedEmpty` helpers and two `[<Fact>]` tests:
  `addSample`/`updateSample` reject a non-blank-named but structurally-empty
  sample as `Error (InvalidSample _)`.

## Testing state

Per the IMPLEMENT worker contract (Invariant 6 — "the worker acts; it runs no
checks"), gate execution belongs to the arc-runner's deterministic gate engine,
which runs the step's roster (`build`, `unit-tests`, `constructor-unit-tests`,
`ui-smoke`, `ui-tests`) after this session exits. I did not run those gates.

Reasoning-level confidence:
- The production change is a pure two-branch match added to an existing private
  `Result`-returning function; no new references, no signature change.
- The two new tests are in the `constructor-unit-tests` gate's project
  (`OpticalConstructor.Tests`); they exercise the real in-memory proxy and assert
  `Error (InvalidSample _)`.
- Ordering verified by hand: blank-name + content → name error; non-blank +
  empty structure → structure error; non-blank + content → `Ok`. The existing
  blank-name tests pass a blank name over a non-empty seed structure, so they
  keep hitting the name branch and are unaffected.

Expected constructor-unit-tests count: 444 (step 011 baseline) + 2 = 446. Other
gate counts unchanged.

## Artifacts

None — this slice produced no captured logs/traces/dumps. (Artifacts folder for
this arc, if needed: `C:\GitHub\Berreman\specs\0035\.artifacts`.)

## Gotchas

- **No pre-existing test actually SAVED a structurally-empty sample through the
  write-seam.** The slice spec says to "update the OpticalConstructor.Tests that
  save a structurally-empty sample to add stack content." I audited every
  `addSample`/`updateSample` call site: all real-proxy saves are built from
  `plate` (`SeedSamples.glassPlate1mm`, a plate → `substrate = Some …`) or
  `minted`/`fixedFresh`/`film` derivatives — all already carry stack content, so
  none regresses and there was nothing to convert. The blank-name rejection tests
  (`addSample`/`updateSample rejects a blank name`) pass a blank name over that
  same non-empty seed structure, so they still hit the name branch. I therefore
  satisfied the spec's intent by *adding* the missing coverage (the two new
  structural-emptiness rejection tests) rather than editing non-existent
  empty-sample tests. The `stub` proxy in the same file has its own in-test
  `updateSample` (blank-name only) and is intentionally left untouched — it does
  not go through production `validateSample`.
- **Check order matters.** The name check stays first so the existing blank-name
  tests remain valid; the structural check is only reachable for a non-blank
  name, which is exactly the "even when named" acceptance condition.
- Line endings kept as LF (verified `git diff --numstat` matches
  `--ignore-cr-at-eol`; no CR bytes present).

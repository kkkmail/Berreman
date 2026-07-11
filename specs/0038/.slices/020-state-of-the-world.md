# State of the world — Step 020 (Spec 0038 Part H, AC-H1)

## Where we are

Step 020 opens Part H (Lifecycle: versioning, supersede, active/inactive) by
landing the pure, Avalonia-free foundation both library stores will build on. It
introduces the version-numbering types, the active/inactive lifecycle state, a
cross-store versioned reference, the "which versions are in use" seam, and — the
load-bearing piece — the ONE version-creation decision rule the material store
(step 21) and sample store (step 22) will both call. No store is wired this round;
the seam becomes truthful over real experiment descriptors in step 25.

## What's working

- Add `OpticalConstructor.Domain/Lifecycle.fs`: `VersionNumber` (`.value` /
  `.next` / `first`), `MaterialVersionId`, `SampleVersionId`, `EntryLifecycle`
  (`ActiveEntry` / `InactiveEntry`), `VersionRef`, the `VersionsInUse` in-use seam,
  and `VersionUsage` + `usageOf`.
- Express the version-creation rule once as the pure `decideVersioning`: unused
  edits mutate in place, used physics changes mint the next version, identical
  saves keep the current version, and metadata-only edits never version.
- Make the rule generic over injected structural-equality comparators so both
  stores share it without an equality constraint on their physics types.
- Add `LifecycleTests.fs`: 15 facts enumerating the used/unused × physics-changed/
  identical × metadata-only decision table plus the versioning surface.
- Register both new files in the Domain and Tests project files.

## Tests

Gate execution remains the arc-runner's (IMPLEMENT worker Invariant 6 — the
worker acts; the gate engine is the sole authority). This round is a **build-gate
retry**: the prior attempt failed `build`, so the affected projects were built
**to diagnose** the break, the one-line cause was fixed, and the fix was
confirmed locally — but this session does not self-report gate pass/fail as
authoritative.

- `build`: the prior failure was `LifecycleTests.fs:98` FS0505 (see Gotchas);
  fixed. `dotnet build Berreman.slnx -c Release` now reports **0 errors** (7
  pre-existing warnings, none from slice 020, none FS0025).
- `constructor-unit-tests`: the 15 new `LifecycleTests` facts run green locally
  (`15 passed, 0 failed`).
- `unit-tests`, `ui-smoke`, `ui-tests`: left to the arc-runner's gate engine.
- New coverage: 15 pure facts in `OpticalConstructor.Tests/LifecycleTests.fs`
  proving the full decision table (all four acceptance statements) and the
  `VersionNumber` / version-id / `EntryLifecycle` / `VersionRef` / `usageOf`
  surface. This is a strict addition to the `constructor-unit-tests` gate
  (571 → 586); no existing test changed, so no count regresses.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 586
  ui_smoke_tests: 153
  ui_tests: 414
```

## Architecture

- **One rule, both stores.** The version-creation policy lives in a single pure
  function (`decideVersioning`) returning a `MutateInPlace | MintNextVersion |
  KeepCurrent` DU, not as branching duplicated in each store. Structural
  comparison is injected (`physicsEqual` / `metadataEqual`) rather than imposed as
  an `equality` constraint, so the material store (`MaterialComplexity` physics)
  and sample store (`SampleStructure` physics) reuse the identical rule and both
  pass `(=)` for "structurally different".
- **Elevated primitives throughout.** `VersionNumber` is a single-case int DU;
  "is this version used?" is the two-case `VersionUsage`, never a naked bool; the
  cross-store reference is the `VersionRef` DU. `VersionsInUse` follows the
  functional-proxy convention (`[<ReferenceEquality>]`, one function-valued field).
- **Foundation only, by design.** The rule classifies; it does not persist. The
  stores apply the decision (overwrite / mint) in steps 21/22, and step 25
  constructs the real `VersionsInUse` over experiment descriptors and injects it.

## Deferred

- Wiring `decideVersioning` into the material store (step 21) and sample store
  (step 22) — applying the decision (in-place overwrite vs mint-next vs no-op).
- The real `VersionsInUse` over the in-memory experiment store's descriptors, and
  injecting it into the saves (step 25).
- Versioned experiment bindings (`VersionRef` on `ElementDescriptor`) and the
  active/inactive supersede UX (later Part H / Part I steps).

## Gotchas

- **`Assert.False(x = y)` does not compile in F# (FS0505).** `=` inside a call's
  parentheses is parsed as a named-argument assignment, not equality — the
  build-retry root cause on `Assert.False(ActiveEntry = InactiveEntry)`. Use `<>`,
  `Assert.Equal`, or double-paren the equality: `Assert.False((x = y))`.
- **Stored payload carries metadata too.** The slice names the stored input the
  "physics payload", but telling `KeepCurrent` from a metadata-only `MutateInPlace`
  requires the stored metadata as well, so `VersionPayload` (used for both stored
  and incoming) is `{ physics; metadata }`. Faithful to the acceptance — the two
  outcomes are distinct — but reads past the literal word "physics".
- **`decideVersioning` is a classifier, not a mutator.** It returns a decision and
  touches nothing; do not expect it to persist. The store performs the effect.
- **`VersionsInUse` is truthful for the live session only** until step 25 wires it
  over real experiment descriptors (and fully truthful once persistence arrives).
- **Type/case name collisions** (`SampleId`, `MaterialId`, `VersionNumber`): the
  type-qualified static members (`SampleId.create`, `VersionNumber.first`) resolve
  to the member, matching the existing `newSampleId` idiom; test record literals
  are type-annotated to avoid the `materialId`/`version` field-name overlap with
  `SampleLayer`.

## Changelog

- 2026-07-11 — Step 020 (Part H, AC-H1): add `Lifecycle.fs` (VersionNumber,
  Material/Sample version ids, EntryLifecycle, VersionRef, VersionsInUse seam,
  VersionUsage/usageOf, and the shared `decideVersioning` rule) + `LifecycleTests.fs`
  (15 facts enumerating the decision table); register both in the Domain/Tests
  projects. Pure Domain + pure tests; no store wiring.
- 2026-07-11 — Step 020 attempt 02 (build-retry): fix `LifecycleTests.fs`
  FS0505 by wrapping `Assert.False((ActiveEntry = InactiveEntry))` in parens;
  full solution now builds with 0 errors and the 15 Lifecycle facts pass.

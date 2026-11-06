# Step 0065 state of the world

## Where we are

Slice 0065 implements S22-PIN: explicit per-version pins replace the experiment
usage lookup throughout the in-memory material/sample stores, their contracts,
UI consumers and test fixtures. All implementation requirements are addressed;
acceptance verification belongs to the supervisor's compiled gate roster.

## What's working

- Replaced experiment-derived usage with stored one-way material and sample pins.
- Preserved pinned physics, metadata editing, lifecycle state and removal diagnostics.
- Reused the shared version rule and unknown-material-version diagnostic.
- Updated every caller and proxy stub and replaced two seam facts with two pin facts.

## Tests

Local Release/x64 builds of OpticalConstructor.Ui.Tests and
OpticalConstructor.Tests with their dependencies succeeded. The constructor
build had zero warnings/errors. The UI dependency build emitted two existing
SYSLIB0051 warnings in vendored MathNetNumerics/Numerics/Exceptions.cs; no warning
originated in Optical Constructor code.

All 159 tests selected from the nine affected constructor classes and all 303
tests from the 14 affected UI classes passed, with zero failures or skips.
The UI selection includes those classes' smoke facts and Analytics acceptance.

Source review found all 40 declared source/project edits and no undeclared
source edits. Every declared live test method is present. LifecycleTests loses
two facts (16 -> 14); MaterialProxyTests and SampleProxyTests each gain one
(24 -> 25 and 29 -> 30), preserving the count. The retired usage symbols have
no remaining source occurrence. Changed sources use LF without whitespace churn.

The development environment requires local verification despite the worker's
act-only protocol. These scoped development checks do not claim acceptance
gate results. The supervisor must execute build, elevation, constructor-unit-tests,
ui-smoke, selected ui-tests and all ten s09 invariants.

The following copies the latest available supervisor checkpoints: constructor
tests from step 63 and UI smoke from step 57. These are not new measurements or
worker-authored baselines. No ui_tests checkpoint exists in the current file.

```yaml
gates:
  constructor_unit_tests: 807
  ui_smoke_tests: 208
```

## Architecture

Each stored version carries the existing PinState. New entries and minted
versions start unpinned; saving an existing latest version passes its pin to
decideVersioning. Metadata-only edits remain in place for either pin state;
pinned physics edits mint a new unpinned successor. Record updates preserve pins
through identical saves, metadata edits and all three lifecycle verbs.

The proxy pin verbs address one exact version, are idempotent and reject unknown
ids/versions with typed diagnostic errors. No operation unpins. Removal checks
every stored version; material sample-reference refusal retains precedence.
MaterialError.unknownMaterialVersion owns the diagnostic shared by pinning and
Propagation.resolveMaterialVersion.

VersionRef, ElementBinding.versionRefOpt and Experiment.versionRefs remain as
binding data. They do not infer or set a store pin. The retired lookup types and
adapters are deleted, without compatibility shims. No serialized format, schema,
package pin or version ring changes.

## Deferred

No implementation requirement is deferred. Acceptance execution remains with
the supervisor; commit readiness is not asserted before that verification.

## Gotchas

- A version reference is not itself a pin operation; firstOf only constructs a reference.
- The slice explicitly permits metadata changes to pinned versions; physics remains frozen.
- Local logs and TRX files are under specs/0050/.artifacts/0065-local-*.
- The pre-existing supervisor modification to .manifest.state.json was left untouched.
- No git mutation, analyzer configuration change or known-findings change was made.

## Changelog

- 2026-11-06: Implemented one-way version pins, removed the usage seam, updated
  the 40 declared files and preserved fact counts; local builds, 159 selected
  constructor tests and 303 selected UI tests passed; acceptance is with the supervisor.

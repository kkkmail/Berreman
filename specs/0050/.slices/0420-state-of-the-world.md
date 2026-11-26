# Where we are

Slice 0420 implements S121-KIT-IDS at the start of S12.1: kit creation now
accepts an injected identity provider, supporting reproducible kits built
through semantic messages. The six authorized source files and both required
regression facts are updated. Supervisor verification remains pending.

# What's working

- Added KitIdProvider and its system implementation at the composition boundary.
- Routed CreateKit through the injected provider, minting only during initialization.
- Preserved openKitAt's signature and composed the provider in every start-screen context.
- Added the two authorized facts for persisted identity and opening without minting.

These bullets describe delivered source, not measured runtime results.

# Tests

No build, test, analyzer or invariant command was executed. IMPLEMENT Invariant
6 assigns gate execution and verdicts to the supervisor after worker exit.
commit_ready=false reflects pending verification, not incomplete implementation.

The unchanged 0420 roster requires build and arc-origin elevation (600 seconds
each), all constructor-unit-tests (180 seconds), all ui-smoke (600 seconds),
selected non-smoke ui-tests over OpticalConstructor.Ui.Tests.KitStartWindowTests
(360 seconds), and all twenty-one s41 invariants (300 seconds each). There is no
development-run duration or deadline overrun to report.

Exactly two new constructor fact identities were added:

- OpticalConstructor.Tests.KitStartTests.a new kit takes its id from the start screen's kit-id provider
- OpticalConstructor.Tests.KitStartTests.opening an existing kit, read-write or read-only, mints no kit id

The first uses the specified e63c9d6e-7961-4d9f-9c8d-e5a151414e0e ID, requires
exactly one provider call, and checks both the handed-over kit and a real
KitProxy read of its persisted metadata. The second observes zero provider
calls through OpenKit, OpenKitReadOnly, CreateKit on an existing kit, and direct
openKitWith calls in both access modes. Every session closes before reopening.
Fixtures remain under the test output folder and reuse the existing cleanup.
All existing assertions are retained; the UI suite gains no test identity.

The YAML inherits constructor and smoke counts from supervisor checkpoint 418.
No ui_tests entry exists in that registry; 16 is the prior handoff's historical
floor, not a measurement for this selected class. Expected effects are +2
constructor cases (1,756 after verification), unchanged smoke and unchanged
selected UI cases. The supervisor owns measured counts and checkpoint updates.

```yaml
gates:
  constructor_unit_tests: 1754
  ui_smoke_tests: 264
  ui_tests: 16
```

# Architecture

KitIdProvider is a reference-equality record beside ItemIdProvider in Domain.
Its UI extension supplies a fresh GUID on each mint. KitOpening.openKitWith
accepts the provider and reuses KitContext.openOrInitialise's deferred
ClockTime-to-metadata factory; only that callback invokes mintKitId. Existing
kits and opening errors other than NotAKitFolderErr never invoke the factory. Read-only access uses
KitContext.openKit directly and cannot initialize.

openKitAt keeps all its explicit argument and return types and forwards through
KitIdProvider.system. KitStartContext carries kitIds directly after clock;
CreateKit consumes it, and the real window plus both test context factories
compose the system provider. No storage format, proxy, message, option, schema
or migration was added. The scratch-kit mint and its Result contract are kept.

# Deferred

No requested implementation is deferred. Supervisor execution of the frozen
gate roster remains required before acceptance.

# Gotchas

- Keep mintKitId inside the deferred metadata callback: partial application of
  namedKitFile with a minted ID would eagerly consume one on every read-write open.
- OpenKit and OpenKitReadOnly deliberately keep their existing routes. The
  extra existing-kit CreateKit and direct openKitWith assertions exercise the
  injected provider as well, within the same authorized regression identity.
- Source diff inspection found matching ordinary and ignore-CR-at-EOL numstats;
  no line-ending churn was introduced. No runtime verification is claimed.
- Preserve the pre-existing specs/0050/.manifest.state.json modification.
  No Git mutation, version bump, gate/oracle/census/checkpoint rewrite, analyzer
  exception, external repository edit or notebook edit occurred.
- Worker documents use the task's exact paths. No runtime artifacts were produced.

# Changelog

- 2026-11-14: Injected kit identity generation into CreateKit, deferred minting until initialization, added both authorized regression facts and handed verification to the supervisor.

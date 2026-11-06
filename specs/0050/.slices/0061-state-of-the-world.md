# Step 0061 state of the world

## Where we are

Slice 0061 implements S21-DB-TAGS after the library database model: the
state types now supply their disk tags and sample material references
preserve version keys. All four authorized product/test files are complete.
Formal gate acceptance and checkpoint recording belong to the supervisor.

## What's working

- Added 1-based database tags and unknown-tag rejection to lifecycle and protection states.
- Added PinState with VersionPinned/VersionUnpinned and the documented one-way pin contract.
- Preserved every referenced material version through one sample-structure walk.
- Projected referencedMaterials from the version set and added both declared regression facts.

## Tests

Local Release/x64 solution compilation succeeded with zero errors. All 797
constructor cases passed with zero failures or skips, including both new
facts and the unchanged referencedMaterials coverage fact. The two build
warnings are SYSLIB0051 in unchanged vendored MathNet Exceptions.cs, also
present in slice 0060; no warning was suppressed. Our F# code and project
configuration emitted no warnings. Diff inspection found no whitespace
errors or line-ending churn in the four edited F# files.

These are local development results required by the coding-session
instructions, not formal gate acceptance. The supervisor must run build,
elevation, constructor-unit-tests and all nine s08 invariants from
0061.gates. This worker did not execute elevation or the invariant gates.
The YAML below retains the existing checkpoint 59 baseline; it does not
record a new measurement. Local logs and TRX are listed in 0061-impl-log.md.

```yaml
gates:
  constructor_unit_tests: 795
```

## Architecture

EntryLifecycle, EntryProtection and PinState use explicit 1/2 mappings and
return None for every unknown database tag. PinState is only a domain type
in this slice; VersionsInUse and stores retain their behavior.

SampleStructure.referencedMaterialVersions visits film layers, each repeated
cell once, the optional substrate plate and the optional lower half-space.
The set retains distinct versions of one material. referencedMaterials is
only its materialId projection, preserving identity-based removal checks.
No durable storage shape, format, migration or version ring changes.

## Deferred

No slice implementation is deferred. Formal verification is the supervisor's
part of this cycle; commit readiness is not asserted before it completes.

## Gotchas

- Do not expand repeated groups to obtain references or discard their material versions.
- None in the lower half-space contributes no explicit reference, preserving existing semantics.
- The original referencedMaterials coverage fact remains unchanged.
- The initially dirty .manifest.state.json belongs to the supervisor and was not edited.
- No gate, oracle, checkpoint, manual note, version file or store was changed.

## Changelog

- 2026-11-06: Added lifecycle/protection/pin database tags and version-preserving sample references; locally built Release and passed all 797 constructor cases.

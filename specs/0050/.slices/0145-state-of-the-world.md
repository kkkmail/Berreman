# Where we are

Step 0145 partially implements S43-OPEN-SEAM. Folder-derived naming, the asynchronous picker contract and read-write open-or-initialize behavior are implemented with seven authorized concrete test identities. The step is blocked, not complete: the prescribed Domain-only, infallible in-memory context constructor conflicts with the current Storage-owned serialized proxy factory. No gates were run and commit readiness is not claimed.

# What's working

- Added folder-leaf kit naming with slash normalization and drive-root rejection.
- Added the reference-equality asynchronous folder-picker seam.
- Added open-or-initialize orchestration through existing kit locking, stamping and access controls.
- Added seven authorized test identities covering naming, initialization, preservation, error propagation and read-only recovery after a foreign lock.

# Tests

No build, test, analyzer or invariant gate was run, per IMPLEMENT Invariant 6. The resolved supervisor roster is build, elevation, all-discovered constructor-unit-tests and eleven s17 invariants.

The new naming fact covers the three specified path spellings, complete created metadata, drive roots and whitespace leaves. Three new shared contract facts run under both KitProxyTests and KitProxyFolderTests. They cover exact fresh-kit paths, pre-existing lab files, one metadata-constructor call at the supplied time, preserved existing metadata bytes, recorded stamps, construction and initialization failures, IO/schema read failures, unchanged foreign locks and successful read-only recovery. Existing test identities and assertions are unchanged.

The required eighth identity, `OpticalConstructor.Tests.KitProxyTests.an in-memory kit context holds its kit file and writes through its own folder`, is not added because the required production constructor is blocked. Its owning step is 0145, path OpticalConstructor.Tests/KitProxyTests.fs, discovery gate and suite constructor-unit-tests, obligation S43-OPEN-SEAM. The required eight-case increase has not been delivered or measured.

This YAML repeats the existing checkpoint 142 count, not a new baseline or a result from this round.

```yaml
gates:
  constructor_unit_tests: 1129
```

# Architecture

`KitFolder.namedKitFile` uses string normalization and `KitName.tryCreate` followed by `KitFile.create`; it does no filesystem IO. A drive designator left after stripping trailing separators is treated as an empty leaf, meeting the explicit C:/ rejection requirement.

`KitFolderPicker.pickKitFolder` returns `Async<KitFolder option>`, so a UI can supply an asynchronous dialog without blocking its dispatcher. `KitContext.openOrInitialise` calls `openKit` read-write, initializes only on `NotAKitFolderErr`, then calls `openKit` again. Other errors pass through unchanged. No proxy field, durable representation, schema version or migration changed.

The blocked constructor cannot reuse the current factory from its prescribed layer. Domain/KitFolders.fs's `InMemoryKitFolder.createKitProxy` requires metadata serialization, lock serialization, lock parsing and snapshot validation functions. The sole `KitProxy.createInMemory` implementation lives in Storage/KitFolderStore.fs and supplies those functions through the existing canonical JSON/schema code. Storage already references Domain. Calling it from Domain creates a project cycle; copying serializers violates reuse; placeholders or ignored initialization errors violate the real proxy contract. The requested constructor has neither injected dependencies nor a Result channel.

# Deferred

No work is assigned to a later slice. This step remains blocked on an authority decision for `KitContext.createInMemory` and its one test.

Required decision: authorize a Storage extension reusing the existing factory with `Result<KitContext, KitError>`, add Storage/KitFolderStore.fs to step 0145's edits, and adjust the downstream S43-OPENING construction contract; or specify an authorized lower-level serializer seam and move. The current effects fix the member in Domain, the two-argument infallible signature, four editable files and no additional member/option/verb. The worker has not amended that contract.

# Gotchas

- The existing serialized in-memory proxy is newer than the simple step-125 construction described by this slice. Its Storage dependencies cannot be replaced by a fake metadata encoding or a second parser.
- Seven of eight authorized concrete identities are authored. No census entry, gate, oracle, checkpoint or baseline was edited to conceal the missing one.
- The supervisor-owned specs/0050/.manifest.state.json was dirty before the worker started and remains untouched.
- Source edits are additions in exactly the four authorized files. Diff numstats match with and without ignoring CR-at-EOL; worker documents use LF and only the supplied output paths.
- No git mutation, version bump, external repository write or persistent run artifact was produced.

# Changelog

2026-11-07 — Added folder naming, asynchronous picker and open-or-initialize behavior with seven authorized test identities; blocked the remaining in-memory constructor on its Domain/Storage construction contract conflict, with supervisor verification pending.

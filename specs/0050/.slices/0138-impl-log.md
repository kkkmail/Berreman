# Implementation log — 0138

## Progress

- Read the task, worker protocols, project rules, slice and gate snapshot, naming conventions, and shared lock recovery ruling (`0050 .manual/009`). Initial working tree contains an unrelated supervisor change to `.manifest.state.json`; preserve it.
- Before test edits: inspect the existing kit contract fixtures and canonical library serialization so the ten authorized identities reuse production fixtures and parsers.
- Added the domain naming fact, three inherited snapshot contract facts (six discovered cases), and three filesystem facts; widened error-rendering coverage. The filesystem facts also cover absent library creation, BOM/case handling, schema/version/hash rejection, wrong-version/kind filenames, and top-level-only enumeration without adding identities.
- Before implementation: add snapshot names and errors in Domain, shared in-memory state and read-only routing, then reuse canonical Storage serialization, parsing and atomic publication. The immutable acceptance oracle requires validation on read in both implementations; supply the in-memory listing with Storage's shared validation callback rather than duplicate the parser in Domain.
- Implemented snapshot naming, immutable in-memory entries, the two proxy fields and read-only routing. Storage renders canonical UTF-8 bytes through the existing new-file writer, maps duplicate publication to SnapshotExistsErr, and lists top-level entry files through the shared schema/version/hash and filename validation. The lock guard and writer implementation remain untouched.
- Before final refinement: review both implementations against the immutable oracle, extend the authorized shared fact to cover multiple versions and validation in memory as well as on disk, and preserve exactly the authorized discovery identities.
- Extended that shared fact with versions 2 and 10 (ordinal filename ordering) and invalid schema-kind, schema-version and hash envelopes. Both implementations list through `LibraryFileText.parseEntry`; the in-memory store receives the same Storage callback through its existing serializer-injection seam. UTF-8 BOM removal is performed once by the existing byte decoder.
- Source review found no additional full `KitProxy` record constructors or exhaustive `KitError` matches needing changes outside the authorized files. Normal and ignore-CR-at-EOL source diff counts agree. No gates were executed.
- Completed the implementation and mandatory handoff files. No implementation obligation is deferred; supervisor verification remains pending.
- Normalized the five source files and three worker documents to UTF-8 without BOM and LF endings. Final source diff counts match with and without ignoring CR-at-EOL; only the five authorized source files and three handoff documents were changed by this worker.

## Files modified

- This log and the implementation plan at the task's prescribed paths.
- `KitTests.fs` and `KitProxyTests.fs` (only authorized facts and their support).
- `Kit.fs`, `KitFolders.fs`, and `KitFolderStore.fs` (snapshot contract and implementations).

## Testing state

No gates run. IMPLEMENT Invariant 6 assigns build, elevation, constructor tests and all eleven section invariants to the supervisor. No measured counts or pass claims are authored by this worker.

The ten new discovered identities are one `KitTests` fact, three inherited `KitProxyContract` facts in each concrete class, and three `KitProxyFolderTests` facts. The existing error-rendering fact retains all prior assertions and now covers the tenth error case. The last supervisor constructor baseline is 1,096 (checkpoint 136); the expected total after this slice is 1,106, not measured here. The exit summary leaves `commit_ready` false because verification has not been performed by this worker.

## Artifacts

- `specs/0050/.slices/0138-impl-plan.md`
- `specs/0050/.slices/0138-impl-log.md`
- `specs/0050/.slices/0138-state-of-the-world.md`

## Gotchas

- Preserve the bounded, exclusive kit lock guard from `.manual/009`; snapshots reuse the independent non-overwriting atomic byte writer.
- The round adds exactly ten discovered tests and widens only the declared error-rendering test.
- No durable schema changes, migration, cache, overwrite option, new gate, test-census amendment or version bump. The injected in-memory read validation implements the oracle's shared validation requirement without introducing a Domain dependency on Storage.

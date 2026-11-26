# Where we are

Slice 0125 declares `STORE_BigBoy_0004` for spec 0050 S4.1 and addresses `S41-KIT-PROXY`. The three-field kit proxy, shared in-memory folder and five prescribed reusable contract facts are written and wired into their projects. Verification belongs to the arc-runner after this ADD_CONTRACT worker exits; no gate outcome is claimed.

# What's working

- Declared the reference-equality kit proxy for initialisation, metadata reads and whole-file replacement.
- Added a shared in-memory folder with private metadata, text and folder state and fresh proxies over that state.
- Preserved lab paths and parent folders, refused outside metadata writes and listed contents in ordinal order.
- Added the five prescribed inherited contract facts with deterministic kit metadata and a stepping clock.
- Wired the Domain file immediately after Kit.fs and appended the test file to its compile list.

# Tests

No build, test, analyzer or invariant checks were run. ADD_CONTRACT invariant 6 explicitly assigns gate execution to the supervisor. The resolved roster is build, elevation, all discovered constructor-unit-tests and the eleven s15 invariant gates. The supervisor must measure the required increase of five cases; no new test count is reported here.

The facts cover missing-kit read/write refusal with no contents created; exact initial kit paths and metadata; preservation of pre-existing lab paths; second-proxy initialisation refusal with unchanged metadata and contents; and whole-file replacement visible through other proxies. Additional assertions within those same five identities cover already-open proxy visibility and replacing metadata back to its original empty notes and exclusions.

The YAML below repeats the inherited `constructor_unit_tests` count from checkpoint 123. Checkpoint 124 is absent in the checkpoint file read by this worker. This value is not a new baseline or a result measured in this round.

```yaml
gates:
  constructor_unit_tests: 1024
```

# Architecture

`KitProxy` has exactly `initialiseKit`, `readKitFile` and `writeKitFile`. `InMemoryKitFolder` holds one private `KitFile option`, one map of every other text by `KitRelativePath`, and a set of existing folders. Its public surface is `create`, `placeLabFile` and `paths`. An internal factory creates fresh proxy records whose closures capture that same folder state; `KitProxy.createInMemory` calls it without caching or copying the contents.

Only initialisation creates metadata and adds `data` and `library`. Reading or writing without metadata returns `NotAKitFolderErr`; reinitialisation returns `KitAlreadyExistsErr`, and refusal branches write nothing. Outside writers replace text by path, add every parent folder and receive `InvalidKitPathErr` for the reserved metadata filename. The filename comes from `KitFile.fileName` through a source-type conversion. Paths are deduplicated and sorted ordinally.

`KitProxyContract` owns all five facts and consumes only the `KitTestFolder` adapter. `KitProxyTests` creates a fresh in-memory folder for each fact and opens a new proxy over it on every adapter call. No real file IO, lock, retry, additional proxy field or durable format is introduced. The existing contract registry already records `STORE_BigBoy_0004` as declared at step 125, so it remains unchanged and unimplemented.

# Deferred

No slice implementation requirement is deferred. The required verification is the supervisor's pending stage under the act-only worker protocol; the real filesystem adapter is outside this ADD_CONTRACT slice.

# Gotchas

- All text files other than the typed kit metadata share one map; later kit-file kinds must use that map rather than introduce a separate store.
- Tests exercise both already-open and newly opened proxies, so per-proxy cached metadata would violate the contract.
- Metadata filenames are protected case-insensitively; reported paths are ordered ordinally.
- The five facts stay on the abstract contract to support a filesystem adapter without duplicating assertions.
- Both project files add one compile line without comments or line-ending churn; new source and worker-output files use LF.
- The pre-existing `.manifest.state.json` modification is supervisor-owned and untouched. No gate, census, checkpoint, registry, analyzer configuration, version ring or neighbouring repository was changed.

# Changelog

- 2026-11-07: Declared the kit-folder proxy, added its shared in-memory implementation and five reusable contract facts, and prepared the slice for supervisor verification.

# Where we are

Slice 0159 completes the S4.4 `S44-PROJECT-FILE` retirement within its authorized source and test effects. The unused `.ocproj` file IO and autosave sidewrite are deleted. Recent-path persistence stays intact, and kit-file tests retain the replacement storage obligations. Implementation is complete; the supervisor owns executable verification under the IMPLEMENT worker protocol.

# What's working

- Deleted ProjectFile.fs and its compile entry, retiring the extension and project save/open members.
- Removed Autosave and its unused imports, stale documentation and ignore block.
- Preserved RecentFiles implementation bodies and changed the retained R-1 test to kit-folder paths with all assertions intact.
- Deleted RoundTripTests and AC-I6 with their exclusive fixtures, preserving the existing KitContentFileJsonTests successor coverage.

# Tests

No build, test, analyzer, acceptance grep or invariant gate was executed by this worker. The supervisor runs build, elevation, constructor-unit-tests (all discovered) and the twelve s18 invariant gates from `0159.gates`, and verifies the required retirement search. Source diff review shows that the remaining RecentFiles bodies and all five R-1 assertions are retained. Ordinary and ignore-CR-at-EOL source diff counts match.

The following count is inherited from checkpoint 157, not measured or authored in this round. Deleting three non-parameterized RoundTripTests facts and the one AC-I6 fact implies 1142 constructor cases, a reduction of four. The supervisor must measure and record the result; no checkpoint, baseline, census, oracle or gate was edited.

```yaml
gates:
  constructor_unit_tests: 1146
```

# Architecture

Kit storage replaces the retired standalone project-file and sidewrite paths, without migration or compatibility readers, as required by spec 0050 §7 and S4.4. Canonical serialization and schema refusal are already covered by the unchanged KitContentFileJsonTests facts. RecentFiles retains maxRecent, prefsPath, bump, loadRecent and FileName.pushRecent unchanged for the subsequent history work. No new persistence shape or API was introduced.

# Deferred

No implementation work was deferred. Supervisor verification is pending as required by Invariant 6; `commit_ready` is false because the worker does not certify gates.

# Gotchas

- ExperimentDataStore.fs already had the required kit-aware two-line module documentation and no obsolete precedent sentence, so no edit was necessary.
- The declared successor action is satisfied by the existing kit round-trip, invalid-schema and unsupported-version facts, as the slice explicitly directs. Copying the old project-specific facts would duplicate coverage and violate the four-deletion census.
- HistoryTests remains compiled for its single R-1 fact. KitContentFileJsonTests remains unchanged and compiled.
- The supervisor-owned `.manifest.state.json` was modified on entry and was not edited by this worker.
- No version bumps, git mutations, cross-repository edits, analyzer configuration changes or notebook edits were made.

# Changelog

- 2026-11-08: Retired standalone project-file IO and autosave with four obsolete facts; preserved recent-path behavior and kit-file successor coverage.

# Where we are

Step 0140 implements S42-KIT-ORIGIN after the kit snapshot storage and closure work. Kit snapshots now have their own import origin and use the shared decision table with defaults that reuse held content or retain the kit's copy. Implementation and the two authorized facts are complete; executable verification belongs to the arc-runner after worker exit.

# What's working

- Add the named kit snapshot origin and render collision rows with the kit's name.
- Default kit rows to identical-content reuse or skipping the incoming entry, preserving the kit's copy.
- Offer explicit adds, versions without moving latest, divergence forks and ordered same-name rebindings.
- Exclude category rebindings while preserving catalogue and dropped-file decisions.
- Add the two authorized facts without changing existing assertions.

# Tests

No build, test, analyzer or invariant gate was run, as required by IMPLEMENT Invariant 6. The supervisor owns build, elevation, all-discovered constructor-unit-tests and the eleven S16 invariants in `0140.gates`. No passing result or commit readiness is claimed.

The new facts cover missing ids, identical versions, unheld built-in versions, hash and kind divergence, material collision order, category collisions, default choices, origin and row rendering, and retention of all kit rows for confirmation. Exactly two fact identities were added; the expected constructor suite count is 1,116, not yet measured.

This YAML repeats the existing supervisor baseline from checkpoint `138`; no checkpoint, test census, acceptance oracle or gate definition was edited.

```yaml
gates:
  constructor_unit_tests: 1114
```

# Architecture

`KitSnapshotOrigin` reuses `Kit.KitName`, which already compiles before the import module. The existing classification determines each status and available import action before kit-specific action selection. Identical content keeps reuse; other kit rows select skip and offer the original import action. Collision rebindings reuse the existing ordered alternatives, with categories excluded. The catalogue-only latest-move condition continues to keep latest fixed for kit imports.

The existing apply seam already treats reuse and skip as no-write operations. No IO, storage contracts, durable formats, migrations, project wiring or dialog behavior is introduced. `split` continues to return all rows for confirmation, including reuse.

# Deferred

None of the requested implementation is deferred. Supervisor gate execution remains pending under the worker protocol.

# Gotchas

- `.manifest.state.json` was already modified at entry and remains untouched by the worker.
- The slice's reference to an unchanged `mode` does not correspond to a member in the current import module; no new mode was introduced.
- The source diff preserves all earlier import decision tests and all catalogue/dropped-file action selection.
- Source review does not establish compilation, analyzer compliance or passing tests; the exit summary leaves commit readiness false pending supervisor gates.
- Version files, shared storage guards, analyzer settings, known findings, git history and index are unchanged.

# Changelog

2026-11-07 — Added kit snapshot import decisions with read-only defaults, explicit import alternatives, category rebinding exclusion and the two authorized facts.

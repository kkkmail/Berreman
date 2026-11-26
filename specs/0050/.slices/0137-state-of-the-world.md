# Where we are

Step 0137 implements S42-REFERENCES: the pure domain description of the library
entries and versions a kit needs to snapshot. The three authorized domain files
and the two authorized test modules are updated. Implementation is complete;
supervisor verification is pending.

# What's working

- Add exact-version and latest-entry library references with diagnostic descriptions.
- Derive material-category and sample-material references through one content walk.
- Expose an envelope's own entry identity and version.
- Project lifecycle and element bindings into library references.
- Collect experiment references once each while preserving setup order.
- Add the two authorized facts while preserving existing reference assertions.

# Tests

No worker build, test, analyzer or invariant gate was run, per IMPLEMENT
invariant 6. No gate pass or newly measured count is claimed. The supervisor
owns build, elevation, all-discovered constructor-unit-tests and the eleven
s16 invariants in 0137.gates.

The entry-reference fact covers Silicon's latest category, the shipped
quarter-wave sample's glass and vacuum versions in set order, another version
of the same material, exact/latest descriptions and ids, envelope version
projection, absent categories and all other entry kinds. The experiment fact
covers the required sample/source/source/unbound setup, exact and latest
references to the same entry, multiple versions, material bindings, repeated
bindings, first-occurrence order in both directions, invalid preset ids and
empty setups. The existing `an entry's references are its category and its
materials` fact is unchanged.

Exactly two facts were added. The expected count is 1096, an unmeasured increase
of two. The YAML carries only the supervisor's existing baseline from
checkpoint 134, without inventing a new measured count.

```yaml
gates:
  constructor_unit_tests: 1094
```

# Architecture

LibraryVersionRef contains LibraryEntryId and VersionNumber. EntryReference
distinguishes an exact LibraryVersionRef from an entry requested at its latest
version. LibraryEntryContent.references reuses the existing category id
conversion and SampleStructure.referencedMaterialVersions; referencedEntryIds
projects those references to a set. This preserves the existing identity-only
answer while retaining versions for snapshots.

VersionRef.entryReference and ElementBinding.entryReferenceOpt reuse the
existing one-way id conversions. Experiment.libraryReferences chooses bound
references and uses structural distinctness in setup order. Different versions
and exact/latest references remain distinct. No reverse conversions, extra
members, serialization changes, migrations or compatibility paths were added.

# Deferred

None of the requested implementation is deferred. Executable verification
belongs to the supervisor under the worker protocol.

# Gotchas

- specs/0050/.manifest.state.json was already modified and was left untouched.
- Normal and ignore-CR-at-EOL source diff counts agree; the source changes do
  not introduce line-ending churn.
- Source inspection does not establish a passing build, test suite or analyzer
  result. The exit summary therefore does not claim commit readiness.
- Test identities, gate membership, acceptance oracle and measured checkpoints
  were not edited.

# Changelog

2026-11-07 — Added typed library references, shared content reference projection,
ordered distinct experiment references and the two authorized regression facts.

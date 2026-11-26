# Where we are

Slice 0113 implements S33-ATTRIBUTION in S3.3. Domain attribution, shared
catalogue fixture builders, and all three declared acceptance facts are
complete. All resolved local checks passed; the supervisor owns formal
acceptance and checkpoint updates.

# What's working

- Added one CC-BY citation rule and exact licence display text in Domain.
- Preserved pulled-entry attribution recursively through fork ancestry.
- Projected an entry's own identity followed by its sample's material identities.
- Added five catalogue-backed fixture helpers shared by both test suites.
- Added the three declared facts, including unlicensed forks and non-GUID ids.

# Tests

- Confirmed the missing attribution API before implementation.
- Release solution build passed with zero warnings and errors (47.50 seconds).
- Elevation passed with zero findings across seven projects (150.58 seconds),
  excluding ELEV004 as prescribed.
- All-discovered constructor-unit-tests passed 988/988 with no failures or
  skips (40.42 seconds).
- All eleven s14 invariants passed using the resolved slice roster.

TRX identity comparison against the preceding full constructor run confirms
985 to 988 cases: exactly the three declared additions, with no cases removed.
The three authorized source files are LF-only, with no whitespace errors or
line-ending churn. No product edits followed passing verification.

```yaml
gates:
  constructor_unit_tests: 988
```

Logs and TRX output live under `specs/0050/.artifacts/0113-attempt-01-*`.
The count above is a measured result; no supervisor checkpoint was edited.

# Architecture

`EntryLicence.attributionWith` owns the case-insensitive CC-BY prefix rule.
`EntryProvenance.attributionOpt` follows fork ancestry to the original pulled
entry and returns no attribution for built-in, user, or kit-import origins.
`EntryAttribution` and `EntryAttributionLine` provide the display renderers.
`LibraryEntry.attributedEntryIds` uses the existing GUID conversions and
`SampleStructure.referencedMaterials`, preserving its set order.

The fixture helpers reuse the shipped catalogue decoders, content builders,
and seal/put seams. The new types are derived display data, with no persisted
shape, schema, migration, package, or version changes.

# Deferred

None within slice 0113.

# Gotchas

- `BuiltinCatalogue.fs` is linked into both constructor test suites.
- Fixture layer thickness uses `Berreman.Media.Thickness`.
- The session's higher-priority verification instructions require local checks
  despite the task-file act-only wording; the supervisor remains the formal
  acceptance authority.
- The pre-existing `.manifest.state.json` edit belongs to the supervisor and
  remains untouched.

# Changelog

- 2026-11-07: Implemented licence attribution, recursive fork credit, attributed
  entry identities, shared catalogue helpers, and the three declared facts;
  all resolved local checks passed, including 988 constructor cases.

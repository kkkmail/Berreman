# Where we are

Slice 0114 implements S33-ATTRIBUTION-READ in S3.3. The shared library read and
its contract over in-memory and SQLite stores are complete. Every resolved
local gate passed; formal acceptance and checkpoint updates belong to the
supervisor.

# What's working

- Added ordered, distinct attribution reads from held entries' latest provenance.
- Omitted missing and unlicensed entries while retaining licensed fork credit.
- Propagated the first read error without later store reads or any writes.
- Added the declared contract fact over both in-memory and SQLite stores.
- Covered empty input, exact display text, latest-provenance changes, and read order.

# Tests

- Pre-implementation build confirmed the missing `attributionLinesOf` member.
- Release solution build passed with zero warnings/errors (46.72 seconds).
- Elevation passed across seven projects with zero gated findings (144.53
  seconds), with ELEV004 excluded as prescribed.
- Full constructor-unit-tests passed 990/990 with no failures/skips (42.07
  seconds).
- All eleven s14 invariants passed using the resolved slice roster.
- TRX comparison with slice 0113 confirmed exactly the two declared additions,
  increasing the count from 988 to 990 with no removed cases. Both edited
  source files remain LF-only with no whitespace errors or diff churn.

```yaml
gates:
  constructor_unit_tests: 990
```

This is a measured result; no supervisor baseline or checkpoint was edited.
Logs, analyzer reports, TRX, and identity review are under
`specs/0050/.artifacts/0114-attempt-01-*`. No product edit followed passing checks.

# Architecture

`CatalogueImportProxy.attributionLinesOf` reads each distinct requested id
through `tryHeldEntry`, preserving input order. The existing
`EntryProvenance.attributionOpt` owns licence and fork-ancestry rules, and
`EntryAttributionLine` owns display text. The method uses the held latest
metadata and introduces no cache, per-version query, formatting rule, or write.

The inherited `CatalogueImportContract` fact reuses the shipped catalogue,
`BuiltinCatalogue.licensedMaterialContent`, `userMaterialContent`, and
`putSealedEntries`. The two store implementations run the same assertions.
No durable type, serialized schema, migration, UI, package, or version changes
were needed.

# Deferred

None within slice 0114.

# Gotchas

- `importFrom` returns decisions; the fixture applies default choices before
  adding licensed entries that depend on the shipped catalogue categories.
- The session's verification instructions take precedence over the task-file
  act-only wording. The supervisor remains the formal acceptance authority.
- The red build emitted existing SYSLIB0051 warnings in vendored MathNet;
  the final Release build reported zero warnings without suppressions.
- The pre-existing `.manifest.state.json` edit belongs to the supervisor and
  remains untouched.

# Changelog

- 2026-11-07: Added ordered attribution reads and the inherited two-store
  contract; all resolved checks passed, including 990 constructor cases.

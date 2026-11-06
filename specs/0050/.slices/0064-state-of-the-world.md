# Step 0064 state of the world

## Where we are

Slice 0064 implements S21-RELATIONAL-KEYS on the existing S2.1 library
database rows. Each envelope now projects its own discrete facets and, for
samples, its referenced material versions. Both members and all four declared
facts are complete. Acceptance remains with the supervisor's gate engine.

## What's working

- Added distinct discrete facet rows keyed by entry identity and version.
- Preserved category GUID keys and excluded numeric and material-derived sample facets.
- Added sample material links preserving every referenced material version.
- Added four facts covering built-ins, decoder refusals, persistence and cascades.

## Tests

Local development verification built OpticalConstructor.Tests and its
dependencies in Release/x64 with zero warnings/errors. All 11 facts in
OpticalConstructor.Tests.LibraryDatabaseTests passed, including the four exact
facts declared by this slice; none failed or were skipped. Logs and the TRX
are under specs/0050/.artifacts/0064-local-*.

The development environment requires relevant local verification, conflicting
with the task's act-only restriction. These scoped checks do not assert any
acceptance gate result. The supervisor must run the resolved 0064.gates roster:
build, elevation, constructor-unit-tests and the nine s08 invariants.

This YAML copies the latest available supervisor baseline, checkpoint 62;
it is not a newly measured or worker-authored suite baseline:

```yaml
gates:
  constructor_unit_tests: 803
```

## Architecture

LibraryEntryEnvelope.facetRows and sampleMaterialRows carry StorageBoundary.
Facet projection decodes each declared kind through its existing domain
decoder, traverses applicable LibraryFacets attributes over empty category
and material lists, retains only DiscreteValue pairs, deduplicates them and
writes their key text. A category contributes no facets. Decoder refusals
become LibraryRowInvalidErr carrying the original describe text.

Sample links reuse SampleStructure.referencedMaterialVersions; all other kinds
contribute none. Owner identity/version come from envelope content, and each
material identity/version comes from the reference. There is no new schema,
index, serialized contract, parser, cache or migration.

The persistence fact saves every built-in's version, facet and material rows
in one run, compares every relational column in a fresh run, then deletes only
the quarter-wave multilayer version in another context. A fresh read confirms
both cascades and preservation of every unrelated row.

## Deferred

No implementation requirement is deferred. Acceptance gate execution belongs
to the supervisor; commit readiness is not asserted before that verification.

## Gotchas

- Empty category inputs deliberately render material categories as GUID text,
  so category renames cannot stale relational rows.
- Empty material inputs prevent denormalizing material facets onto samples;
  the sample-material relation holds that information instead.
- Test imports must select MaterialLibrary.VersionNumber, not the similarly
  named string type from Softellect.Sys.Primitives.
- The extra version test changes in-memory sample content without storing it;
  file/hash validation remains the existing versionRow responsibility.
- The supervisor's existing dirty .manifest.state.json was left untouched.
- Both edited source files use LF; whitespace inspection and ordinary versus
  ignore-CR-at-EOL diff counts show no line-ending churn.

## Changelog

- 2026-11-06: Implemented relational facet/material keys and four declared
  facts; local compilation and all 11 database tests passed; handed acceptance
  verification to the supervisor.

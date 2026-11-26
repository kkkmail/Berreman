# Where we are

Slice 0093 implements S31-DECISION-TABLE in S3.1. Domain now describes all six per-entry import outcomes and their default/alternative actions without reading or writing stores. Every row remains awaiting confirmation. Implementation and the resolved local checks are complete; supervisor acceptance remains separate.

## What's working

- Added held-entry history and the pure six-row import decision table with explicit choices.
- Restricted latest-version moves to newer protected built-ins arriving from their own catalogue.
- Preserved every decision for confirmation and kept collision captions separate from entry names and hashes.
- Added category/material reference projection and nine shipped-catalogue-based regression facts.

## Tests

- Regression-first compilation failed on the new missing production types/members as expected.
- Final Release solution build passed with zero warnings/errors (13.99 seconds).
- Elevation passed over seven affected/dependent projects with zero new/known findings (148 seconds, ELEV004 excluded).
- All 921 constructor cases passed with zero failures/skips (18.628 seconds), up exactly nine from the prior measured 912. All nine required test identities were verified as Passed in the TRX.
- All eleven s12 invariants from 0093.gates passed. Source whitespace review passed; all five source/project files are LF and diff counts match with/without CR-at-EOL ignored.

Local checks follow the higher-priority workspace verification requirement. These are observed results, not edits to supervisor checkpoints or acceptance records.

```yaml
gates:
  constructor_unit_tests: 921
```

Evidence is under specs/0050/.artifacts: 0093-regression-red.log, 0093-build.log, 0093-build-final.log, 0093-elevation.log and 0093-elevation-reports/, 0093-constructor-unit-tests.log/.trx, and 0093-invariants.log. Output-directory overrides change only artifact placement.

## Architecture

ImportDecision.decide uses kind conflict first, then the matching held version and hash, then same-kind/name entries only when the incoming id is unheld. It preserves the supplied name-collision order for rebindings. Only an incoming BuiltInProvenance matching the catalogue origin and newer than held.latest requests MoveLatestVersion. unlisted describes retirement; byDefault packages a selected action without applying it; split retains every input row in order.

LibraryEntryContent.referencedEntryIds reuses category identity conversion and SampleStructure.referencedMaterials. Test envelopes originate in BuiltinCatalogue.catalogueData and changed content is resealed with the existing Storage member. No store, proxy, caller or durable-format change was made; no migration is needed.

## Deferred

None within the requested slice. Supervisor acceptance and commit handling remain external.

## Gotchas

- The older S3.1 brief describes auto-applying independent rows. The slice and .spec-md §3.7 explicitly supersede that behavior: every row, including reuse and unrelated new entries, requires confirmation.
- A name-collision label is only a caption; the entry retains its own name and sealed hash.
- The pre-existing supervisor-owned specs/0050/.manifest.state.json modification remains untouched.
- The regression build exposed existing vendored MathNet SYSLIB0051 warnings and cascading F# inference warnings from missing declarations; the completed builds reported zero warnings/errors.
- No Git mutations, version bumps, other-repository edits/builds, analyzer policy changes or checkpoint edits were made.

## Changelog

- 2026-11-06: Added the pure import decision table, all-row confirmation and entry references; Release build, elevation, all 921 constructor cases and eleven s12 invariants passed locally.

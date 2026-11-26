# Step 0040 - CatalogueImportProxy - state of the world

## Where we are

Spec 0050 section S1.6 now has the catalogue-fed import seam required by
S16-IMPORT. The step's implementation and all nine named facts are complete.
Composition-root and shared-fixture wiring remain the scope of their separately
specified slices; this step adds the read-then-import member they consume.

## What's working

- Replaced seedAll with ordered catalogue decoding and import through the existing save fields.
- Added typed decode/read failures and diagnostic descriptions, preserving earlier saves on failure.
- Added importFrom and retained the four real empty in-memory stores unchanged.
- Replaced five seed-fed facts with eight catalogue-fed facts and retained reference-equality coverage.
- Marked the inherited contract lifecycle implemented without minting or re-declaring its identity.

## Tests

Local constructor verification built the test project and its references, then
passed all 797 tests with zero failures or skips, including all nine required
catalogue-import facts. The first compile exposed FS0691 in an equality assertion;
parenthesizing the condition fixed it without changing the expectation. The
successful run's log and TRX are in `.artifacts/0040-local-tests-retry.log` and
`.artifacts/0040-local-tests/constructor-tests.trx`.

Formal acceptance remains with the supervisor; no gate was green-lit by this
worker. Its resolved roster is pending:

- `build`
- `elevation`
- `constructor-unit-tests`
- `invariant-s06-elev-list-empty`
- `invariant-s06-no-vstest`
- `invariant-s06-no-kit-kit-json`
- `invariant-s06-no-eps-axis-evaluated`
- `invariant-s06-complexity-mandatory`
- `invariant-s06-no-seeded-string-preset-ids`

The YAML retains the supervisor's historical checkpoint-38 count, not a new
baseline. This step replaces six import facts with nine, a net gain of three.

```yaml
gates:
  constructor_unit_tests: 794
```

## Architecture

CatalogueImportProxy retains its four Result-returning save functions and
reference equality. importCatalogue groups envelopes by category, material,
sample, source, polarizer and detector, preserving catalogue row order within
each kind. Decoding occurs only inside the successful fold continuation; the
first decoder or store error prevents every subsequent decode and save. Samples
use only saveSample. importFrom reads once, maps CatalogueError to the import
error and delegates to importCatalogue. Nested errors render through their own
describe members. No retry, cache, rollback or ordering option was added.

The two test files share their small read/decode preparation helpers in
CatalogueImportTestData in the existing first compile item of the pair. All
expectations derive from the shipped catalogue, with structural equality replacing
seed reference identity. No stored format changes or migrations apply.

## Deferred

Nothing within this slice's implementation scope. The supervisor's formal gates
remain pending under the worker protocol; no acceptance obligation is waived.

## Gotchas

- Manifest kind order places categories last; import dependency order places them first.
- An undecodable material fixture intentionally lacks categoryOpt; CatalogueData.tryCreate
  constructs its manifest but leaves kind decoding to the importer.
- Retired seeding names have no matches under Berreman/. The changed source and
  lifecycle files use LF, with no whitespace errors or CRLF diff churn.
- The inherited contract's existing entry is in specs/0038/.contracts-json. Only
  lifecycle changed. implementStep stays null because a bare 40 there would name
  the wrong arc; this log records spec 0050 step 40 without changing registry identity.
- Local test execution follows the coding-session validation requirement despite
  the worker role's act-only wording. Formal gate execution/reporting remains
  the supervisor's responsibility; the final gate collections are empty.
- The first local build emitted two existing SYSLIB0051 warnings from vendored
  MathNet Exceptions.cs, outside the authorized product edits. The successful
  incremental run emitted none, and changed code introduced none.
- The pre-existing supervisor manifest modification is untouched. No version
  bump, git mutation, external repository edit or catalogue-file write occurred.

## Changelog

- 2026-11-05: Implemented catalogue import, typed failures and nine catalogue-backed facts; local constructor verification passed 797 tests and the supervisor handoff was recorded.

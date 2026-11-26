# Step 0040 - IMPLEMENT_CONTRACT STORE_XDUO_0008 - impl log

## Progress

- Read the task, repository instructions, worker and project protocols, slice,
  resolved gate snapshot, naming conventions, S16-IMPORT oracle, operator §B,
  existing seam, decoders, catalogue proxies and both test modules.
- Confirmed the only pre-existing modification is the supervisor manifest state.
  The inherited contract has its lifecycle entry in specs/0038/.contracts-json;
  it is not re-declared in spec 0050.
- Before the first substantive step: replace the five seed-fed facts with the
  eight specified catalogue facts, retaining the reference-equality fact.
- Replaced both test files with the nine required facts. Expectations decode
  the shipped rows, the recording tape includes the single refused attempt,
  and the malformed fixture contains a category, category-less material and
  sample. The real-store cases cover counts, every id, duplicates and read errors.
- Before the second substantive step: implement the two import members and three
  error descriptions, preserving the four fields and createInMemory body.
- Implemented ordered, lazy-per-entry decoding through the existing decoders and
  save fields. importFrom reads once and maps its error before importing. Samples
  go only to saveSample; neither retry nor rollback was introduced.
- Shared the two test preparation helpers in CatalogueImportTestData within the
  existing first test file; the second file calls them rather than copying them.
- Marked the inherited STORE_XDUO_0008 lifecycle implemented in its existing
  specs/0038 registry entry. No identity was minted or current-arc declaration added.
- Before handoff: inspect the source diff, run local constructor verification as
  required by the coding-session instructions, and write the final state. Formal
  acceptance gates remain owned by the supervisor.
- Local compilation found FS0691 in a test assertion: F# interpreted the bare
  equality as a named argument. Parenthesized the equality arguments; no test
  expectation or production behavior changed. Re-running local verification.
- Local verification completed successfully: all 797 constructor tests passed,
  none failed or skipped. The TRX includes all nine required import facts.
- Reviewed the diff against S16-IMPORT. The old seeding names have no matches
  under Berreman/. Ordinary and ignore-CR numstats agree, the three source files
  and lifecycle file contain zero CR bytes, and git diff --check reports no issue
  in this change. The supervisor's pre-existing manifest edit remains untouched.
- Wrote 0040-state-of-the-world.md with completed scope, local verification,
  pending supervisor gates and the unchanged historical checkpoint count.

## Files modified

- Created this log and 0040-impl-plan.md at the prescribed paths.
- CatalogueImportProxyTests.fs and CatalogueImportInMemoryTests.fs.
- CatalogueImport.fs.
- specs/0038/.contracts-json (the inherited contract's lifecycle only).
- 0040-state-of-the-world.md at the prescribed path.

## Testing state

Implementation complete. Local `dotnet test -c Release --no-ansi` built the
constructor test project and its references and passed all 797 tests, with zero
failures or skips. TRX output is in the per-arc artifacts folder. The two edited
classes now have nine facts instead of six: five seed-fed facts were removed,
eight catalogue-fed facts added, and the reference-equality fact retained.

The first compile failed with FS0691 in the new assertion helper; the corrected
run passed. That first build also emitted two existing SYSLIB0051 warnings from
vendored MathNetNumerics/Numerics/Exceptions.cs. No warning originated in our
changed code, and the successful incremental run emitted none.

No formal gate result is claimed. The supervisor owns the resolved solution
build, elevation, constructor-unit-tests and six s06 invariant gates. The final
gates_run and gate_results collections are empty; commit_ready means the entire
implementation is ready for supervisor verification, not that its gates passed.

## Artifacts

- `.artifacts/0040-local-tests.log`: initial compile diagnostic.
- `.artifacts/0040-local-tests-retry.log`: successful local build/test run.
- `.artifacts/0040-local-tests/constructor-tests.trx`: all 797 passing results.

## Gotchas

- CatalogueManifest orders categories last by kind tag; the import must explicitly
  put them first so material references resolve.
- Decoding constructs equal values, so the new assertions compare values rather
  than seed object identity.
- The origin registry's implementStep is left null: assigning 40 there would
  falsely identify spec 0038 step 40. The current implementation is spec 0050
  step 40, recorded here; its inherited identity is not re-declared.
- The role's act-only instruction conflicts with the coding-session requirement
  to run appropriate tests. Local build/test verification follows that session
  requirement; the supervisor's gate engine and gate reporting are not invoked.

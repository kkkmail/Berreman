# Step 0458 — impl plan

Tests only: `OpticalConstructor.Tests/MaterialSourcePullSqliteTests.fs` gains the three facts the step names, proving
the PTB OCDB and CXRO Henke pulls end to end over their real adapters (`MaterialSourceProxy.createPtbOcdb`,
`createHenkeCompounds`) and the real library database in SQLite `:memory:` (`BuiltinCatalogue.freshLibraryDatabase`).
No product file changes: step 444's `pullRecord` / `updateSourceSnapshot` already serve every adapter.

## Approach

- Split the fixture composition so all three adapters share it: `fixtureWith database adapter` composes the import
  context (fixed `newEntryId`) and the material store; `fixtureOver` (refractiveindex.info) and a new
  `ptbFixtureOver` / `henkeFixtureOver` build their adapter over `freshSources owner` and call it.
- Generalise `resultAt` to take the `MaterialSourceHit` it reports, and `pulledFixture` into `pulledIn hit fixture`, so
  the PTB update fact reuses the pull-then-assert step; the existing facts keep every assertion.
- Factor the one-segment table match out of `nAtLongestWaveLength` as `tabulatedTableOf`, reused for the PTB σ and the
  Henke table comparisons.
- PTB facts: expected tables come from `PtbOcdbText(...).importMaterial` of the fixture text; σn of the first sample is
  asserted as `(0.96810 − 0.96772) / 6.0` (v2) and `(0.96804 − 0.96772) / 6.0` (v1) — the reader's exact arithmetic,
  so exact equality holds; PulledSources rows and attribution line checked.
- Henke fact: search `SiO2 2.2`, pull, `tryLatestEnvelope` provenance (hash over the record text and the snapshot's
  `si.nff`, `o.nff` bytes in formula order, as the adapter hashes), table equals `CompoundSpec.indexTableWith` of the
  fixture factors, attribution without citation; a 404-serving downloader records that nothing was downloaded.

## Risks

- Database round trip of uncertainty arrays must be exact (step 453 says the canonical payload keeps them).
- `refreshedWith` stamps 3 files: true for both the refractiveindex.info and the PTB archives.

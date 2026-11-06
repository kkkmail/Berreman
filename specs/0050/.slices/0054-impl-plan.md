# Step 0054 implementation plan

Re-point the six authorized UI test files to the existing BuiltinCatalogue
fixture, following spec 0050 S1.7 and 001 §B. Replace material/category lists,
known-id material/sample lookups, library construction and self-seeding store
construction with the matching fixture members and identical arguments.
Keep the recording stubs and synthetic editor inputs; only their catalogue
reads change. Add the fixture namespace where needed and remove unused opens.

Files: MaterialEditorWindowTests.fs, MaterialsWindowTests.fs,
EmbeddedChartTests.fs, NkDispersionChartTests.fs, CategoryEditorWindowTests.fs
and SampleEditorWindowTests.fs in OpticalConstructor.Ui.Tests. No product,
fixture, project or fact additions are authorized. Preserve every fact name,
assertion, count and numerical input. Update catalogue-related comments to
name the fixture; retain comments about editor initialization and chart styles.

Review the bounded source diff for lookup identity, ordering assumptions,
imports and LF line endings. The existing linked fixture supplies all needed
members. Catalogue row order can differ from code order, so retain product
ordering assertions and use known ids for catalogue entry selection.

Invariant 6 and implement_worker.system-md reserve all build, test, analyzer,
acceptance-grep and invariant execution for the supervisor. Record pending
verification honestly in the log and state of the world; do not author new
test baselines or change supervisor-owned files.

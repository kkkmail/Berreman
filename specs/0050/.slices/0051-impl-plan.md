# Step 0051 implementation plan

Re-point the ten material/category test files named in the slice to the existing
BuiltinCatalogue fixture. Use its material and category lists, explicit known-id
lookups, and fresh materialStore/categoryStore doubles with the original lookup
functions and VersionsInUse arguments. Construct MaterialLibrary records from
the fixture's materials wherever resolution previously used the code library.

Preserve every fact name, assertion, count and numeric expectation. Catalogue
row order can differ from code-seed order: select Silicon and EUV molybdenum by
their well-known MaterialIds and compare collections independently of order only
where the product imposes a different order. Update seed-related comments and
remove imports made unnecessary by the re-pointing. Do not change product code,
fixture members, project files, or the supervisor-owned manifest state.

The source scope is CategoryEditorTests.fs, CategoryProxyTests.fs,
MaterialProxyTests.fs, MaterialComplexityTests.fs, MaterialPreviewTests.fs,
DispersionModelsTests.fs, StackEditTests.fs, OutOfBandDiagnosticTests.fs,
ExportImportTests.fs and CanonicalJsonTests.fs under OpticalConstructor.Tests.
Write the implementation log and state of the world at the supplied paths.
Per Invariant 6 and implement_worker.system-md, leave build, tests, analyzers,
acceptance grep and the seven s07 invariant gates to the supervisor; report no
gate outcomes from this worker.

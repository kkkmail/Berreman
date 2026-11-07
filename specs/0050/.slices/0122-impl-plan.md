# Approach

Implement spec 0050 S4.1's kit identity and file as pure Domain data. Reuse `LibraryEntryFiles.KitId`, `FileSchemaVersion`, and `FileStem` plus the existing Softellect path types. Keep the supplied identity and instant unchanged, preserve valid display-name text, reject blank names with a diagnostic error, and derive the singleton file name through the shared stem rule.

Add exactly the four authorized `KitTests` facts before the implementation. Cover the name rule, all initial kit fields, the extension-only file name, and all five error descriptions. Register `Kit.fs` directly after `EntryTextDiff.fs` and append `KitTests.fs` to the test compile list.

# Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- The canonical implementation log and state-of-the-world paths supplied in the task.

# Risks and verification ownership

Respect F# compile order, elevated public types, unique names, and LF text. Do not add clock composition, IO, schema conversion, access flags, or extra API. The IMPLEMENT worker protocol assigns all checks to the supervisor; build, elevation, constructor-unit-tests and the eleven s15 invariants remain pending there. Do not invent baseline counts or change the immutable oracle, gate snapshot, test census, or supervisor state.

# Implementation plan — 0157

Remove the project-only `DesignHistory` and `Report` modules and their documentation from `OpticalConstructor.Storage/Report.fs`, then remove imports used only by those modules. Preserve the complete `MaterialLibrary` module, its current schema behavior, and Export's chart-image surface.

Remove Storage's `Giraffe.ViewEngine.StrongName` package reference and its explanatory comment. Shorten the existing persistence compile-order comment to its surviving subjects. Leave the central package version and all compile entries intact.

Delete only the authorized AC-I11 fact in `OpticalConstructor.Tests/ExportImportTests.fs`, its five exclusive fixtures, and their unused imports. Remove the design-history clause from the module documentation; preserve every other fact's identity and assertions. The expected constructor suite count decreases by one.

The main risks are removing a shared import or changing the material-library code accidentally. Source inspection identifies all retired references within the three authorized files. No replacement API, durable format, migration, new test identity, or cross-repository change is needed.

Under the IMPLEMENT worker's Invariant 6, the supervisor executes build, elevation, constructor-unit-tests and the twelve s18 invariants after this worker exits. Record no local gate result or invented baseline. Write the implementation log and state-of-the-world at the supplied paths, carrying the latest available measured constructor count forward as historical context.

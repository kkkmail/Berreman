# Step 0041 implementation plan

Add the shared `OpticalConstructor.Tests.BuiltinCatalogue` fixture over the shipped catalogue files. Reuse `CatalogueProxy.createBuiltIn`, the entry decoders, and `CatalogueImportProxy.createInMemory`; keep only one lazy catalogue load per process and create independent imported stores on every request. Expose the specified ordered lists and typed-id lookups with diagnostic failures and one-line API documentation.

Add `BuiltinCatalogueFixtureTests` with the two specified facts covering complete store population, per-call sample isolation, known identities, and every entry's lookup round trip. Compile the fixture first in `OpticalConstructor.Tests.fsproj`, link the same file first in `OpticalConstructor.Ui.Tests.fsproj`, and place the new tests immediately after `CatalogueImportInMemoryTests.fs`.

Only those four source/project files and this slice's three worker reports will change. Preserve LF, existing project comments, catalogue data, and the supervisor's pre-existing manifest-state modification. Main risks are F# compile order, matching the existing decoder/import API, preserving manifest row order across preset kinds, and ensuring lazy failures occur on first use rather than test discovery.

The coding session requires local validation of changed code. Build the solution and run the relevant constructor and UI tests locally, capturing output under the supplied artifacts folder. Formal execution and acceptance of build, elevation, constructor-unit-tests, ui-smoke, selected ui-tests, and all six s06 invariants remain with the arc-runner. Do not author gate baselines or claim formal gate results.

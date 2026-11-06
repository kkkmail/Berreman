# Step 0081 implementation plan

Add `DataLocations` after `WorkbenchSettings` in the existing Domain module, reusing `Softellect.Sys.Primitives.ConnectionString` and `FolderName`. Keep the seven configured defaults in that type, compose the Documents children with `FolderName.combine`, and leave environment expansion and filesystem access to the composition root. Existing workbench settings stay unchanged.

Add `CatalogueProxy.createBuiltInUnder` in `CatalogueFolderStore.fs` and route `createBuiltIn` through it using the shared relative shipped-catalogue default. Reuse the current folder proxy and its typed IO errors; introduce no cache, fallback, configuration reader or storage format.

Add the two exactly named facts to `WorkbenchSettingsTests.fs` and `CatalogueFolderTests.fs`. Check all configured texts, the relative shipped default, the real shipped catalogue, and a fresh empty folder under the test output. The existing fresh-folder helper supplies the test folder.

Local development verification will build the affected constructor test project and run its full suite, as required by the higher-priority development instructions. These observations do not execute or certify the supervisor's acceptance gate roster. Preserve the existing manifest-state modification and the version lock. Persistent logs and reports go only under `specs/0050/.artifacts/`.

Risks are folder composition direction, accidental environment expansion, and unintended access to user data. The tests use literal `D:/Docs` only as text and perform IO only beneath the test assembly output. No migration is required because no existing durable shape changes.

# Step 0449 — impl plan

**Scope.** The app scope offers refractiveindex.info over its configured sources folder (obligation
S131-APP-SOURCES). Two files, exactly the authorized effects:

- `OpticalConstructor.Ui/AppContext.fs`: the private `compose` that `create` and `recreateLibrary` share builds
  `catalogueImports` with, beside `exportFolderOpt` in the same record update,
  `materialSources = [ MaterialSourceProxy.createRefractiveIndexInfo locations.materialSourcesFolder
  SourceArchiveDownloader.http ClockProvider.system ]`. The `catalogueImports` field doc names the source.
- `OpticalConstructor.Ui.Tests/AppContextTests.fs`: one new fact, `the app scope offers refractiveindex.info over its
  configured sources folder, holding no snapshot`.

**Opens.** `createRefractiveIndexInfo` and `SourceArchiveDownloader` live in `OpticalConstructor.Storage.MaterialSourceStore`
(an extension of `MaterialSourceProxy`, so the module must be opened); `MaterialSourceProxy` in
`OpticalConstructor.Domain.MaterialSources`; `ClockProvider` in `OpticalConstructor.Domain.Kit`, its `system` member an
extension in `OpticalConstructor.Ui.KitOpening`, which compiles before `AppContext.fs`.

**Every other composition keeps `[]`.** `CatalogueImportContext.create` (Storage) still answers `materialSources = []`;
`DefaultStores` and `initWith` in `TableAndElementRotationView.fs` call it and are untouched.

**The fact.** `TestAppScope.freshAppContext (FolderName "AppContextTests")` (the module's `owner`):
`Assert.Single` on `catalogueImports.materialSources`; its `sourceName` is `MaterialSourceName "refractiveindex.info"`;
its `snapshotVersion ()` is `Ok None`; nothing exists under `(TestAppScope.testLocations owner).materialSourcesFolder`.
No `updateSnapshot` is run, so no network.

**Risks.** An opened module shadowing a name `AppContext.fs` already uses (checked at build); the sources folder
existing from another fact of the same owner (no fact in the assembly writes there).

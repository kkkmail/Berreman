# Step 0459 — impl plan

## Approach

The app scope's single composition, `AppContext.compose`, is shared by `create` and `recreateLibrary` (step 449). It
already sets `materialSources` to refractiveindex.info over `locations.materialSourcesFolder`, through the HTTP seam
and the system clock. This step appends PTB OCDB (`MaterialSourceProxy.createPtbOcdb`, step 455) and CXRO Henke
(`MaterialSourceProxy.createHenkeCompounds`, step 457), in that order, over the same folder, downloader and clock.
Building each adapter touches nothing: each one's `snapshotVersion` only reads its stamp file when it exists. So
composing the app scope still writes nothing under the sources folder.

`CatalogueImportContext.create` keeps `materialSources = []`, so every other composition offers no source. The
Online sources window opens on the first source (`initMaterialSources`, step 450), which is refractiveindex.info as
before. Its choice row lists all three sources. The window's code does not change.

`AppContextTests`' fact `the app scope offers refractiveindex.info over its configured sources folder, holding no
snapshot` keeps its name and widens its assertions:

- the sources are named `refractiveindex.info`, `PTB OCDB` and `CXRO Henke`, in that order;
- each `snapshotVersion ()` answers `Ok None`;
- nothing exists under the sources folder.

No fact runs `updateSnapshot`, so no test reaches the network.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs`: the `materialSources` list and the
  `catalogueImports` field doc, citing S13.2.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs`: the one fact's widened assertions.

## Risks

- **Other facts over `freshAppContext`.** They see three sources now. Only `MaterialsWindowView` consumes the list,
  and only to decide whether to show `Online sources…`, which stays visible since the list is still non-empty. The
  facts of `MaterialsWindowTests` / `ScrollExtentTests` / `MaterialSourcesWindowTests` compose their own sources, so
  they are unaffected.
- **Elevation.** A list of the three sources adds no primitive and no new name.

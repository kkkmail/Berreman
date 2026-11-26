# Where we are

Step 0449 wires S13.1's refractiveindex.info source into the app scope (obligation S131-APP-SOURCES). Earlier steps
built what this step composes:

- step 443 declared the `MaterialSourceProxy` seam;
- step 444 gave `CatalogueImportContext` its `materialSources` list, `[]` by default;
- step 446 added the configured `materialSourcesFolder`;
- step 447 added the real adapter, `MaterialSourceProxy.createRefractiveIndexInfo`;
- step 448 proved the pulls over that adapter and SQLite.

Now `AppContext.create` and `recreateLibrary` offer that one source over the configured folder through the HTTP
seam. Every other composition still offers none.

# What's working

- `AppContext`'s shared `compose` offers refractiveindex.info as the one material source, over
  `locations.materialSourcesFolder`, through `SourceArchiveDownloader.http` and `ClockProvider.system`.
- Composing the app scope reads and writes nothing. No folder is created and nothing is downloaded until Update
  snapshot runs.
- `DefaultStores`, `initWith` and every test import context keep the `[]` of `CatalogueImportContext.create`.
- Added the `AppContextTests` fact `the app scope offers refractiveindex.info over its configured sources folder,
  holding no snapshot`.

# Tests

These are development checks; the gate engine runs the step's roster after this session and is the authority.

- **UI test project build:** exit 0 in 79 s, against a 600 s deadline. There are 0 errors, and no warning comes
  from our code. The warnings that remain are SYSLIB0051 in the vendored MathNet, which predates this step, and the
  exempt NU1902.
- **`OpticalConstructor.Ui.Tests.AppContextTests`, filtered:** `Passed!`, 19 of 19, in 43 s against a 360 s
  deadline.
  - The class had 18 facts at HEAD, so the count rises by 1.
  - The new fact is listed by `--list-tests`.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing in the working tree.
- **Not run here:** elevation, the whole-solution build, ui-smoke and the whole constructor-unit-tests suite. Under
  Invariant 6 those belong to the gate engine.
- **Unchanged counts:**
  - constructor-unit-tests stays at step 448's 1795, because no constructor-test or Domain/Storage file changed. This
    is carried forward, not measured here.
  - The ui-smoke membership is unchanged, because the new fact has no `ui-smoke` trait.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1795
  ui_tests: 19
```

# Architecture

- **Only the app scope offers the source.** The source is composed once, in `AppContext.compose`, beside
  `exportFolderOpt` in the same record update of `CatalogueImportContext.create`.
- **Its three seams are each built at their own composition boundary:**
  - the folder comes from `DataLocations`;
  - the HTTP seam is `SourceArchiveDownloader.http`, the one wire carrier;
  - the clock is `ClockProvider.system` from `KitOpening.fs`.
- **No test reaches the network.** No test runs the source's `updateSnapshot`, and no UI path calls it yet.
- No stored format, schema, package, gate or project file changed. The R0001–R0005 behaviour is untouched.

# Deferred

Nothing.

# Gotchas

- **Two names come from extension members:**
  - `ClockProvider.system` is an extension in `OpticalConstructor.Ui.KitOpening`;
  - `createRefractiveIndexInfo` is an extension in `OpticalConstructor.Storage.MaterialSourceStore`.

  Both modules must be opened where the source is composed.
- **The fact checks `Directory.Exists` of `TestAppScope/AppContextTests/Sources`.** It asserts the folder does not
  exist, which proves that composing creates nothing. A later fact that runs an update under the same owner would
  have to use its own folder.

# Changelog

- 2026-11-16, step 0449: the app scope offers refractiveindex.info over its configured sources folder (HTTP seam,
  system clock), and every other composition offers none. Added one `AppContextTests` fact; the class count goes from
  18 to 19.

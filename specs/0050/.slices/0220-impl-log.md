# Step 0220 — impl log (WIRE: `AppContext.itemModels`)

## Progress

- [x] Read the protocol (the arc-runner and wire_worker system prompts), the project prompt, AGENTS.md, the slice and
  its `.gates`, manuals 013 (elevation from the arc origin) and 015 (the ui-smoke deadline), and step 0219's impl-log
  and SoW. No operator note is in flight.
- [x] `AppContext.fs`:
  - `AppContext` gains `itemModels : ItemModelProxy`, after `categories`, with the one-line doc "The item-model store
    over the app's library database (spec 0050 S7.1)."
  - `compose` sets it to `ItemModelProxy.createInDatabase database` (step 211), beside the other database stores.
    `create` and `recreateLibrary` both go through `compose`, so both carry it.
  - The file opens `OpticalConstructor.Domain.ItemModels` (the type) and `OpticalConstructor.Storage.ItemModelDatabaseStore`
    (the extension member).
  - No other field changes. No window reads the field: a grep of Ui and App outside `AppContext.fs` finds no
    `itemModels`, `ItemModelProxy` or `ItemModelDatabaseStore`.
  - `compose` is the only `AppContext` record literal. Every other construction copies one (`{ … with kitHistory = … }`)
    and needs no change.
- [x] `AppContextTests.fs`: the declared fact `the app scope serves the Built-in item models from its library database`,
  in the pure-contract (`ui-tests`) block, untraited, after `create imports the Built-in catalogue into empty stores`.
  The file opens `OpticalConstructor.Domain.ItemModels`.
  - It composes `TestAppScope.freshAppContext owner` (owner is the class's `FolderName "AppContextTests"`), which
    settles the start-up import of the shipped catalogue (steps 95, 100).
  - `itemModels.listItemModels ()` equals `Ok (BuiltinCatalogue.itemModels () |> List.sortBy name)`.
  - Beyond the brief's minimum:
    - the shipped list is checked to contain the empty-slot model, so an empty catalogue cannot pass vacuously;
    - every Built-in item model resolves at its exact version through `resolveItemModelVersion`.
    Both are the assertions `ItemModelProxySqliteTests` makes for the bare store, now made through the app scope.
- [x] Development runs: build, class tests, one mutation, ui-smoke, elevation and the fourteen invariants (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs` (+5 / −0)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs` (+11 / −0)

Both are the slice's authorized `edits`. Both are LF only, with no BOM: 0 CR bytes each, and `git diff --numstat`
equals `--ignore-cr-at-eol`. No `.fsproj` changed.

`specs/0050/.manifest.state.json` was already modified before this round. It is the supervisor's file, not this
step's, and was not touched.

## Testing state

These are development runs only. Under Invariant 6, the gate verdict belongs to the arc-runner's gate engine.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`):
  - First build: exit 0, 0 errors (`0220-dev-build-01.log`). Its two warnings are the existing vendored-MathNet
    `SYSLIB0051` warnings, which appear whenever `MathNetNumerics/Numerics` recompiles. Steps 0195 and 0219 recorded
    them too.
  - Final build, over the restored source after the mutation run: 0 warnings, 0 errors (`0220-dev-build-02.log`).
- **ui-tests** (the gate's command, selection `OpticalConstructor.Ui.Tests.AppContextTests`, final binaries):
  - 16 passed, 0 failed, 0 skipped (`0220-dev-ui-tests-02.log`).
  - Before this step, the class had 15 untraited facts and one `ui-smoke` fact, counted in the pre-step source
    (`0220-dev/AppContextTests.fs.orig`). So the count is 15 → 16, up by 1.
  - The TRX (`0220-dev-ui-tests.trx`) lists
    `OpticalConstructor.Ui.Tests.AppContextTests.the app scope serves the Built-in item models from its library database`
    as `Passed`.
- **The fact bites.** `itemModels = ItemModelProxy.createInDatabase database` was replaced with
  `ItemModelProxy.createInMemory ()`, and the class was rebuilt and run:
  - exactly the new fact failed, and the other 15 passed (`0220-mutations/mutant-in-memory-store.log`);
  - the source was then restored byte for byte (`cmp` against `0220-mutations/AppContext.fs.final`) and the solution
    rebuilt (build 02).
- **ui-smoke** (the gate's command, whole suite, final binaries):
  - 237 passed, 0 failed, 0 skipped, in 191 s against the 600 s `attempt_timeout_seconds` (manual 015);
  - that equals checkpoint 209's 237, so the count is unchanged;
  - logs: `0220-dev-ui-smoke.log`, `0220-dev-ui-smoke.trx`.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597c…` from `.arc-base-json`, `-ExcludeRules ELEV004`, final source):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 199 s against a 600 s deadline;
  - the whole scope ran because `Berreman.slnx` changed since the arc origin;
  - Ui's suppressed count is 479, App's 14, and the total 1309. All three equal step 0219's run, so the new field
    adds no finding, ELEV004 included;
  - log: `0220-dev-elevation.log`.
- **The fourteen `invariant-s22-*`:** each decoded `git grep` finds nothing (exit 1). Log: `0220-dev-invariants.log`.
- `constructor-unit-tests` and `unit-tests` are not in this step's roster. No Domain, Storage or constructor test file
  changed.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0220-dev-build-01.log`, `0220-dev-build-02.log`
- `0220-dev-ui-tests.log` (first run), `0220-dev-ui-tests-02.log` (final binaries), `0220-dev-ui-tests.trx`
- `0220-dev-ui-smoke.log`, `0220-dev-ui-smoke.trx`
- `0220-dev-elevation.log`
- `0220-dev-invariants.log`
- `0220-dev/AppContext.fs.orig`, `0220-dev/AppContextTests.fs.orig` (the pre-step sources)
- `0220-mutations/`, which holds `mutant-build.log`, `mutant-in-memory-store.log` and `AppContext.fs.final` (the source
  the mutation restored)

## Gotchas

- **Ordering.** The database store sorts by `(name, id)`, and the brief says "ordered by name". The Built-in item
  model names are unique, so `List.sortBy name` gives the same list. `ItemModelProxySqliteTests` already relies on
  this for the bare store. If two shipped models ever share a name, sort the expectation by `(name, id)`.
- **The fact needs no `ui-smoke` trait.** It opens no window. The `ui-tests` selection is the class, and ui-smoke's
  count is unchanged by design.
- **The YAML carries the latest recorded checkpoint floors**, as steps 0195 and 0219 did: constructor 1312
  (checkpoint 218) and ui-smoke 237 (checkpoint 209). `ui_tests` has no recorded checkpoint (steps 0185, 0195). The
  measured 16 over this step's selection is reported in the SoW's Tests section, not as a floor, because a later step
  selects other classes.
- **The WIRE delta says the worker runs no checks.** Following the arc's established practice (steps 0195, 0219),
  development runs were taken to catch defects early, and they are recorded here as development evidence only. The
  gate engine's runs are the verdict.

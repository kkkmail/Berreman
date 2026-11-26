# Step 0459 — state of the world

## Where we are

Spec 0050, section S13.2 (PTB OCDB, the Henke compound calculator and glass catalogues). The pieces this step wires
were built earlier:

- step 449: the app scope's shared `compose`, offering refractiveindex.info;
- step 450: the Online sources window, which opens on the first source;
- step 455: the PTB OCDB adapter;
- step 457: the CXRO Henke adapter;
- step 458: proved those adapters' pulls end to end.

This step makes the app itself offer all three sources, so the Online sources window lists PTB OCDB and CXRO Henke
beside refractiveindex.info. The window's code does not change.

## What's working

- The app scope offers refractiveindex.info, PTB OCDB and CXRO Henke, in that order. All three are composed over the
  configured sources folder with the HTTP downloader and the system clock.
- `create` and `recreateLibrary` share that one composition. Every other composition still offers no source.
- The Online sources window still opens on refractiveindex.info, and its choice row now lists all three sources.
- Composing the app scope still reads and writes nothing under the sources folder.
- Widened the AppContextTests app-scope fact, name kept: it now checks the three names in order, `Ok None` for every
  snapshot, and that the sources folder is still absent.

## Tests

These are development checks; the gate engine runs the roster after this session and is the authority. Every figure
is from the final source.

| Check | Result | Time / deadline |
|---|---|---|
| build | exit 0, 0 errors; no FS or MSB warning (12 × exempt NU1902; 4 × SYSLIB0051 from vendored MathNet, pre-existing) | 83 s / 600 s |
| ui-tests, gate command over `AppContextTests` | `Passed!`, 18 of 18 | 38 s / 360 s |
| ui-smoke, as the gate selects it (`all_discovered`) | `Passed!`, 267 of 267 | 216 s / 600 s |
| constructor-unit-tests, as the gate selects it (`all_discovered`) | `Passed!`, 1823 of 1823 | 105 s / 180 s |
| elevation, all 13 projects (standalone) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 233 s / 600 s |
| the 21 s44 invariants | each decoded `git grep` finds nothing, as written and with `--untracked` | — |
| LF | numstat equals the CR-ignoring numstat | — |

- **ui-tests:** the selected class has 19 facts. One carries `Category=ui-smoke`, which the gate excludes, so the
  gate counts 18. The step neither adds nor removes a fact, so the count is unchanged.
- **Other counts:** constructor-unit-tests (1823) and ui-smoke (267) are unchanged from the prior baselines.
- **Deadlines:** no development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1823
  ui_smoke_tests: 267
  ui_tests: 18
```

## Architecture

- No architectural change. The composition root picks the three adapters and their order, and the window renders
  whatever list it is given.
- The order is a product decision, recorded in a one-line S13.2 comment: the first source is the one the window opens
  on.

## Deferred

Nothing. The composition and the widened fact both landed as the slice describes.

## Gotchas

- **Two counts for one class.** The ui-tests gate excludes `Category=ui-smoke`, so `AppContextTests` counts 18 under
  the gate, not the 19 facts the class holds. Step 0449's "19 of 19" was a run without the trait filters.
- **Every `freshAppContext` scope offers three sources now.** Of the UI code, only `MaterialsWindowView` reads the
  list, and only to show `Online sources…` when it is non-empty, so nothing it renders changes. Window facts that
  need specific sources compose their own.
- **Absence of the folder is the "nothing written" check.** Each adapter's `snapshotVersion` only probes its stamp
  with `File.Exists`, and no fact runs `updateSnapshot`, so no test reaches the network.
- **SYSLIB0051 is pre-existing.** It comes from vendored MathNet source and appears whenever MathNet recompiles.

## Changelog

- 2026-11-17 — step 0459: the app scope offers refractiveindex.info, PTB OCDB and CXRO Henke over its sources folder;
  widened the AppContextTests app-scope fact to the three names, `Ok None` snapshots and an absent sources folder.

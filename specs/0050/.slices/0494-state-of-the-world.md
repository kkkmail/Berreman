# Step 0494 — state of the world

## Where we are

Spec 0050's Close section, step 494 of 494, the last step of the arc. The composition root needed no new wiring: its
two files were already composed by steps 83–492. This step rewrote their stale docs and updated AGENTS.md wherever spec
0050 changed a fact it states. It also proves the whole composition end to end headless: from the Launcher through the
start screen to a kit under the test output in each mode, and each committed kit opened read-only from the start screen
without a byte written under `data/`.

## What's working

- Rewrote the `Program.fs` file doc, `Startup` docs and `AppContext` type doc to describe the closed root (spec 0050 Close)
- Updated AGENTS.md: test locations, project layout (Database, Catalogue/Builtin, BerremanTestSupport, data/),
  library-is-data, kit-folder and start-screen gotchas, and the flow strip on Main ribbon
- Added acceptance: the composed app goes from the Launcher through the start screen to a kit under the test output and
  shows every flow-strip step of each mode, with Data locations reading the library file and kits folder
- Added acceptance: the committed optics-mueller and absorbing-biaxial-noisy kits open read-only from the start screen,
  show every flow-strip step and write nothing

## Tests

Local runs with each gate's own command over the final tree; the supervisor's gate engine is the authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902` (plus the vendored MathNet's pre-existing `SYSLIB0051`) | 89 s | 600 s |
| ui-smoke (whole) | `Passed!`, 274 of 274 = 271 + the 3 new facts | 238 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 241 s | 600 s |
| 21 × `invariant-s47-*` | each decoded grep finds nothing | — | 300 s |

- The class `WireUiFinalCompositionTests` passes 6 of 6 in 22 s (its three earlier facts unchanged).
- Both 001 §R removal greps (`ocproj`, and `SceneProxy` … `measuredElementOf`) find nothing under `Berreman/`.
- Not run locally, left to the gate engine: constructor-unit-tests, ui-tests, unit-tests, kit-acceptance and
  elevation-full. No code they exercise changed: the product edits are comments, and the test edits are one `ui-smoke`
  class. No run exceeded its effective deadline.
- `data/` is unchanged (`git status`); the edited files are LF only.

```yaml
gates:
  ui_smoke_tests: 274
```

## Architecture

- No product code changed. `Program.fs` and `AppContext.create` remain the single composition root: the SQLite library over
  `DataLocations`, the catalogue import of every start, the start screen behind Main and Inverse, the kit window with its
  mode's flow-strip steps, and the Data locations window behind the Launcher's footer.
- The new facts drive only by id: a launcher button by `Click`, every box by a mouse press at its centre. Each window a
  press opens is observed through `Window.WindowOpenedEvent`. Waits use `Dispatcher.UIThread.RunJobs` alone, and every
  opened window is closed in a `finally`.

## Deferred

Nothing. Every requirement of the slice is in this round.

## Gotchas

- A new library's start-up import only awaits confirmation (spec 0050 §3.7), so silicon is not in the database straight
  after `AppContext.create`. Fact (a) shows the launcher over the unchanged `Ok ctx` and presses Apply in the import
  dialog the launcher opens on its first show, then reads silicon from the database. This is the app's real first-start
  path.
- `KitAcceptanceTests.closeCommittedReadOnly` is private, and that file is outside this step's edits. The read-only facts
  therefore write the same two-line close (close the kit, then `assertCommittedUnchanged` in a `finally`).
- `TableAndElementRotationView` is reached through the alias `Scene` rather than an `open`, so it cannot shadow the
  names the earlier facts use.
- `grep -r` over `Berreman/OpticalConstructor` walks `bin/` and `obj/` and takes minutes. Use `git grep` instead.

## Changelog

- 2026-11-18 — Step 0494: closed the composition root's docs, updated AGENTS.md (library database, kit folders, start
  screen, flow strip, test locations, layout), and added the three Close acceptance facts to `WireUiFinalCompositionTests`.

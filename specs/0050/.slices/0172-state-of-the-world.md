# Where we are

Step 0172 is an `IMPLEMENT` step in spec 0050 S5.2, the kit start screen and the Launcher. Step 150 built
`KitOpening.openKitAt`, which opens a picked kit and initialises one if the folder is not a kit yet. This step adds
the open-only path that Open kit… needs. That path opens a folder that is already a kit and never initialises one,
so the start screen can offer "Make this a kit" for a folder without `.kit-json` (001 §F).

Verification belongs to the arc-runner after this act-only worker exits.

# What's working

- Moved `openKitAt`'s post-open body (read experiments and scene names, resolve snapshots, open the overlay, close
  the kit on failure) unchanged into the private `KitOpening.openedOver`. `openKitAt` binds into it, so every answer
  it gives is unchanged.
- Added `KitOpening.openExistingKitAt folder clock imports`. It opens the kit read-write through `KitContext.openKit`,
  never initialises one, and answers:
  - `NotAKitFolderErr` for a folder that is not a kit;
  - `KitLockedErr` for a kit open elsewhere.

  It writes nothing in either case.
- Added two `KitOpeningTests` facts:
  - an existing kit opens read-write with its kit file, and a folder holding only `notes.txt` or a missing folder is
    refused untouched;
  - a kit held open elsewhere is `KitLockedErr` naming this process, and the folder keeps its bytes.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `OpticalConstructor.Ui.Tests.AppContextTests`;
- the fourteen s20 section invariants.

The new facts are named exactly as the slice gives them:
- `KitOpeningTests.opening an existing kit opens it read-write with its kit file and never initialises a folder`;
- `KitOpeningTests.opening an existing kit open elsewhere is KitLockedErr and writes nothing`.

The constructor count should rise by 2 over step 0171's. Nothing was measured here. The YAML repeats the latest
recorded checkpoints (constructor 1,171 at step 170; ui-smoke 224 at step 168), not a result of this round.

```yaml
gates:
  constructor_unit_tests: 1171
  ui_smoke_tests: 224
```

# Architecture

- **Two public functions over one private body.** Initialise-or-open (`openKitAt`) and open-only
  (`openExistingKitAt`) both bind their open into `openedOver`.
  - `openKitAt` gets no flag parameter (.spec-md §4: a reuse is a move or a call).
  - `openExistingKitAt` takes no author, because it never makes a kit.
- **Refusal comes from the existing open.** `openExistingKitAt` adds no checks of its own; both refusals come from
  `KitContext.openKit`:
  - `readKitFile` answers `NotAKitFolderErr` before any lock or stamp is taken;
  - `acquireLock` answers `KitLockedErr` with the live holder.

# Deferred

Nothing in this slice is deferred. Wiring `openExistingKitAt` into the start screen's Open kit… action, and offering
"Make this a kit", belong to later S5.2 steps.

# Gotchas

- **The close-failure log prefix changed.** The moved body's line now says `KitOpening.openedOver:` (it said
  `KitOpening.openKitAt:`), because `openKitAt` and `openExistingKitAt` share it. No answer changed, and no test reads
  the text.
- **The missing-folder case must stay unbuilt.** It must not create the folder, so it never calls `folderBytes`,
  which would throw on a missing directory.
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was already modified when this worker
  started, and it was not touched.

# Changelog

- 2026-11-08: Moved `openKitAt`'s post-open body into the private `openedOver`, and added the open-only
  `openExistingKitAt` and two `KitOpeningTests` facts. Verification is left to the arc-runner.

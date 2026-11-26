# Step 0306 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Steps 298 to 305 added the kit file's import profiles and their one read of a lab file. They also replaced the strict
  X,Y data seam with attach-with-profile.
- A profile lives in the kit file, not in the experiment. Before this step, a copy between kits (steps 224–291) kept
  each experiment's profile name but left the profile behind (001 §H "Between kits"; .spec-md §3.15).
- This step makes the copy bring the kit profiles its experiments' data is mapped by:
  - it reuses an equal one;
  - it renames a different one to the first free suffix, as items, setups and experiments do;
  - it saves the kit file before any setup or experiment is written.
- The window then takes the kit file the copy left.

## What's working

- Add `ImportProfileName.nextFreeAmong`, the shared `FileStem.firstFreeText` suffix sequence for profile names (S9.2).
- `copyExperimentsFrom` now brings the source kit's profiles that its experiments use. An equal held one is reused,
  and a differing one is added as `<name> (n)`, its experiments following it.
- The copy saves the kit file after the data files and before items, setups and experiments, and only when it adds
  something.
- `ExperimentCopyReport` gains `profiles` (as added) and `kitFile` (as the copy left it). `CopyChosenExperiments`
  shows the window over that kit file.
- Two new constructor facts (1518 → 1520), and the window copy fact now asserts the brought `Lab scan`.

## Tests

These were development runs of each gate's own command over the final build. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 21 s (incremental; first full build 56 s) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 205 s | 600 s |
| constructor-unit-tests | Passed!, 1520 of 1520, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 228 s | 600 s |
| ui-tests (KitWindowTests) | Passed!, 16 of 16, 0 failed, 0 skipped | 24 s | 360 s |
| invariant-s30-\* (16) | each exit 0, `INVARIANT HOLDS` (`section_invariants --find-nothing`) | under 1 s each | 300 s |

- **constructor-unit-tests: 1520**, the 1518 baseline plus 2. In the TRX, each named identity is `Passed` under its
  exact name:
  - New: `KitTests.a profile name takes the first free suffix among the taken ones`.
  - New: `ExperimentCopyTests.a copy brings the kit profiles its experiments use, reusing an equal one and renaming a
    different one`.
  - Changed expectation: `KitWindowModelTests.copying the chosen experiments brings them into the kit and the window,
    and closes the source`.
- **ui-tests: 16** over the selected class `KitWindowTests`. No Ui.Tests file changed, so its count is unchanged.
- **ui-smoke: 251**, unchanged.
- **Red first:** the test project failed to compile on the missing `nextFreeAmong` and `ExperimentCopyReport.profiles`
  / `.kitFile` alone.
- **Mutation check:** three rounds, each defect failing its own fact.
  - A: the window keeps the old kit file, and the copy always writes the kit file.
  - B: a clashing profile is not renamed.
  - C: the kit file is saved after the experiments; this fails the fact's order assertion.

  The files were restored and their SHA-256 verified before the final rebuild.

```yaml
gates:
  constructor_unit_tests: 1520
  ui_smoke_tests: 251
  ui_tests: 16
```

## Architecture

- **Profile placement is one more pure planning step.** `placeProfiles` runs over the renumbered plan, after every
  read and before `writeAll`, beside `placeSetups`, so the copy keeps its read-then-write order.
  - The kept experiments' profile names are taken distinct, in first-use order.
  - Only those `ChosenImportProfile.tryOfName source.kitFile.importProfiles` answers as `KitProfileChoice` are
    considered. A built-in or an unheld name needs nothing.
- **"Held" means the target's profiles plus the ones this copy added before.** An equal one is reused. One of that
  name with other content is renamed by `nextFreeAmong` over those names. Because of this, no two profiles in the kit
  file ever share a name, even when a renamed profile takes a name a later copied profile carries.
- **One kit-file write, at the right time.** `saveProfiles` calls `this.saveKitFile` (step 131) after the data files
  and before items, setups and experiments, so a written experiment never names a profile the kit lacks. With nothing
  added it writes nothing and answers `this.kitFile`. Its `KitError` is `KitLibraryKitErr`, as for every other write.
- **The window takes the copy's kit file.** It does not re-read `.kit-json`. `{ model.kit with kitFile =
  report.kitFile }` keeps the context's proxy, access and clock.
- No merge or replace of a held profile was added, as the slice says.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **The "held" reading is a recorded interpretation.** The slice's literal "the target holds" would let a renamed
  profile collide with a later copied profile of the renamed name. Counting the profiles added before as held prevents
  that, and it coincides with the slice's rule whenever no such collision exists.
- **The new tests reuse `KitTests.labScanProfile` as the slice's "test-local `Lab scan`".** It is exactly the profile
  described, it compiles before both test files, and `KitFileJsonTests` and `ImportProfileTests` already reuse it.
  "Reuse, never copy" rules out a fourth copy.
- **`ExperimentCopyReport` is a record.** Any full literal of it must name `profiles` and `kitFile`. The one in
  `ExperimentCopyTests` now carries `profiles = []` and `kitFile = target.context.kitFile`.
- **Two comparisons differ in case.** Profile names compare ordinally (`availableAmong`, `tryOfName`). File stems ignore
  case in `nextFreeAmong`, so `Lab scan` beside `LAB SCAN` is no clash, but a renamed name skips a stem held in any
  case.
- **`saveKitFile` stamps `modified` from the context's clock.** A test comparing `report.kitFile` must compare it with
  the kit file read back, not with the pre-copy one.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0306: a copy brings the kit import profiles its experiments are mapped by (S9.2).
  - `ImportProfileName.nextFreeAmong` names a clashing profile.
  - `copyExperimentsFrom` reuses an equal profile, renames a different one and saves the kit file before any setup or
    experiment. `ExperimentCopyReport` gains `profiles` and `kitFile`.
  - The window shows the copy's kit file.
  - constructor-unit-tests rises by two, to 1520.

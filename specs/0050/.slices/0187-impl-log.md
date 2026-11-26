# Step 0187 — impl log

## Progress

- [x] `KitCheckOrigin` (`KitCheckOnActivation` | `KitCheckOnRequest`) before the model
- [x] `kitChanges : FileName list` after `kitShell`; `initWith` sets `[]`
- [x] `CheckKitChanges of KitCheckOrigin`, `ReloadKitChanges` and `KeepMyKitChanges` messages, after `CloseKit`
- [x] `loadKit`'s content setting moved into one private function, `withKitContent`
- [x] the three arms, behind the scratch-kit guard `guardKitFolder`
- [x] the five declared `KitWindowModelTests` facts
- [x] static review: name collisions, invariant words, LF, `git diff --check`

Attempt 01 (2026-11-08):

1. Read:
   - the task, both system prompts, the project prompt and the slice;
   - the gate snapshot, manual 014 (named for step 0187) and step 0186's log and state of the world;
   - the code the slice names: `TableAndElementRotationView.fs`, `KitFolders.fs` (`KitProxy`, `readOnlyView`,
     `KitContext.openKit`), `KitFolderStore.fs` (`KitProxy.create`'s stamps), `KitContentFiles.fs`, `KitOpening.fs`
     and `KitLibrary.fs`.

   No operator note was in flight.
2. Wrote the plan and this log's skeleton.
3. `TableAndElementRotationView.fs`:
   - `KitCheckOrigin` sits between `ExperimentFileStatus` and the model, and each case has a one-line doc.
   - `kitChanges` sits after `kitShell`, and `initWith` sets `kitChanges = []`.
   - The three messages sit after `CloseKit`.
4. `withKitContent kit experiments scenes model` (private, after `validateExperimentAttachment`) takes over what `loadKit`
   set from the kit's content:
   - `kit`;
   - `experimentCollection` (through `ExperimentCollection.ofLoaded`);
   - `savedScenes`;
   - `experimentDataStatus`: the moved attachment recheck, which calls `validateExperimentAttachment`;
   - `kitStatus = None` and `kitChanges = []`.

   `loadKit` sets `kitFolderOpt` and `experimentData`, then pipes into it with `opened.library.kit`,
   `opened.experiments` and `opened.scenes`.
5. Added two private helpers after `guardKitWrite`:
   - `guardKitFolder`: a model with no kit folder answers unchanged;
   - `withKitCheckFailure`: `The kit could not be checked for changes — <describe>`, shared by the check and Keep mine.
6. The arms, after `CloseKit`'s:
   - `CheckKitChanges origin` → `changedKitFiles ()`:
     - `Ok []`: sets `kitChanges = []`, and `kitStatus` to `No kit file changed outside the app.` on `KitCheckOnRequest`
       only;
     - `Ok files`: sets `kitChanges = files` and nothing else;
     - `Error`: the check's status.
   - `ReloadKitChanges` → `readKitFile`, `readExperiments`, `readSceneNames`, then `recordStamps`, in one `Result.bind`
     chain:
     - `Ok`: `withKitContent { model.kit with kitFile = … } experiments scenes model`, so the proxy, access and clock
       are kept, the draft starts fresh, the offer is cleared and the status is `None`;
     - the first `Error`: keeps the model and the offer, and sets `The kit could not be reloaded — <describe>`.
   - `KeepMyKitChanges` → `recordStamps ()`:
     - `Ok ()`: `kitChanges = []`;
     - `Error`: keeps the offer and sets the check's status.
7. `KitWindowModelTests.fs`:
   - Opens `OpticalConstructor.Storage.KitFolderStore` for `KitProxy.create`.
   - `withLoadedScene` became generic in its outcome (`'Outcome`). Every existing caller still returns `unit`.
   - Added four private helpers:
     - `committedPolarizerExperiment`: `AddElement LinearPolarizer`, `ExpChooseElement`, `ExpCommit`, as the step-186
       switch fact does;
     - `writeE2`: the committed experiment with `id = ExperimentId 2`, through `writeExperiment`;
     - `renameKit`: `readKitFile`, then `writeKitFile` with a new `name`;
     - `withOtherWritersChanges`: a read-write kit, `E1` committed through the model, then the other writer's `E2`
       and `Renamed`.
   - Added the five declared facts.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` (+120 / −25):
  - `KitCheckOrigin`, `kitChanges` and `initWith`;
  - the three messages;
  - `withKitContent` and the slimmed `loadKit`;
  - `guardKitFolder` and `withKitCheckFailure`;
  - the three arms.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs` (+123 / −1): the open, the generic
  `withLoadedScene`, four helpers and five facts.

## Decisions

- **`withKitContent` also clears `kitStatus` and `kitChanges`.**
  - `loadKit` already cleared `kitStatus`, and the reload must clear both.
  - With both inside the shared function, "a reload sets exactly what an open sets" holds literally. An open also
    starts with no offer.
  - `loadKit` keeps `kitFolderOpt` and `experimentData`, as the slice says. It sets `experimentData` before the shared
    function runs, so the attachment recheck still reads the opened kit's `experimentData`. That preserves the two
    `InverseConstructorTests` facts that pass `{ opened with experimentData = … }`.
- **One guard for the scratch kit.** `guardKitFolder` follows the `guardKitWrite` precedent, so each of the three arms
  reads as its behaviour, and the rule that nothing outside the app changes a scratch kit is stated once.
- **One status text for a failed check.** `withKitCheckFailure` holds `The kit could not be checked for changes — …`.
  Keep mine's `recordStamps` refusal uses it too (the slice: "sets the check's status text"), so the text is not
  written twice.
- **The other writer's `E2` in the read-only fact.** The kit is first opened read-write, one experiment is committed
  through a model, and the kit is closed. It is then opened read-only, and the other writer writes that committed
  experiment with `id = ExperimentId 2`. That keeps `E2` "the committed experiment with `id = ExperimentId 2`" in
  every fact.
- **What the facts compare.** Re-read experiments are compared with what the kit holds now, read through
  `writer.readExperiments ()`, never with the in-memory committed record. The read-only fact checks the loaded `E1` by
  id. That way no fact depends on a float round trip through the experiment JSON.
- **The read-only fact's "writes no file".** The fact takes the folder's bytes right after the other writer's write.
  It then runs the check, the reload and a second check, and the bytes must be unchanged.
- **The further write in Keep mine.** The other writer renames the kit again, to `Renamed again`. Rewriting identical
  `E2` bytes would leave the content hash unchanged, so it would not be offered.

## Testing state

No build, test, analyzer or invariant gate was run. The `IMPLEMENT` role is act-only (Invariant 6), as at steps
0184–0186. The arc-runner's gate engine runs the frozen roster after this worker exits:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests (`InverseConstructorTests`, `SceneSaveLoadTests`);
- the fourteen s21 invariants.

Checked without running a gate:

- Both edited files are LF (zero CR bytes). `git diff --check` is clean, and `--numstat` equals
  `--ignore-cr-at-eol`.
- No added line matches any s21 invariant's pattern, and none uses `sprintf`.
- These names occur nowhere else in Berreman's or STL's sources (ELEV014 / ELEV015 / ELEV016): `KitCheckOrigin`,
  `KitCheckOnActivation`, `KitCheckOnRequest`, `CheckKitChanges`, `ReloadKitChanges`, `KeepMyKitChanges`,
  `kitChanges`, `withKitContent` and `guardKitFolder`.
- `initWith` is still the model's only full record construction (grep for `kitShell =`), so `kitChanges` is
  initialised everywhere.
- No other code matches `TableAndElementRotationMsg` exhaustively (grep for `CloseKit` / `ShowKitInExplorer`).
- `KitFolderStore` declares no public `let`, so opening it in the test shadows nothing.
- Every API the facts use has a precedent in the suites:
  - `KitProxy.create folder` with `readExperiments` (`ExperimentCollectionRoundTripTests`);
  - `Assert.Equal<KitContext>`, `Assert.Equal<TableAndElementRotationModel>` and `Assert.StartsWith`
    (`KitWindowModelTests`, step 186);
  - `Assert.NotEmpty` (`CanonicalJsonTests`);
  - `Assert.Empty` over an F# list (`InverseConstructorTests`);
  - `Assert.Equal<Map<FileName, FileByteContent>>` (`KitWindowModelTests`, step 185).

Test identities created, exactly the five the step declares (none removed or renamed):

- `OpticalConstructor.Tests.KitWindowModelTests.files another writer changed are offered, and the window's own writes are not`
- `OpticalConstructor.Tests.KitWindowModelTests.Reload reads the kit's files in and clears the offer`
- `OpticalConstructor.Tests.KitWindowModelTests.Keep mine keeps the model and clears the offer until the next change`
- `OpticalConstructor.Tests.KitWindowModelTests.a requested check with nothing changed says so, and an activation check stays silent`
- `OpticalConstructor.Tests.KitWindowModelTests.a kit opened read-only is offered changes and reloads them`

Expected counts:

- constructor-unit-tests rises by exactly 5, from checkpoint 185's 1202 to 1207;
- ui-smoke stays at 227;
- the selected `InverseConstructorTests` and `SceneSaveLoadTests` classes are unchanged, and their `loadKit` facts run
  over the moved body.

## Artifacts

None. No run was captured, because the role runs no gate.

## Gotchas

- `changedKitFiles` compares content hashes, not timestamps. A further outside write is offered only when its bytes
  differ from the recorded stamp.
- The check's names are leaf names (`.kit-json`, `E2.experiment-json`) in ordinal order, so `.kit-json` comes first.
- The scratch-kit guard has no fact of its own. The slice declares no test for it, and the five declared facts all run
  over folder kits. The guard is the same shape as `guardKitWrite` and is reached by every non-folder model.
- No operator note was in flight for this attempt. Manual 014 is about read-only access and storage refusal. This step
  changes neither, and it touches no `SceneSaveLoadTests` or `LauncherTests` fixture.

# Step 0260 — impl plan

## Goal

`<name>.setup-json` holds a `Setups.Setup` (spec 0050 S8.2), replacing S4.3's scene file. The constructor's scene
save / load becomes `CreateSetup` / `ActivateSetup`. `Scene.fs` keeps `SnapMode` alone.

## Approach (one pass, bottom-up so each layer compiles over the one below)

1. **Domain.** `Setups.fs` gains `Setup.libraryReferences` (step 147's rule, moved: current slots then each history
   record's, in slot order, each once, as `EntryLatestReference`), `SetupFile` with `currentSchemaVersion` = 1 and
   `type Setup with member this.setupFile`. `Scene.fs` deletes `SceneStoreError`, `SceneName`, `SceneElement`,
   `SceneSnapshot` and `SceneFile`. The fsproj comment above `Scene.fs` is cut to what stays true.
2. **Storage.** The `setup.schema.json` body becomes `{ schemaVersion, setup: { name, revision, slots, history } }`.
   `validateSceneFile` is renamed `validateSetupFile`. `SetupFile.fileText` and `KitFileText.parseSetupFile` reuse
   the step-146 stack. `KitProxy.writeSetup` / `readSetups` replace `writeScene` / `readScene` / `readSceneNames`,
   reusing `writeRenderedKindFile` / `readEveryKindFile`. `KitLibraryContext.recordSetup` replaces `recordScene`.
3. **Ui.** `OpenedKit.setups : Setup list` comes from `readSetups`. The table model gets `setupName`, `kitSetups`
   and `activeSetupOpt`. `withKitContent` takes the setups. `SetSetupName` / `CreateSetup` / `ActivateSetup`
   replace the scene messages. `captureScene` / `restoreScene` are deleted, and the view's setup row replaces the
   scene row. Ids: `setupNameField`, `createSetupButton`, `savedSetups`, `savedSetup` and `SetupName.caption`.
4. **Tests.** Follow the census. Rename the two classes (file, module, `<Compile>`), add `SetupModelTests` at the
   end of the compile list and migrate every scene fixture to a setup.

## Files

The slice's authorized edits, plus the files the retirements force (see Risks).

## Risks

- `LauncherTests.fs` (Ui.Tests) names `ConstructorUiId.sceneNameField` / `saveSceneButton`, which this step retires.
  It is not in the edit list, but the build cannot pass without it. Only the ids (and the matching wording) change;
  every assertion stays (`.manual/014` launcher fact).
- The slice's view brief puts a `kitStatus` line in the setup row. Since S5.3 the kit bar alone carries `KitStatus`,
  and `KitWindowTests` asserts it is single. The line stays the kit bar's (recorded as a decision).
- Constructor case count: three methods (four cases) leave and three facts arrive, so the count falls by one
  (`.manual/006`, step 0260).

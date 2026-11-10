# Step 0259 — impl plan

The table's elements become the active setup's slots: `TestElement` (Ui, `{ id; placement; zoom; beam }`) is the
shape of `Setups.SetupSlot` (Domain, step 256) without its `content`, so the lower type survives and `TestElement`
is deleted (AGENTS.md "A bijection is collapsed").

## Approach

1. `OpticalConstructor.Ui/TableAndElementRotationView.fs`
   - Delete `TestElement`; its `elementX` / `boxCorners` move unchanged onto a `type Setups.SetupSlot with`
     extension at the same place; the drawing extension `type TestElement with` becomes `type Setups.SetupSlot with`.
   - `TableAndElementRotationModel.elements : Setups.SetupSlot list`, doc cites spec 0050 S8.2.
   - `mkElement`, `mainSeedElements`, `initInverseWithKit`'s three seeds and `addElement`'s record build a slot with
     `content = Setups.EmptySlotContent`, each construction annotated (the file opens `Scene`, whose `SceneElement`
     shares the four labels).
   - `layoutElements` = `model.elements |> List.map (fun s -> s.layoutElement)`.
   - `captureScene` / `restoreScene` map slot <-> `SceneElement`, restoring with `EmptySlotContent`.
   - Every `TestElement` annotation names `Setups.SetupSlot`.
2. `OpticalConstructor.TestWindows/ElementRotationView.fs`: annotations + `onRay` gains the content; its comment
   naming `TestElement` names `Setups.SetupSlot`.
3. `OpticalConstructor.Domain/Scene.fs`: its `SceneElement` doc does not name `TestElement` (verified by grep), so
   nothing to change there.
4. Tests: annotations rename; `SceneRoundTripTests`' three fixtures and `OutOfBandBadgeTests`' flagged sample gain the
   content. New fact in `MainSceneMsgTests`:
   `the table's elements are slots: each one placed is empty, and each lays out as the table lays it out`.

## Risks

- `KitWindowModelTests.fs` (constructor suite) and `SceneSaveLoadTests.fs` (UI suite) also name `TestElement` in
  `Assert.Equal<TestElement list>` but are not in the authorized edit list. The acceptance requires the grep to find
  nothing and the build to pass, so their annotations must be renamed too — a mechanical, assertion-preserving rename,
  recorded as a deviation.
- Record-label resolution: with `TestElement` gone an unannotated `{ id; placement; zoom; beam }` would resolve to
  `Scene.SceneElement`; annotate every construction.
- ELEV: new extension members on `SetupSlot` (`elementX`, `boxCorners`, `testSceneViews`) must not collide with
  existing SetupSlot member names (checked: none do).

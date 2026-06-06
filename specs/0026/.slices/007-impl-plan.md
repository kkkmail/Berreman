# 007 — Impl plan: element groups, multiple detectors, collections, undo/redo (Part G/H/K)

## Approach

Final slice of arc 0026. Three feature areas, all net-new on top of slices 001–006.

**Part G/H (domain + storage):**
- New `OpticalConstructor.Domain/Groups.fs`: `ElementGroup` (named set of members, each a full
  `Placement.ElementPlacement` config), the two member modes (`MutuallyExclusive` / `MultiSelect`),
  pure `setInBeam`/`swapTo` operations that flip only `inBeam` (lossless — every member's stored
  config preserved, AC-G1), and `ExperimentCollection` (named, shared-sample experiments with
  element/group on/off toggles, AC-H1).
- New `OpticalConstructor.Storage/GroupsLibrary.fs` + `schema/optical-constructor-groups.schema.json`:
  load/save a separate per-user app-data JSON (`<AppData>/Softellect/Berreman/OpticalConstructor/groups.json`),
  reusing the shared `ProjectJson.options` and the validate-on-load seam (its own lazy schema, mirroring
  `UserEnvironment`). Independent of any `.ocproj` (AC-G3).

**Multiple detectors + primary (G.2):** represented WITHOUT a new project field (the project record is
constructed in ~12 sites; a new required field would break them all). Detectors are `Detector`-kind
placements; the primary is the FIRST detector (already the convention `centralRayEndpoints` uses).
`setPrimaryDetector i` reorders placements so detector `i` is first → a real project mutation that
round-trips and undoes, with `rayOf`/`selection` index-remapped.

**Part K (undo/redo + safety):**
- Replace `ConstructorView`'s single-level `undo`/`redo` option fields with the shared multi-level
  `History.EditHistory` (R-8 reuse, no parallel history). Invariant: `model.history.present = model.project`.
  Convert the existing `snapshot`-before-edit helpers to a `commit`-after-edit (`History.push`), so every
  project-mutating edit (placement, rotation, lock, emission, delete, reset-rotation, duplicate/paste,
  group on/off, swap, detector add/remove/set-primary) pushes one undoable step. Slide drags push once at
  `EndDrag` (guarded against no-op). `Undo`/`Redo` step `History.undo`/`redo` (multi-level, AC-K1).
- Destructive-action safety (K.2): `reset rotation` and `delete` already arm `pending`; add `reset view`
  to the gate (it is ephemeral view state, not in `EditHistory`, so it must be confirmation-gated). Render
  the same-row Confirm/Cancel gate via `Controls.destructiveGate` (distinct positive/negative CTA colours).
- Render the front-door element overlays slice 006 deferred (context menu, element dialog placeholder,
  confirm gate), and DROP the four element-edit commands from `commandsWithoutFrontDoorSurface` (they now
  have surfaces → re-enabled). Update the two slice-006 RibbonTests that asserted them disabled.

**Commands (constraint 0.4):** add `SwapGroup`, `AddDetector`, `RemoveDetector`, `SetPrimaryDetector` to the
ONE registry; `tabOf` classifies them onto the Experiment tab; `SetPrimaryDetector` is `inContextMenu`
("Set as primary"). `AddDetector`/`RemoveDetector`/`SetPrimaryDetector` are parameterless-invokable;
`SwapGroup`/`ToggleGroup` are parameterized (disabled generic button + real Experiment-tab extras).

**Ribbon:** thread `cv` into the content fns; render Experiment-tab group/detector controls; add the
context-menu, confirm-gate and element-dialog overlays (rendered by `Shell.constructorBody`).

## Files

New: `Groups.fs`, `GroupsLibrary.fs`, `optical-constructor-groups.schema.json`, `GroupsRoundTripTests.fs`,
`GroupDetectorUndoTests.fs`.
Edited: Domain/Storage `.fsproj` (compile + content), `Commands.fs`, `ConstructorView.fs`, `LocalHelp.fs`,
`strings.json`, `Ribbon.fs`, `Shell.fs`, `RibbonTests.fs`, `CommandRegistryTests.fs`, `Ui.Tests.fsproj`,
`Tests.fsproj`.

## Risks
- Part K is cross-cutting: every existing snapshot site must convert to `commit` without double-pushing
  (slide = one push at EndDrag; SlideTo mid-drag does not push). Existing AC-E5 single-level undo test must
  still pass against the multi-level history.
- Group↔project sync: group on/off diff-syncs member placements into `project.placements` (add on, remove
  first-equal off). `model.groups` `inBeam` is the durable config store; undo restores the project (the
  gated assertion), `inBeam` is best-effort across undo (documented).
- Re-enabling the four element-edit commands changes two slice-006 tests; gating reset-view changes one
  slice-005 test — all updated deliberately, the rest of those files untouched.
- Groups schema must accept the exact FSharp.SystemTextJson serialization (Angle as bare number, fieldless
  DUs as bare strings, option as null).

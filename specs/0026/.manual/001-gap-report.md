# Spec 0026 Gap Report - Optical Constructor UI

## Scope

Reviewed:

- `specs/0026/.spec-md`, especially the binding constraints and acceptance criteria.
- Related spec-folder data: `.slices/*`, `.summary-md`, `.feature-list-md`, `.manual/000-comments.docx` text/comments, and slice critic/postmortem artifacts.
- Current implementation touched by `0026*` commits, especially:
  - `1205027` slice 001 placement/domain
  - `42f9c78` slice 002 ray model
  - `be25197` slice 003 localization
  - `3650bd1` slice 004 table/drawer/controls
  - `decffdc` slice 005 commands/interactions
  - `9b75071` slice 006 ribbon/front door
  - `662aae8` and `6a0801a` slice 007/manual group-detector-undo work

This was a source/spec audit. I did not change implementation code and did not run the full gate set.

## Executive Summary

The implementation contains many domain/helper modules and many tests, but the actual operator-facing constructor is mostly an unwired shell. The broad pattern is:

- Pure helper code exists for placement, ray tracing, drawer geometry, table state, groups, and storage.
- The live constructor canvas does not use most of that helper code.
- The constructor page owns a separate project snapshot that is not wired into the app's open/new/save lifecycle.
- Several tests assert the weakened helper-level behavior rather than the user-facing behavior the spec and original manual comments describe.

So the user's suspicion is supported: the implementation is not merely missing polish. Several central acceptance criteria are not delivered in the live app.

## Critical Gaps

### 1. Constructor edits are disconnected from app lifecycle and persistence

Spec requirements:

- The constructor is the front door (`.spec-md` D.6 / AC-D4).
- `Ctrl+S` must save the project (`.spec-md` E.6 / AC-E5).
- Reused storage is `saveProject` / `openProject` (`.spec-md` 0.7, A.8, E.6).

Current code:

- App startup creates a constructor model once: `Shell.fs:222` calls `ConstructorView.init env initialProject`.
- New/open/template/gallery load paths call `loadProjectInto`, but that function updates only `construction`, `workspace`, `fit`, `page`, and `status`; it does not update `constructor` (`Shell.fs:456-467`).
- Those load paths also land on `Page.Construction`, not the constructor front door (`Shell.fs:467`, `Shell.fs:473`, `Shell.fs:477`, `Shell.fs:489`, `Shell.fs:493`).
- Save is wired to the legacy construction model: `LifecycleView.SaveRequested -> saveCmd model.construction` (`Shell.fs:479`).
- Constructor `Ctrl+S` only increments an in-memory counter: `SaveProject -> { m with saveRequests = m.saveRequests + 1 }` (`ConstructorView.fs:587`).
- Shell routes constructor messages with no effect command: `RootMsg.Constructor cm -> { model with constructor = ConstructorView.update cm model.constructor }, Cmd.none` (`Shell.fs:549`).

Impact:

- Editing the constructor front door does not save through the app save path.
- Opening/loading a project does not refresh the constructor model.
- The legacy and constructor pages can show different projects.
- The test suite validates the wrong path: `LifecycleTests.fs` saves/opens `model.construction`, while `CommandRegistryTests.fs` treats `saveRequests += 1` as enough for AC-E5.

This is probably the highest-impact gap because it makes the new "front door" non-authoritative.

### 2. Fresh projects do not contain actual default light-source and detector placements

Spec/manual requirements:

- A fresh project must place the light source left-middle and detector right-middle, exactly `2.0 m` apart on the CR (`.spec-md` B.4 / AC-B4).
- The manual comments repeat this explicitly: light source near the left side, detector near the right side, initial distance 2 meters, light-source reset re-snaps the rest.

Current code:

- `Templates.projectOf` creates projects with `placements = []` (`Templates.fs:78`).
- The constructor draws a virtual central ray by falling back to `RayModel.defaultSourcePoint` and `RayModel.defaultDetectorPoint` when no placements exist (`ConstructorView.fs:235-247`, `ConstructorView.fs:918-923`).

Impact:

- The apparent light source and detector are not actual project elements.
- They cannot be selected, edited, reset, saved, or round-tripped as placements.
- The default project starts as an empty table with a line, not a usable LS -> ... -> D optical constructor.

### 3. Ray snapping, downstream travel, Snell tracing, and orphaning are domain-only and not wired into the constructor

Spec requirements:

- The constructor must draw CR plus side rays, IRG/RRG/TRG, detector termination, branch-consistent groups, snapping, downstream travel, orphaning, and schematic Snell tracing (`.spec-md` Part B / AC-B1..B6).
- If an upstream element rotates, downstream placement points must travel with the ray while preserving distances (`.spec-md` B.3, B.4, B.6; manual comments items 60-62).

Current code:

- `RayModel.fs` defines `outgoingDirection`, `snapChain`, `RayChain`, `removeRay`, and `restoreRay` (`RayModel.fs:243`, `RayModel.fs:296`, `RayModel.fs:313`, `RayModel.fs:369`, `RayModel.fs:392`).
- Production `ConstructorView` does not call `RayModel.snap`, `snapChain`, `removeRay`, `restoreRay`, or `outgoingGroups`.
- Rotating an element edits only that active placement: `editActive` calls `setPlacement i` and commits (`ConstructorView.fs:204-208`).
- Sliding moves only the active element's `x` coordinate and clamps by neighbor `x` values (`ConstructorView.fs:254-281`).
- Reassigning a ray only changes an ephemeral `rayOf : Map<int, RayId>` (`ConstructorView.fs:88`, `ConstructorView.fs:130`, `ConstructorView.fs:324-326`).

Impact:

- Downstream elements do not travel when a ray bends.
- Branch reassignment does not redraw geometry onto a new branch.
- Orphaning/restoration rules are not present in the live UI.
- Snell tracing is testable in isolation but absent from the canvas.
- The constructor is not actually "driving" the ray model in the way the spec requires.

### 4. The live canvas bypasses the standard drawer, side rays, reflected/transmitted groups, and bounding-box toggles

Spec requirements:

- The top-down surface must draw the optical table, standard cylinder drawer, role-specific source/detector shading, optional bounding box, active indicator, CR-only/all-rays toggle, and schematic ray weights (`.spec-md` C.1-C.7 / AC-C4..C6).

Current code:

- `Drawer.draw` exists and returns cylinder geometry plus optional bounding-box edges, but the live `ConstructorView.elementView` does not call it.
- The actual view renders:
  - plate rectangle (`ConstructorView.fs:900-916`)
  - one central line (`ConstructorView.fs:918-928`)
  - each element as a simple ellipse (`ConstructorView.fs:930-943`)
  - active indicator ring (`ConstructorView.fs:945-965`)
  - children list contains only those items (`ConstructorView.fs:995-998`)
- `showCentralRayOnly` and `showBoundingBox` are model fields initialized from defaults (`ConstructorView.fs:89-90`, `ConstructorView.fs:131-132`) but there is no production toggle/update path found for either.
- `ConstructorTable.drawnSideRayCount` and `Drawer.draw` are tested directly, not through the rendered constructor surface (`ConstructorViewTests.fs:81`, `ConstructorViewTests.fs:111-116`, `ConstructorViewTests.fs:141-145`).

Impact:

- Element rotations are not visible in the actual element drawing.
- The value-id parameter is not used by the rendered element.
- Bounding boxes cannot be toggled on the live canvas.
- Side rays are never rendered.
- RRG/TRG are never rendered.
- Detector/source styling is reduced to a filled ellipse, not the specified cylinder/frame geometry.

### 5. Ribbon "drag-to-place" is not a drag/drop interaction and ignores the drop point

Spec/manual requirements:

- Elements are dragged from the ribbon to the table and snap to the middle of the closest CR path when the mouse is released (`.spec-md` E.7 / AC-E4; manual comments item 66).

Current code:

- Catalogue entries are normal buttons, not drag sources. Each button dispatches `RibbonDrop(kind, TablePoint.origin)` (`Ribbon.fs:234-239`).
- The update handler ignores the supplied point: `RibbonDrop (kind, _point)` then creates the element at `centralRayMiddle model` (`ConstructorView.fs:789-790`).
- The test asserts this weakened behavior: a "drop" at `(0.4, 0.3)` must ignore the free point and use `centralRayMiddle` (`CommandRegistryTests.fs:238-246`).

Impact:

- There is no real drag from ribbon to table.
- There is no nearest-CR-path calculation.
- Multiple branch paths cannot be targeted by placement.
- The current test suite locks in a simplified behavior that is not the user-facing interaction described by the spec.

### 6. Table resizing and table view rotation are missing from the live constructor

Spec requirements:

- User may edit the optical table size and it persists/reloads (`.spec-md` C.1 / AC-C1; manual comments item 47).
- The table view supports pan, zoom, rotate, and reset; R1/R2/R3 for the table are view rotations relative to screen (`.spec-md` C.2; manual comments item 30).
- Table resize is listed as a constructor snapshot edit in K.1 (`.spec-md` K.1.1).

Current code:

- `Table.withSize` exists as a pure helper (`Table.fs:74-75`) and persistence tests cover table round-trip, but no constructor UI command or message edits table size.
- Commands only expose `PanView`, `ZoomView`, and `ResetView` for table/view (`Commands.fs:128-130`, `Commands.fs:243-248`).
- `panBy`, `zoomBy`, and reset are the only live table-view mutations found (`ConstructorView.fs:512-520`, `ConstructorView.fs:814`).
- `projectToCanvas` applies `view.r1`, but no command changes `view.r1`; `view.r2` and `view.r3` are effectively data-only in the live top-down canvas.

Impact:

- Users cannot resize the table from the constructor.
- Users cannot rotate the table view as specified.
- K.1 cannot be satisfied for table resize because there is no table-resize action.

### 7. Groups and collections are not live app features, and group undo/redo leaves state inconsistent

Spec requirements:

- Groups and experiment collections must live in a separate per-user JSON file and be reusable across projects (`.spec-md` G.3 / AC-G3, H.1 / AC-H1).
- Group on/off and swap must participate in multi-level undo/redo (`.spec-md` K.1 / AC-K1).
- Groups should preserve member configuration losslessly across swaps (`.spec-md` G.1 / AC-G1).

Current code:

- `GroupsLibrary.fs` implements serialize/deserialize/load/save (`GroupsLibrary.fs:73`, `GroupsLibrary.fs:84`, `GroupsLibrary.fs:115`, `GroupsLibrary.fs:127`).
- No production UI/shell code calls `GroupsLibrary.load`, `GroupsLibrary.save`, or `GroupsLibrary.libraryPath`.
- `ConstructorView.init` always starts with `groups = []` (`ConstructorView.fs:142`), despite the model comment claiming it is loaded/persisted through `GroupsLibrary` (`ConstructorView.fs:114-115`).
- Slice 007 state data admits this directly: "Live app-boot wiring of the groups library is not added" (`007-state-of-the-world.md:116-118`), even though the spec marks G.3 as required.
- The only live group creation path creates a one-member multi-select group from the active element (`ConstructorView.fs:497-505`, `Ribbon.fs:317`).
- `ExperimentCollection` exists only as a domain/storage value (`Groups.fs:114-129`); there is no live collection UI beyond a string key.
- `applyGroupChange` mutates `model.groups`, computes a project placement delta, and commits only the project (`ConstructorView.fs:482-490`).
- Undo/redo restore only `project` and `history`; they do not restore `groups` (`ConstructorView.fs:581-585`).

Impact:

- Reusable groups/collections do not load at startup and do not save after edits.
- Collections are not user-facing.
- Undoing a group toggle/swap can restore placements while leaving `groups[].members[].inBeam` in the post-edit state. The group UI and project placements then disagree.
- Existing group tests check placements after undo, but not the group's own `inBeam` state (`GroupDetectorUndoTests.fs:193-220`).

### 8. Multiple detectors are not connected to results or branch-specific readouts

Spec requirements:

- With multiple detectors, the user can choose the primary; headline readout follows the primary while secondary detectors report their own branch (`.spec-md` G.2 / AC-G2).

Current code:

- The constructor defines detector indices and primary as the first detector placement (`ConstructorView.fs:401-411`).
- "Set as primary" reorders placements (`ConstructorView.fs:430-439`).
- The ribbon shows only a count and primary index (`Ribbon.fs:306-310`).
- Results/readout surfaces (`ResultsView`, `Workspace`, chart/readout modules) do not consume `ConstructorView.primaryDetectorIndex` or detector branch state.

Impact:

- "Primary detector" is currently a placement-order convention and a ribbon label.
- There is no evidence that headline numerical results follow the chosen detector.
- Secondary detectors do not report branch-specific values.

## Major/Secondary Gaps

### 9. Localization is partial, not "all UI strings"

Spec requirements:

- All UI strings resolve through runtime `strings.json`; editing the file changes strings with no rebuild (`.spec-md` I.1 / AC-I1).

Current code:

- `Localization.fs` and `strings.json` exist and are used by the ribbon/front-door pieces.
- Many UI strings remain hardcoded, including app titles (`Program.fs:55`, `Program.fs:83`), status messages (`Shell.fs:478-494`), group names (`ConstructorView.fs:500`), and large parts of existing panels (`ChartView.fs`, `FitView.fs`, `MaterialsView.fs`, `ConstructionView.fs`, `ResultsView.fs`, `SourceView.fs`, `LifecycleView.fs`, etc.).

Impact:

- The EN/RU mechanism is real but not complete.
- If the spec is read literally as "all UI strings", AC-I1 is not met.
- If the intended scope was only the new constructor surface, that scope should be stated explicitly; the current spec does not narrow it that way.

### 10. Tests and state reports overstate completion

Examples:

- `ConstructorViewTests.fs` validates `Drawer.draw` and `ConstructorTable.drawnSideRayCount` directly, but the live canvas does not use them.
- `CommandRegistryTests.fs` validates that `Ctrl+S` increments `saveRequests`, not that the shell saves the constructor project.
- `LifecycleTests.fs` saves/opens only the legacy `construction` model.
- `CommandRegistryTests.fs` validates a weakened "ribbon drop" seam, not actual drag/drop.
- `GroupDetectorUndoTests.fs` validates project placements after group undo, not the group state that remains mutated.
- Slice 006 state explicitly notes constructor and legacy pages hold independent project snapshots (`006-state-of-the-world.md:158`), but the arc still closes with the constructor as the front door.
- Slice 007 state explicitly defers live groups-library loading/saving (`007-state-of-the-world.md:116-118`), despite G.3/AC-G3 being a must-level requirement.

Impact:

- Passing gates are not strong evidence that the operator-facing spec works.
- The tests often prove the presence of helper modules or simplified update paths, not the integrated behavior users need.

## Acceptance Coverage Snapshot

| Part | Status | Notes |
| --- | --- | --- |
| A - placement domain | Mostly helper-level present | Domain record, rotations, emission invariant, schema round-trip appear present. Live value-id binding remains placeholder by design. |
| B - ray model/snapping | Not integrated | `RayModel.fs` exists, but the constructor canvas/update does not use it for drawing, snapping, downstream travel, or orphaning. |
| C - table/rendering | Mostly not live | Plate and one CR line render. Drawer, bounding box, all-rays toggle, outgoing groups, table resize, and view rotation are missing or helper-only. |
| D - ribbon/front door | Partially present | Top-level page/ribbon exist. It is not authoritative because lifecycle/save/open still target legacy construction state. |
| E - commands/interactions | Partial | Some keyboard/mouse update paths exist. Save is not wired to shell persistence. Drag-to-place is not real drag/drop. |
| F - catalogue/value-id | Partial | Catalogue entries exist; value-id empty modal exists. Placement uses button dispatch rather than drag/drop. |
| G - groups/detectors/storage | Partial and inconsistent | Group domain/storage exists. Live load/save absent. Group undo desyncs group state. Detector primary not connected to results. |
| H - collections | Storage-only | Domain/storage round-trip exists; no live collection UI or behavior. |
| I - localization | Partial | Runtime resource exists for new shell pieces, but many UI strings remain hardcoded. |
| J - controls | Helper-level present | `Controls.fs` exists; broad UI still uses many ad hoc/hardcoded controls. |
| K - undo/redo/safety | Partial | Project snapshot history exists. It does not cover constructor lifecycle, group state, table resize, or view reset undoability beyond confirmation. |

## Bottom Line

The implementation should not be considered a completed Spec 0026 implementation. It is closer to a scaffold:

- Domain helpers and storage helpers landed.
- A ribbon/canvas shell landed.
- The live app integration did not land for the core workflows: save/open, default LS/D, ray-driven placement, actual optical drawing, real drag/drop, groups persistence, collections, detector readout, table resize, and table view rotation.

The minimum repair direction is to make the constructor project the single authoritative project state in the shell, then wire the live canvas to the existing ray/drawer/group/storage helpers. Without that, additional helper tests will keep making the arc look green while the operator-facing constructor remains non-functional.

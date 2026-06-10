# 006 - Optical Constructor UI Gap Closure

Date: 2026-06-06
Source repo: C:\GitHub\Berreman
Related spec: C:\GitHub\Berreman\specs\0026\.spec-md

## Scope

This pass closed the current Spec 0026 constructor UI gap reported against the table drawer and interaction model:

- Build catalogue buttons must only arm placement.
- The next table click must drop the pending element; the button click must not create or visibly place it.
- While placement is armed, pointer movement over the table must show the pending element with the pointer at the element bounding-box center.
- Elements must be selectable from the table, and the element menu/rotations must act on that selected element.
- The default top-down/rest-pose cylinder must render side-on as a rectangle, not as a rectangle plus end circles.

The user-requested click-drop workflow intentionally supersedes the literal Spec 0026 E.7 drag wording for the current UI behavior.

## Gaps Found

1. The prior click-drop implementation still initialized `PlacementDraft.previewPoint` to the central-ray midpoint. No project element was committed, but the live draft rendered immediately at the table center, so the UI still looked like the element appeared automatically after pressing the Build button.

2. The table pointer handler relied on drag classification to establish active element selection. That made selection/menu/rotation behavior too fragile: if the active element was not selected by the time a menu command fired, menu rotations looked dead.

3. The armed-placement cursor was only a standard crosshair plus the table ghost. The requirement explicitly asked for an element-shaped cursor with its center as the placement point.

4. The regression set did not pin the complete user path: arm from catalogue, no immediate placement/preview point, pointer-preview update, table click commit, right-click menu selection, and menu-invoked rotation.

## Changes Made

- `ConstructorView.PlacementDraft.previewPoint` is now `TablePoint option`. `StartPlacement` stores `None`, so a Build button press arms the placement without committing an element and without drawing a table-center draft.

- `PreviewPlacementAt` now sets `Some point`, and `placementDraftView` renders only when a preview point exists. This makes pointer movement the first moment the pending element becomes visible.

- `DropPendingPlacement` still commits only on the next table click and snaps the click point onto the nearest central-ray path.

- The Build catalogue continues to dispatch `StartPlacement kind`; it does not dispatch the direct `RibbonDrop` append path from the button UI.

- The pointer press path now dispatches `SelectAt` before `BeginDrag` for ordinary left clicks. This makes table/element selection explicit and keeps the Element tab, context menu, and rotations tied to the clicked element.

- The armed-placement cursor now uses Avalonia's bitmap cursor constructor. The bitmap is generated from the same `Drawer.draw` geometry used for the table element, with the cursor hotspot at the element center. If a platform rejects custom cursors, the code falls back to the crosshair rather than breaking placement.

- Context-menu and rotation behavior remains wired through the centralized command registry. `RotateR1`, `RotateR2`, and `RotateR3` are parameterless menu/ribbon effects that perform one configured rotation step on the active element, while wheel gestures continue to rotate by notches.

- Regression coverage was extended for:
  - click-drop catalogue flow with no project append and no preview point on the button click;
  - preview point appearing after pointer movement;
  - table-click commit snapped to CR;
  - right-click opening the element menu on the clicked element;
  - menu-invoked rotation mutating that selected element.

Earlier closure work in the same dirty 0026 implementation set also fixed the rest-pose cylinder drawer: side-on default cylinders now render as rectangular side silhouettes, with circular caps only when the cylinder axis projects end-on.

## Key Files

- `C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui\ConstructorView.fs`
  - `PlacementDraft`, `StartPlacement`, `PreviewPlacementAt`, `DropPendingPlacement`, `placementDraftView`, pointer press handling, custom placement cursor.

- `C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui\Ribbon.fs`
  - Build catalogue element buttons dispatch `StartPlacement` instead of directly appending placements.

- `C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui\Commands.fs`
  - R1/R2/R3 remain centralized active-element commands and are surfaced in the element context menu.

- `C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui\Drawer.fs`
  - Projection-correct side-on/end-on cylinder geometry from the prior closure pass.

- `C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\CommandRegistryTests.fs`
  - Added placement/menu/rotation regression coverage.

- `C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\ConstructorViewTests.fs`
  - Covers rectangle side-on drawer projection and hit-test alignment.

## Verification

Release-only verification was run, per instruction. No Debug build or Debug test was run.

- `dotnet test Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj -c Release --no-restore -m:1`
  - Result: passed, 102/102 tests.

- `dotnet build Berreman/OpticalConstructor/OpticalConstructor.App/OpticalConstructor.App.fsproj -c Release --no-restore -m:1`
  - Result: build succeeded, 0 errors.

The sandboxed shell could not write the Release `obj` folder, so the successful Release test/build commands were run with escalation. The failed sandbox attempts were permission failures on generated build artifacts, not test failures.

Known existing warnings during verification:

- `NU1902`: `log4net` 3.2.0 moderate-severity advisory.
- `MSB3277`: unresolved `WindowsBase` 4.0.0.0 vs 5.0.0.0 reference conflict involving WebView2 WPF.

## Current Expected Behavior

1. Click a Build catalogue element.
2. No element is added to the project and no table-center element appears.
3. Move the mouse over the table.
4. The cursor is an element-shaped cursor centered on the pending element, and the table also shows the pending element at the pointer center.
5. Click the table.
6. The element is committed at the clicked along-ray position, snapped to the nearest central-ray path, and becomes selected.
7. Right-click the selected element to open the element menu.
8. Menu rotations R1/R2/R3 act on the selected element, with lock semantics preserved.

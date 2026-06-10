# Spec 0026 Gap Closure Log

## 2026-06-06 - Start

Goal: close the major Spec 0026 implementation gap documented in `001-gap-report.md`.

Working backlog:

1. Make the constructor project authoritative for new/open/template/gallery/save flows.
2. Seed fresh projects with real default light-source and detector placements.
3. Wire the live table rendering to the drawer/ray geometry instead of the current ellipse shortcut.
4. Fix the optical element orientation: `(0,0,0)` means the ray goes through the cylinder central axis, not that the cylinder axis is perpendicular to the table.
5. Wire side-ray / CR-only, bounding-box, table resize/view controls where needed.
6. Wire groups/collections persistence and avoid group/project undo desynchronization.
7. Add focused regression tests and run the relevant gates.

Current first move: inspect and repair the shell/constructor project split before changing drawing. If the constructor is not the saved/opened project, every other fix remains decorative.

## 2026-06-06 - Move 2: Make constructor state authoritative

Patched the template and shell path so a new optical constructor project starts with real light-source and detector placements, not only a beam-tree record. Save requests from the Constructor page now serialize `ConstructorView.Model.project`; loading a project opens the Constructor page and synchronizes the legacy Construction/Workspace mirrors from that same project. The old Construction page can still emit updates, but Shell now immediately mirrors those into the constructor model so save/open/lifecycle state does not split across three independent project copies.

## 2026-06-06 - Move 3: Replace live table shortcuts with model geometry

Rewired the constructor canvas to render ray segments from the `RayModel`/`ConstructorTable` helpers and element geometry from `Drawer.draw`. This fixes the most visible orientation bug: at `(R1,R2,R3) = (0,0,0)`, the drawn cylinder axis is along the central light ray, not standing perpendicular to the table. The Trace/View ribbon now exposes CR-only/all-rays, bounding-box visibility, view R1 spin, and persisted table length/width controls. Ribbon drops now use the supplied drop point and snap it onto the nearest point of the current central-ray segment instead of ignoring the point and always placing at the midpoint.

Also loaded the separate groups/collections library into Shell, seeded constructor groups from it, preserved collections when saving group changes, and made undo/redo resynchronize group `inBeam` flags from the restored project snapshot. This closes the visible group/project desynchronization that let the Experiment tab lie after undo.

Verification so far:

- `dotnet restore Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` completed.
- `dotnet test Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj --no-restore` passed: 97/97 tests.
- Known warnings during verification: existing `log4net` NU1902 advisory and WindowsBase version-conflict warnings.

## 2026-06-06 - Move 4: Surface detector readout and broaden verification

Added a detector section to the Results panel. It reads detector placements from the synchronized workspace project, marks the first detector as primary, and lists secondary detectors separately. This gives the primary-detector choice a visible result/readout surface instead of leaving it only as an ordering mutation in `ConstructorView`.

Final verification in this pass:

- `dotnet test Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj --no-restore` passed: 97/97 tests.
- `dotnet restore Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` completed.
- `dotnet test Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj --no-restore` passed: 234/234 tests.
- Known warnings remain the same: existing `log4net` NU1902 advisory, WindowsBase version-conflict warnings, and pre-existing `SeriesDataTests.fs` generic `Range` inference warnings in the constructor test project.

No questions for the user in this pass.

## 2026-06-06 - Move 5: Correct drawer projection and make placement/menu interactions live

User correction received: the default table view is top-down, the CR is horizontal through the table middle, and an element at `(R1,R2,R3) = (0,0,0)` has its R1/N1 cylinder axis parallel to the CR. Therefore the orthographic projection of that cylinder is the side view: a rectangle. It must not draw a rectangle plus two circular end caps.

Projection reference checked: orthographic/multiview projection uses parallel projection and separates top/side/end views; a cylinder presents a circle in the view looking down its axis and a rectangle in side views. Useful references: [McGill Engineering Design - Projections and Views](https://www.mcgill.ca/engineeringdesign/step-step-design-process/basics-graphics-communication/projections-and-views) and [UCLA MAE - Cylindrical Surfaces in Orthographic Projections](https://wireless.ucla.edu/MAE94/chap8/negative.htm).

Changes made in this pass:

1. Fixed `Drawer.draw` / `ConstructorView.elementView` so visible circular caps are emitted only when the cylinder axis is projected end-on in the top view. The rest-pose side-on cylinder now renders as a rectangular side silhouette with the axis line, not as the old smashed rectangle-plus-circles look.
2. Replaced the circular centre hit-test with a hit-test against the projected drawer footprint, so the clickable/selectable area matches the actual visible element rectangle/cap projection.
3. Changed Build-catalogue placement from one-click insertion to click-then-drop: clicking a catalogue role arms a pending placement, pointer movement shows the element preview with the mouse at the bounding-box centre, and the next table click commits the element through the same nearest-CR snap.
4. Added a placement cursor cue (`Cross`) while a pending element is under the pointer. The true element-shape cue is the live ghost element drawn at the pointer centre.
5. Made parameterless rotation commands real: invoking `RotateR1`, `RotateR2`, or `RotateR3` from ribbon/menu applies one configured rotation step to the active element. Wheel gestures still use the same command registry and still respect locks; R3 remains locked until unlocked.
6. Surfaced the missing context-menu element controls: lock/unlock R1/R2/R3 and toggle reflected/transmitted ray emission. The overlay now receives the active constructor model so those labels reflect the selected element lock state.
7. Added regression coverage for the side-on rectangle / end-on cap rule, projected-footprint hit-testing, click-drop placement preview/commit, and menu-invoked rotations.

Release-only verification:

- `dotnet restore Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` completed.
- `dotnet test Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj -c Release --no-restore` passed: 101/101 tests.
- `dotnet build Berreman/OpticalConstructor/OpticalConstructor.App/OpticalConstructor.App.fsproj -c Release --no-restore` passed.

Known warnings remain pre-existing: `log4net` NU1902 moderate advisory and WindowsBase version-conflict warnings. No Debug build or Debug test was run in this pass.

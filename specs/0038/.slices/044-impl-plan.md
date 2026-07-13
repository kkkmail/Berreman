# 044 — IMPLEMENT — impl-plan

## Goal

Consolidate every scattered per-control `UiIds` automation-id module into ONE
`[<RequireQualifiedAccess>] module UiIds` (file `OpticalConstructor.Controls/UiIds.fs`,
compiled FIRST in Controls) with nested per-surface sub-modules. Id **values unchanged**;
control sites and tests re-point mechanically; no per-control `UiIds` module remains.

## Sources to consolidate (27 id-holder modules)

**Controls** (`namespace … + module X = + nested [<RequireQualifiedAccess>] module UiIds`):
Rotation, ElementPalette, RayPosition, Renderer, Ribbon, Library (LibraryControls),
LayerBands, Experiment, Materials (MaterialsControls), SampleLibrary, Category
(CategoryControls), FacetedTree; plus `ChartWindowIds` (ChartWindow.fs) → `UiIds.ChartWindow`.

**Ui views** (`module Qualified.Name + top-level module UiIds`): Handoff (SolverHandoffView),
TableAndElementRotation, SampleEditor, MaterialEditor, CategoryEditor, LibraryWindow
(LibraryWindowView), MaterialsWindow (MaterialsWindowView).

**TestWindows views**: RendererTest, SnapToReflected, SnapToBeam, ElementMovement,
ElementRotation, TableRotation.

**TestWindows.App**: TestLauncher (TestLauncherWindow.fs).

## Key constraint discovered

`OpticalConstructor.Controls` is deliberately **domain-free** (no Domain project reference).
`LibraryWindowView`/`MaterialsWindowView` UiIds contain **Domain-typed** parametric helpers
`versionRow : VersionNumber -> string` and `entryNode : MaterialId/string -> string`
(the latter also calls the view-local `entryNodeCode`) that are heavily used by tests
(`LW.UiIds.entryNode` ×27, `MW.UiIds.entryNode` ×23, `versionRow` ×4+4). These CANNOT live in
Controls. Resolution: move their **[<Literal>] constants** to `UiIds.LibraryWindow`/
`UiIds.MaterialsWindow` in Controls, but relocate `entryNode`/`versionRow` as plain top-level
`let` functions in their view modules (Domain types stay in Ui). References to those two
members rewrite `X.UiIds.entryNode` → `X.entryNode`, `X.UiIds.versionRow` → `X.versionRow`.

## Steps

1. Hand-author `OpticalConstructor.Controls/UiIds.fs` — one `[<RequireQualifiedAccess>]
   module UiIds` with a nested sub-module per surface; fixed ids as `[<Literal>]`, parametric
   helpers as functions (values byte-identical to source). Domain-free only.
2. Add `<Compile Include="UiIds.fs" />` as the FIRST compile item in Controls.fsproj.
3. Reference rewrite across all `.fs`:
   - Qualified: `RotationControls.UiIds.` → `UiIds.Rotation.`, … , `ChartWindowIds.` →
     `UiIds.ChartWindow.`; abbreviations `LW.UiIds.`→`UiIds.LibraryWindow.`,
     `MW.UiIds.`→`UiIds.MaterialsWindow.`, `Scene.UiIds.`→`UiIds.TableAndElementRotation.`.
   - Special members: `{LW,MW,LibraryWindowView,MaterialsWindowView}.UiIds.{entryNode,versionRow}`
     → `X.{entryNode,versionRow}`.
   - Bare self-refs inside defining files: `UiIds.<member>` → `UiIds.<Surface>.<member>`
     (except LW/MW `entryNode`/`versionRow` → bare).
4. Delete the 27 per-control `UiIds`/`ChartWindowIds` module blocks (their doc-comments too);
   relocate LW/MW `entryNode`/`versionRow` to top-level view functions.
5. Add `open OpticalConstructor.Controls` to the ~14 referencing files that lack it (Controls
   files need none — same namespace). Ensure TestWindows.App sees Controls (transitive).
6. Build (`dotnet build Berreman.slnx -c Release`), fix, then run constructor/ui-smoke/ui tests.

## Risks

- Reference rewrite must not corrupt already-qualified refs (use negative lookbehind).
- `[<Literal>]` normalization must not turn parametric functions into literals.
- LF line endings must be preserved.
- Sub-module name collisions avoided (Library vs LibraryWindow, Materials vs MaterialsWindow
  vs MaterialEditor, Category vs CategoryEditor — all distinct).

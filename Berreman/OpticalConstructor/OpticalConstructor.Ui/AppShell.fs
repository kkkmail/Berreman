/// §J.8 — theme & dockable panels [Standard] (010 Part II §1/§3). The `AppShell`
/// hosts the dockable/resizable panels (stack, materials, sources, chart, results)
/// with saved layouts, a light/dark theme, and configurable chart color palettes.
/// The theme selection and the serialized panel layout are fields of
/// `EnvironmentSettings` (J.6, `UserEnvironment.fs`) and round-trip through its
/// JSON (AC-J8); this module reuses those types rather than redefining them.
///
/// **FuncUI-clone gate (constraint 5 / §A.6 / §A.9 / AC-J8).** §J.8 states the
/// dockable-panel mechanism MUST use Avalonia's docking facilities THROUGH the
/// FuncUI clone — and therefore that the clone at `C:\GitHub\Avalonia.FuncUI.Clone\`
/// MUST NOT be referenced, linked, or built until the mandatory audit passes, with
/// its linking mechanism (local NuGet package OR project reference) left UNRESOLVED
/// here. This shell is therefore authored against the FuncUI DSL SURFACE — the
/// public MIT `Avalonia.FuncUI` NuGet (slice 001) — so it compiles under the `build`
/// gate, while the audit-gated clone stays unreferenced and its linking choice
/// unresolved. The richer docking-facility binding (drag-to-redock, floating
/// windows) is the clone's to supply once cleared; this shell lays the saved panels
/// out with the public Avalonia layout primitives only.
module OpticalConstructor.Ui.AppShell

open Avalonia.Controls
open Avalonia.Media
open Avalonia.Styling
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Ui.UserEnvironment

// ---------------------------------------------------------------------------
// Theme & palette boundary mappings (J.8). Avalonia-aware, but pure value maps —
// no charting engine is implemented here; palettes are merely surfaced to Part H.
// ---------------------------------------------------------------------------

/// Map the persisted `Theme` to an Avalonia `ThemeVariant` (J.8). The only place
/// the theme label crosses into Avalonia's styling system.
let themeVariant (theme : Theme) : ThemeVariant =
    match theme with
    | Light -> ThemeVariant.Light
    | Dark -> ThemeVariant.Dark

/// Toggle the light/dark theme (J.8). Pure; the caller persists the result through
/// `EnvironmentSettings` so it round-trips (AC-J8).
let toggleTheme (theme : Theme) : Theme =
    match theme with
    | Light -> Dark
    | Dark -> Light

/// Surface the configured chart palette to Part H as Avalonia colors (J.8). Parses
/// the persisted hex strings; this is the palette HAND-OFF, not a charting engine
/// (the palette implementation stays in Part H, slice 012).
let paletteColors (settings : EnvironmentSettings) : Color list =
    settings.chartPalette |> List.map Color.Parse

// ---------------------------------------------------------------------------
// Avalonia-free layout reducers over the saved PanelLayout (J.8). These keep the
// layout edit logic testable without an Avalonia host; the FuncUI view below
// renders whatever they produce.
// ---------------------------------------------------------------------------

/// Set a panel's visibility in the saved layout (J.8). Unknown ids are a no-op.
let setPanelVisible (panel : string) (visible : bool) (layout : PanelLayout) : PanelLayout =
    { layout with
        panels =
            layout.panels
            |> List.map (fun p -> if p.panel = panel then { p with visible = visible } else p) }

/// Re-dock a panel to a different edge in the saved layout (J.8). Unknown ids are a
/// no-op.
let dockPanel (panel : string) (dock : DockSide) (layout : PanelLayout) : PanelLayout =
    { layout with
        panels =
            layout.panels
            |> List.map (fun p -> if p.panel = panel then { p with dock = dock } else p) }

/// The currently visible panels, in saved order — the set the shell renders.
let visiblePanels (layout : PanelLayout) : PanelState list =
    layout.panels |> List.filter (fun p -> p.visible)

// ---------------------------------------------------------------------------
// Dock-edge mapping seam (J.8). The shell view itself is supplied by the spec-0024
// MVU root (`Shell.view`, slice 001), which reuses this seam plus the layout
// reducers above; the placeholder `shellView`/`panelView` the slice-001 scaffold
// carried here are superseded and removed. Authored against the public FuncUI DSL
// surface so the build gate compiles it; the audit-gated clone stays unreferenced.
// ---------------------------------------------------------------------------

/// Map a saved `DockSide` onto Avalonia's `Dock`. `Center` is the fill region — a
/// `DockPanel` lets its LAST child fill, so center panels carry no dock attribute.
/// Public so `Shell.view` renders dock edges through this single seam (spec 0024 R-4).
let toDock (side : DockSide) : Dock option =
    match side with
    | Left -> Some Dock.Left
    | Right -> Some Dock.Right
    | Top -> Some Dock.Top
    | Bottom -> Some Dock.Bottom
    | Center -> None

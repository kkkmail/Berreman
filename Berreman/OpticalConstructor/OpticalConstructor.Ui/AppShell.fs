/// §J.8 — the theme & saved-layout seam over the persisted `EnvironmentSettings`
/// (J.6, `UserEnvironment.fs`). The old Elmish shell this module once served was
/// retired in spec 0038 Part B.1; what survives is the small seam that still has
/// live consumers: `themeVariant` (the ONLY place the persisted theme label crosses
/// into Avalonia's styling system — `App.Initialize` in the composition root) and
/// the Avalonia-free `setPanelVisible` reducer over the saved `PanelLayout`, which
/// keeps the persisted layout editable and testable without a window.
module OpticalConstructor.Ui.AppShell

open Avalonia.Styling
open OpticalConstructor.Ui.UserEnvironment

/// Map the persisted `Theme` to an Avalonia `ThemeVariant` (J.8). The only place
/// the theme label crosses into Avalonia's styling system.
let themeVariant (theme : Theme) : ThemeVariant =
    match theme with
    | Light -> ThemeVariant.Light
    | Dark -> ThemeVariant.Dark

/// Set a panel's visibility in the saved layout (J.8). Unknown ids are a no-op.
/// Pure — the caller persists the result through `EnvironmentSettings` so it
/// round-trips (AC-J8).
let setPanelVisible (panel : string) (visible : bool) (layout : PanelLayout) : PanelLayout =
    { layout with
        panels =
            layout.panels
            |> List.map (fun p -> if p.panel = panel then { p with visible = visible } else p) }

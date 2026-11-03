/// §J.8 — the theme & saved-layout seam over the persisted `EnvironmentSettings`
/// (J.6, `UserEnvironment.fs`). The old Elmish shell this module once served was
/// retired in spec 0038 Part B.1; what survives is the small seam that still has
/// live consumers: `Theme.themeVariant` (the ONLY place the persisted theme label
/// crosses into Avalonia's styling system — `App.Initialize` in the composition root)
/// and the Avalonia-free `setPanelVisible` reducer over the saved `PanelLayout`, which
/// keeps the persisted layout editable and testable without a window.
module OpticalConstructor.Ui.AppShell

open Avalonia.Styling
open OpticalConstructor.Ui.UserEnvironment

type Theme with

    /// This persisted theme as an Avalonia `ThemeVariant` (J.8).
    member this.themeVariant : ThemeVariant =
        match this with
        | Light -> ThemeVariant.Light
        | Dark -> ThemeVariant.Dark

/// Set a panel's visibility in the saved layout (J.8). Unknown ids are a no-op.
/// Pure — the caller persists the result through `EnvironmentSettings` so it
/// round-trips (AC-J8).
let setPanelVisible (panel : PanelId) (visible : PanelVisibility) (layout : PanelLayout) : PanelLayout =
    { layout with
        panels =
            layout.panels
            |> List.map (fun p -> if p.panel = panel then { p with visible = visible } else p) }

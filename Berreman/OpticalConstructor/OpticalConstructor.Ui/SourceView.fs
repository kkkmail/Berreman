/// Sources panel view (spec 0024 Part U4 / R-1..R-2, AC-U4.1 / AC-U4.2). Edits the
/// light source and its polarization with a live Stokes / Poincaré / ellipse readout.
/// The editor reducer, the polarization presets, and the live projections all already
/// exist and are frozen (§0.1); this slice renders the fields and hosts the ellipse in
/// the slice-002 `AvaPlot` adapter and the Poincaré sphere in the slice-002 WebView2
/// adapter (`ChartHosts.scottPlotHost` / `ChartHosts.webView2Host`).
///
/// Like the slice-002 `ChartView.chartPanel`, the slice-003 `ConstructionView.stackPanel`,
/// and the slice-004 `MaterialsView.materialsPanel`, the panel takes its sub-state (the
/// edited `SourceSpec`) plus a dispatch — NOT `RootModel`, which lives in `Shell.fs` — so
/// it composes under the root without a module cycle. `Shell` passes
/// `RootMsg.Source >> dispatch`.
///
/// This view re-implements NO source reducer and NO polarization projection: a field edit
/// routes the existing `SourceEditorView.update` (R-1) and a preset routes the existing
/// `PolarizationPicker.applyPreset` (R-2); the live readout reads
/// `PolarizationPicker.liveStokes` / `poincareMarker` / `ellipseParameters`, and the plots
/// are built by `PolarizationPlots.renderEllipse` / `poincareSphere`.
///
/// New sibling `*View.fs` module per §0.1; authored against the public MIT
/// `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no clone reference.
module OpticalConstructor.Ui.SourceView

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.SourceSpec
open OpticalConstructor.Ui.Sources
open OpticalConstructor.Ui.Charts

// ---------------------------------------------------------------------------
// The sources panel's local message + pure dispatcher (R-1 / R-2). Pure and
// serializable (§0.5): a `SourceSpec` carries only plain engine values, no Avalonia
// handle, renderer instance, or token source. `Shell` holds the `SourceSpec` and wraps
// `SourceViewMsg` in `RootMsg.Source`.
//
// Field edits ARE `SourceEditorView.SourceMsg` (R-1). The polarization presets are a
// SEPARATE seam (`PolarizationPicker.applyPreset`, R-2) that sets `coherence`
// (`Unpolarized`) — which NO `SourceMsg` case can express — so they cannot be folded
// into `SourceMsg`. Hence this thin local union over the two seams, dispatched through
// the single `Source` `RootMsg` case (the same shape as the materials panel's
// view-local `MaterialsMsg`). `Edit` routes `SourceEditorView.update`; `ApplyPreset`
// routes `applyPreset`.
// ---------------------------------------------------------------------------

/// A sources-panel edit: a field edit (routed through the frozen
/// `SourceEditorView.update`, R-1) or a polarization preset (routed through the frozen
/// `PolarizationPicker.applyPreset`, R-2).
type SourceViewMsg =
    | Edit of SourceEditorView.SourceMsg
    | ApplyPreset of PolarizationPicker.PolarizationPreset

/// Pure dispatcher over the edited source. The view re-implements neither the source
/// reducer nor the presets: it delegates to the existing frozen seams (R-1 / R-2).
let update (msg : SourceViewMsg) (s : SourceSpec) : SourceSpec =
    match msg with
    | Edit m -> SourceEditorView.update m s
    | ApplyPreset p -> PolarizationPicker.applyPreset p s

// ---------------------------------------------------------------------------
// Small parse/format helpers for the numeric fields. Boundary unit conversion stays in
// the frozen `Units` seam (`SourceEditorView.update`'s `SetWaveLength` does the
// conversion); this module only reads the display value back through the same seam.
// ---------------------------------------------------------------------------

/// Parse a human-entered scalar (invariant culture); `None` for blank/garbage so a
/// partial edit dispatches nothing rather than corrupting the model.
let private tryFloat (text : string) : float option =
    match System.Double.TryParse(
            text,
            System.Globalization.NumberStyles.Float ||| System.Globalization.NumberStyles.AllowThousands,
            System.Globalization.CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | _ -> None

let private radToDeg (r : float) : float = r * 180.0 / System.Math.PI

// ---------------------------------------------------------------------------
// View — field editors (R-1).
// ---------------------------------------------------------------------------

/// A labelled numeric TextBox: parses the entry and dispatches `onChanged` only when it
/// parses, so a mid-edit blank never corrupts the model.
let private numberField (label : string) (value : float) (onChanged : float -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text label; TextBlock.width 130.0; TextBlock.verticalAlignment VerticalAlignment.Center ]
            TextBox.create [
                TextBox.width 120.0
                TextBox.text (sprintf "%g" value)
                TextBox.onTextChanged (fun t -> match tryFloat t with | Some v -> onChanged v | None -> ())
            ]
        ]
    ] :> IView

/// A display-unit selector button: dispatches `SetDisplayUnit` (R-1). The chosen unit
/// is the one the wavelength entry is interpreted in.
let private unitButton (dispatch : SourceViewMsg -> unit) (label : string) (u : UnitOfMeasure) (active : UnitOfMeasure) : IView =
    Button.create [
        Button.content label
        Button.background (if u = active then Brushes.SteelBlue :> IBrush else Brushes.Transparent :> IBrush)
        Button.onClick (fun _ -> dispatch (Edit (SourceEditorView.SetDisplayUnit u)))
    ] :> IView

/// The source-field editors (R-1): wavelength (in the chosen display unit), incidence
/// angle (degrees), polarization azimuth (degrees), ellipticity, and per-source
/// intensity, plus the display-unit selector. Each field dispatches the matching
/// `SourceEditorView.SourceMsg` wrapped as `Edit …`, routed through the frozen
/// `SourceEditorView.update`.
let private fieldEditors (s : SourceSpec) (dispatch : SourceViewMsg -> unit) : IView =
    let wavelengthDisplay = wavelengthToUnit s.displayUnit s.light.waveLength
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.margin 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text "Source"; TextBlock.fontWeight FontWeight.Bold ]
            numberField "Wavelength:" wavelengthDisplay (fun v -> dispatch (Edit (SourceEditorView.SetWaveLength (s.displayUnit, v))))
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 4.0
                StackPanel.children [
                    TextBlock.create [ TextBlock.text "Unit:"; TextBlock.width 130.0; TextBlock.verticalAlignment VerticalAlignment.Center ]
                    unitButton dispatch "nm" Nanometer s.displayUnit
                    unitButton dispatch "µm" Micrometer s.displayUnit
                    unitButton dispatch "Å" Angstrom s.displayUnit
                    unitButton dispatch "eV" ElectronVolt s.displayUnit
                    unitButton dispatch "cm⁻¹" Wavenumber s.displayUnit
                ]
            ]
            numberField "Incidence angle (°):" (radToDeg s.light.incidenceAngle.value)
                (fun d -> dispatch (Edit (SourceEditorView.SetIncidenceAngleDegrees d)))
            numberField "Polarization (°):" (radToDeg s.light.polarization.value)
                (fun d -> dispatch (Edit (SourceEditorView.SetPolarization (Polarization.create (Angle.degree d)))))
            numberField "Ellipticity:" s.light.ellipticity.value
                (fun e -> dispatch (Edit (SourceEditorView.SetEllipticity e)))
            numberField "Intensity:" s.intensity
                (fun i -> dispatch (Edit (SourceEditorView.SetIntensity i)))
        ]
    ] :> IView

// ---------------------------------------------------------------------------
// View — polarization presets (R-2).
// ---------------------------------------------------------------------------

/// A polarization-preset button: dispatches `ApplyPreset …`, routed through the frozen
/// `PolarizationPicker.applyPreset` (R-2).
let private presetButton (dispatch : SourceViewMsg -> unit) (label : string) (preset : PolarizationPicker.PolarizationPreset) : IView =
    Button.create [
        Button.content label
        Button.onClick (fun _ -> dispatch (ApplyPreset preset))
    ] :> IView

/// The polarization preset row (R-2): s, p, 45°, RCP, LCP, unpolarized — each routes
/// `applyPreset`.
let private presetBar (dispatch : SourceViewMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.margin 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text "Polarization presets"; TextBlock.fontWeight FontWeight.Bold ]
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 4.0
                StackPanel.children [
                    presetButton dispatch "s" PolarizationPicker.PresetS
                    presetButton dispatch "p" PolarizationPicker.PresetP
                    presetButton dispatch "45" PolarizationPicker.Preset45
                    presetButton dispatch "RCP" PolarizationPicker.PresetRCP
                    presetButton dispatch "LCP" PolarizationPicker.PresetLCP
                    presetButton dispatch "Unpolarized" PolarizationPicker.PresetUnpolarized
                ]
            ]
        ]
    ] :> IView

// ---------------------------------------------------------------------------
// View — live readout (R-2): the Stokes vector, the Poincaré marker, and the ellipse
// parameters, all read from the frozen `PolarizationPicker` projections (never
// hand-derived here).
// ---------------------------------------------------------------------------

/// The live Stokes / Poincaré / ellipse text readout (R-2). Reads
/// `PolarizationPicker.liveStokes` / `poincareMarker` / `ellipseParameters` — the picker
/// projections — so it refreshes from the current source on every render (AC-U4.1).
let private liveReadout (s : SourceSpec) : IView =
    let (StokesVector v) = PolarizationPicker.liveStokes s
    let (s1, s2, s3) = PolarizationPicker.poincareMarker s
    let ell = PolarizationPicker.ellipseParameters s
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.margin 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text "Live readout"; TextBlock.fontWeight FontWeight.Bold ]
            TextBlock.create [ TextBlock.text (sprintf "Stokes: S0=%.3f  S1=%.3f  S2=%.3f  S3=%.3f" v.[0] v.[1] v.[2] v.[3]) ]
            TextBlock.create [ TextBlock.text (sprintf "Poincaré: (%.3f, %.3f, %.3f)" s1 s2 s3) ]
            TextBlock.create [ TextBlock.text (sprintf "Ellipse: azimuth=%.3f rad  axial ratio=%.3f" ell.azimuth ell.axialRatio) ]
        ]
    ] :> IView

// ---------------------------------------------------------------------------
// View — the two hosted plots (R-2). Ellipse → AvaPlot (ScottPlot); Poincaré → WebView2
// (Plotly). The mapping is fixed by the spec (Non-requirements forbid the swap). Both
// route through the slice-002 `ChartHosts` adapters, never embedding their own hosting.
// ---------------------------------------------------------------------------

/// The polarization ellipse hosted in the `AvaPlot` adapter (R-2 / AC-U4.2). The
/// `ScottPlot.Plot` is built by the frozen `PolarizationPlots.renderEllipse` from the
/// `ellipseParameters` projection (signed axial ratio + azimuth), rebuilt each render so
/// a source edit reaches the plot; `scottPlotHost` calls `Refresh()` and degrades to the
/// §U1.8 placeholder if no native graphics surface is available.
let private ellipseHost (s : SourceSpec) : IView =
    let ell = PolarizationPicker.ellipseParameters s
    let plot = PolarizationPlots.renderEllipse ChartSettings.ChartSettings.defaultValue ell.axialRatio ell.azimuth
    ChartHosts.scottPlotHost plot

/// The Poincaré sphere hosted in the WebView2 adapter (R-2 / AC-U4.2). The
/// `Plotly.NET.GenericChart` is built lazily by the frozen `PolarizationPlots.poincareSphere`
/// from the live Stokes vector, so the placeholder path never pays to build it; headlessly
/// (and until the deferred WebView2 bridge lands) `webView2Host` degrades to the §U1.8
/// placeholder. NEVER an `AvaPlot` (Non-requirements).
let private poincareHost (s : SourceSpec) : IView =
    let chart : Lazy<Plotly.NET.GenericChart> = lazy (PolarizationPlots.poincareSphere 24 (PolarizationPicker.liveStokes s))
    ChartHosts.webView2Host chart

/// One titled, fixed-height plot row so each hosted plot lays out a frame.
let private plotRow (title : string) (host : IView) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.margin 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text title; TextBlock.fontWeight FontWeight.Bold ]
            Border.create [ Border.height 220.0; Border.child host ]
        ]
    ] :> IView

// ---------------------------------------------------------------------------
// The sources panel (R-1 / R-2). Field editors + preset row + live readout, then the
// ellipse (AvaPlot) and Poincaré (WebView2) plot rows. Scrollable so each section lays
// out independently of the panel height.
// ---------------------------------------------------------------------------

let sourcePanel (s : SourceSpec) (dispatch : SourceViewMsg -> unit) : IView =
    let body =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.children [
                fieldEditors s dispatch
                presetBar dispatch
                liveReadout s
                plotRow "Polarization ellipse" (ellipseHost s)
                plotRow "Poincaré sphere" (poincareHost s)
            ]
        ]
    ScrollViewer.create [ ScrollViewer.content body ] :> IView

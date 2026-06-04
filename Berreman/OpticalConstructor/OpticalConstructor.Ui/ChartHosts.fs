/// Renderer-host adapters (spec 0024 Part U5 / R-1, §U1.8, §0.5). The single shared
/// seam every renderer-bearing panel reuses: U3 (materials dispersion preview), U4
/// (polarization plots), U6 (multi-system overlay / schematic), and U8 (3-D system
/// view) MUST host their `ScottPlot.Plot` / `Plotly.NET.GenericChart` / 2-D geometry
/// through these adapters rather than embedding their own hosting.
///
/// Each adapter:
///   * hosts its control via `ContentControl.create` (R-1);
///   * keeps the live control instance in the view/host layer — a local created here,
///     never a field of the root model (§0.5);
///   * degrades to a graceful "unavailable" placeholder rather than throwing when its
///     native runtime is absent (§U1.8), so the headless `ui-smoke` gate renders a
///     frame for every panel without throwing.
///
/// New sibling `*Hosts.fs` view module per §0.1 (no frozen module edited). Authored
/// against the public MIT `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no clone
/// reference.
module OpticalConstructor.Ui.ChartHosts

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The §U1.8 "unavailable" placeholder a native-host adapter renders when its runtime
/// (the headless platform, an absent WebView2 Evergreen runtime, a GL surface) cannot
/// host the live control. A plain bordered note so the panel still lays out one frame.
let unavailable (what : string) : IView =
    Border.create [
        Border.borderThickness 1.0
        Border.borderBrush (Brushes.Gray :> IBrush)
        Border.padding 8.0
        Border.minHeight 80.0
        Border.child (
            TextBlock.create [
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.horizontalAlignment HorizontalAlignment.Center
                TextBlock.foreground (Brushes.Gray :> IBrush)
                TextBlock.text (what + " — renderer unavailable")
            ])
    ] :> IView

/// Wrap a successfully-created live control as panel content (R-1 — host via
/// `ContentControl.create`). The control is the host-layer local the caller built.
let private hostControl (control : Control) : IView =
    ContentControl.create [ ContentControl.content control ] :> IView

// ---------------------------------------------------------------------------
// (a) AvaPlot host for a ScottPlot.Plot (R-1 / R-2 / R-4 consumers).
// ---------------------------------------------------------------------------

/// Host a `ScottPlot.Plot` in a `ScottPlot.Avalonia.AvaPlot` (an Avalonia `Control`),
/// assigning the plot and calling `Refresh()` so the rendered frame reflects the
/// current plot (AC-U5.1 — `Refresh()` on model change, since the caller rebuilds the
/// plot from the current settings on each render). The `AvaPlot` instance is the
/// host-layer local (§0.5). If the control cannot be constructed/refreshed (e.g. no
/// native graphics on the headless platform), the adapter degrades to the §U1.8
/// placeholder.
let scottPlotHost (plot : ScottPlot.Plot) : IView =
    try
        let ava = new ScottPlot.Avalonia.AvaPlot()
        // `AvaPlot.Plot` has no public setter; `Reset(Plot)` swaps in our pre-built
        // plot (the ScottPlot 5 `IPlotControl` plot-replacement seam).
        ava.Reset(plot)
        ava.Refresh()
        hostControl (ava :> Control)
    with _ ->
        unavailable "Plot"

// ---------------------------------------------------------------------------
// (b) WebView2 host for a Plotly.NET.GenericChart (R-1 / R-4 / U3 / U4 consumers).
// ---------------------------------------------------------------------------

/// Attempt to host a Plotly chart by navigating a WebView2 to the chart's embedded
/// HTML (`GenericChart.toEmbeddedHTML` → `CoreWebView2.NavigateToString`, R-1).
///
/// There is no Avalonia-hostable WebView2 binding on this project's `net10.0` target:
/// the `Microsoft.Web.WebView2` package ships its control assemblies for `net462` /
/// Windows-desktop TFMs only and contributes no compile-time reference here (verified
/// empirically — its `compile` asset set is empty under `net10.0`), and the headless
/// platform carries no Evergreen runtime. So this returns `None` today and the adapter
/// degrades to the §U1.8 placeholder (the same graceful path AC-U3.1 names for an
/// absent WebView2). The `toEmbeddedHTML` → `NavigateToString` wiring lands unchanged
/// once a Windows-desktop `NativeControlHost` WebView bridge is added (deferred; the
/// app targets win-x64).
///
/// The chart is taken lazily so the placeholder path never pays for building it
/// (a Plotly 3-D surface forces an upstream `calculate3D` sweep): the value is forced
/// ONLY on the real host path, which today is unreachable on this TFM.
let private tryHostPlotly (_chart : Lazy<Plotly.NET.GenericChart>) : Control option =
    // When a WebView2 binding exists (the deferred Windows-desktop bridge), this is:
    //   let html = GenericChart.toEmbeddedHTML _chart.Value
    //   let webView = ... in webView.CoreWebView2.NavigateToString html ; Some webView
    None

/// Host a `Plotly.NET.GenericChart` in the WebView2 adapter, degrading to the §U1.8
/// placeholder when no WebView2 binding/runtime is available (see `tryHostPlotly`).
/// The chart is passed lazily; it is built only if a host is actually available.
let webView2Host (chart : Lazy<Plotly.NET.GenericChart>) : IView =
    match tryHostPlotly chart with
    | Some control -> hostControl control
    | None -> unavailable "3-D / Plotly chart (WebView2)"

// ---------------------------------------------------------------------------
// (c) Canvas host for 2-D geometry (R-1 / U6 schematic / U8 system view consumers).
// ---------------------------------------------------------------------------

/// Host 2-D geometry on a public-Avalonia `Canvas` (R-1 / §0.3). `Canvas` is a pure
/// Avalonia primitive with no native runtime, so this adapter never degrades; the
/// children are the host-layer geometry the caller (U6/U8) supplies.
let canvasHost (children : IView list) : IView =
    Canvas.create [ Canvas.children children ] :> IView

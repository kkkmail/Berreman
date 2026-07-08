namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls.ExperimentChart

/// Spec 0035 (016) — the inline / embeddable form of the shared dual-axis ScottPlot chart. The Material
/// editor's live n/k preview and the Materials-bay View panel embed THIS (replacing the deleted primitive
/// `NkDispersionChart.inlineCanvas`), so a dispersion curve there IS the experiment chart: a
/// `ScottPlot.Avalonia.AvaPlot` whose `Plot` is built through the ONE shared `ChartPlot.renderCartesian`
/// rebuild path (n on the LEFT axis, k on the RIGHT via the caller's paired `ChartStyle` seed). Hosted in a
/// `ContentControl` carrying the caller's stable automation id (the `ChartHosts.scottPlotHost` seam), inside
/// a `try`/`with` that degrades to a bordered "renderer unavailable" placeholder so a native-render failure
/// never throws under the headless `ui-smoke` frame. Public MIT Avalonia + FuncUI + ScottPlot only.
[<RequireQualifiedAccess>]
module EmbeddedChart =

    /// Carry the host's stable automation id on the `ContentControl` (a freely-mutable attached property,
    /// found by `AutomationProperties.GetAutomationId` — the id the two host sites and their tests key on).
    let private automationId (autoId : string) : IAttr<ContentControl> =
        AttrBuilder<ContentControl>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

    /// The graceful "renderer unavailable" placeholder when native ScottPlot cannot host / rasterize the
    /// plot (the `ChartHosts.unavailable` §U1.8 shape), so the panel still lays out one frame headlessly.
    let private unavailable () : Control =
        let text =
            TextBlock(
                Text = "n/k chart — renderer unavailable",
                VerticalAlignment = VerticalAlignment.Center,
                HorizontalAlignment = HorizontalAlignment.Center,
                Foreground = (Brushes.Gray :> IBrush))
        Border(
            BorderThickness = Thickness(1.0),
            BorderBrush = (Brushes.Gray :> IBrush),
            Padding = Thickness(8.0),
            MinHeight = 80.0,
            Child = (text :> Control)) :> Control

    /// The embeddable dual-axis n/k chart: build a live `AvaPlot` through the shared `ChartPlot` rebuild
    /// path over `chart` + `style` (n-left / k-right), hosted under `autoId`. Degrades to the placeholder on
    /// a native-render failure so the headless gate stays green.
    let create (autoId : string) (chart : ExperimentChart) (style : ChartStyle.ChartStyleState) : IView =
        let content : Control =
            try
                let ava = new ScottPlot.Avalonia.AvaPlot()
                ChartPlot.renderCartesian ava.Plot chart style |> ignore
                ava.Refresh()
                ava :> Control
            with _ ->
                unavailable ()
        ContentControl.create [
            automationId autoId
            ContentControl.content content
        ] :> IView

namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable EXPERIMENTS control (Spec 0027 — the "Experiments" bay). The user picks WHICH element
/// currently present on the table is the swept element, AND which 1-D experiment to run on it: rotate its
/// R1 a full circle (the rotating-analyzer / Malus measurement), sweep its R2 (incidence) 0…90°, or sweep
/// the wavelength over an editable range. A readout shows the built experiment and an inline chart shows
/// the result (one intensity series, or two Ψ/Δ series for an ellipsometer detector).
/// This control is DOMAIN-FREE (matching `RendererControls` / `LibraryControls`): the host pre-flattens the
/// present elements into selectable `SweepCandidate`s, pre-builds the chart series + labels + description,
/// and injects the behaviour as a `Handlers` function record (the functional-proxy seam; tests pass stubs).
module ExperimentControls =

    /// One present, sweepable table element offered to the user (the host flattens the live scene into
    /// these; Controls carries no domain types). `elementId` is the element's serializable id; `label`
    /// is a human-readable display (e.g. "Linear polarizer #2").
    type SweepCandidate =
        {
            elementId : string
            label : string
        }

    /// The experiment KIND the user picks — a domain-free mirror of the three `Experiment` cases (a DU, not
    /// an enum). The host maps it back to the domain `Experiment` for the chosen element.
    type ExperimentKindChoice =
        | RotateR1
        | SweepR2
        | SweepLambda

    /// One drawn chart series: a display name (legend) and its (x, y) points in axis-display units.
    type ChartSeries =
        {
            name : string
            points : (float * float) list
        }

    /// The bay's pure, serializable state.
    type State =
        {
            /// The present elements, each offered as a candidate swept element.
            candidates : SweepCandidate list
            /// The currently-chosen swept element's id (the one whose R1 sweeps the full circle), if any.
            chosenId : string option
            /// The built experiment's readout text ("Experiment: rotate <label> R1 0…360°" or empty).
            experimentName : string
            /// `false` when there are no elements to sweep.
            enabled : bool
            /// Spec 0027 (026): the chosen experiment kind (rotate R1 / sweep R2 / sweep λ).
            kind : ExperimentKindChoice
            /// Spec 0027 (026): the editable wavelength range (nm), shown only for the SweepLambda kind.
            lambdaLoNm : float
            lambdaHiNm : float
            /// Spec 0027 (026): the inline chart's series (one intensity series, or two Ψ/Δ series), drawn
            /// as polylines. Empty when there is no run to show.
            series : ChartSeries list
            /// Axis labels for the inline chart (and the pop-out window).
            xLabel : string
            yLabel : string
            /// A prose description of what the chart is about (the bound elements + what is swept).
            description : string
            /// The ellipsometer Ψ/Δ single-point readout (in DEGREES) for the RotateR1 kind; `None` for an
            /// intensity detector or a sweep that produces series instead.
            psiDelta : (float * float) option
        }

    let empty : State =
        {
            candidates = []
            chosenId = None
            experimentName = ""
            enabled = false
            kind = RotateR1
            lambdaLoNm = 200.0
            lambdaHiNm = 800.0
            series = []
            xLabel = ""
            yLabel = ""
            description = ""
            psiDelta = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs).
    type Handlers =
        {
            /// Pick which present element's R1 sweeps the full circle (by its id).
            chooseSwept : string -> unit
            /// Pick the experiment kind (rotate R1 / sweep R2 / sweep λ).
            chooseKind : ExperimentKindChoice -> unit
            /// Set the wavelength-range minimum (nm).
            setLambdaLo : float -> unit
            /// Set the wavelength-range maximum (nm).
            setLambdaHi : float -> unit
            /// Open the pop-out interactive chart window (double-click on the inline chart).
            openChartWindow : unit -> unit
        }

    /// Stable automation ids (CLAUDE.md UI guidance).
    [<RequireQualifiedAccess>]
    module UiIds =
        let readout = "ExperimentReadout"
        let candidates = "ExperimentCandidates"
        /// A candidate's clickable id — the element id, prefixed so it cannot collide with other ids.
        let candidate (elementId : string) : string = "ExperimentCandidate_" + elementId
        /// The inline result polyline (the first series — the intensity / Ψ curve).
        let chart = "ExperimentChart"
        /// The ellipsometer Ψ/Δ single-point readout text.
        let psiDelta = "EllipsometerReadout"
        /// The three experiment-kind selector borders.
        let kindRotateR1 = "ExperimentKindRotateR1"
        let kindSweepR2 = "ExperimentKindSweepR2"
        let kindSweepLambda = "ExperimentKindSweepLambda"
        /// The wavelength-range inputs (nm).
        let lambdaLo = "ExperimentLambdaLo"
        let lambdaHi = "ExperimentLambdaHi"
        /// The chart description text shown under the inline chart.
        let description = "ExperimentChartDescription"

    // -- The button look, identical to the other bars' idle button (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private chosenBackground = color 150 185 235
    let private idleBorder = color 120 120 120

    /// A clickable candidate (a styled, named Border), highlighted when it is the chosen swept element.
    let private candidateBox (id : string) (label : string) (chosen : bool) (enabled : bool) (onClick : unit -> unit) : IView =
        Border.create [
            Border.name id
            Border.isEnabled enabled
            Border.opacity (if enabled then 1.0 else 0.4)
            Border.background (brush (if chosen then chosenBackground else idleBackground))
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 6.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label ])
            // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the
            // chosen flag flips so a reused box can't keep a stale handler.
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box chosen))
        ] :> IView

    /// A clickable kind selector (the same styled Border look), highlighted when it is the chosen kind.
    let private kindBox (id : string) (label : string) (chosen : bool) (enabled : bool) (onClick : unit -> unit) : IView =
        candidateBox id label chosen enabled onClick

    // -- The inline result area: axes, major gridlines, multi-series polylines, and a description. --
    let private chartWidth = 240.0
    let private chartHeight = 120.0
    let private chartMarginLeft = 30.0
    let private chartMarginBottom = 18.0
    let private chartMarginTop = 6.0
    let private chartMarginRight = 8.0
    let private seriesColors =
        [| color 30 90 200; color 220 120 20; color 40 150 70 |]

    let private plotWidth = chartWidth - chartMarginLeft - chartMarginRight
    let private plotHeight = chartHeight - chartMarginTop - chartMarginBottom

    /// The combined (x, y) data range over all series (falls back to a unit box when empty / degenerate).
    let private dataRange (series : ChartSeries list) : (float * float) * (float * float) =
        let pts = series |> List.collect (fun s -> s.points)
        match pts with
        | [] -> (0.0, 1.0), (0.0, 1.0)
        | _ ->
            let xs = pts |> List.map fst
            let ys = pts |> List.map snd
            let xlo, xhi = List.min xs, List.max xs
            let ylo, yhi = List.min ys, List.max ys
            let fixSpan lo hi = if hi - lo > 1e-12 then lo, hi else lo - 0.5, hi + 0.5
            fixSpan xlo xhi, fixSpan ylo yhi

    /// Map a data point to the inner plot rectangle (screen y-down).
    let private toPlot ((xlo, xhi) : float * float) ((ylo, yhi) : float * float) (x : float) (y : float) : Point =
        let px = chartMarginLeft + plotWidth * (x - xlo) / (xhi - xlo)
        let py = chartMarginTop + plotHeight * (1.0 - (y - ylo) / (yhi - ylo))
        Point(px, py)

    /// The L-shaped axis lines (bottom + left).
    let private axisLines () : IView list =
        let line (x1, y1) (x2, y2) : IView =
            Line.create [
                Line.startPoint (Point(x1, y1))
                Line.endPoint (Point(x2, y2))
                Line.stroke (brush idleBorder)
                Line.strokeThickness 1.0
            ] :> IView
        [
            line (chartMarginLeft, chartMarginTop) (chartMarginLeft, chartMarginTop + plotHeight)             // left
            line (chartMarginLeft, chartMarginTop + plotHeight) (chartMarginLeft + plotWidth, chartMarginTop + plotHeight)  // bottom
        ]

    /// Light-grey major gridlines (4 across, 3 up) inside the plot rectangle.
    let private gridLines () : IView list =
        let grid = brush (color 220 220 220)
        let vert =
            [ 1 .. 4 ]
            |> List.map (fun k ->
                let px = chartMarginLeft + plotWidth * float k / 5.0
                Line.create [
                    Line.startPoint (Point(px, chartMarginTop))
                    Line.endPoint (Point(px, chartMarginTop + plotHeight))
                    Line.stroke grid
                    Line.strokeThickness 0.5
                ] :> IView)
        let horiz =
            [ 1 .. 3 ]
            |> List.map (fun k ->
                let py = chartMarginTop + plotHeight * float k / 4.0
                Line.create [
                    Line.startPoint (Point(chartMarginLeft, py))
                    Line.endPoint (Point(chartMarginLeft + plotWidth, py))
                    Line.stroke grid
                    Line.strokeThickness 0.5
                ] :> IView)
        vert @ horiz

    /// One series as a coloured polyline; the FIRST series carries the stable chart automation id.
    let private seriesPolyline (xr : float * float) (yr : float * float) (index : int) (s : ChartSeries) : IView =
        let pts = s.points |> List.map (fun (x, y) -> toPlot xr yr x y)
        // The FIRST series carries the stable chart automation id; the rest reuse the same name (it locates
        // the chart group; the named-id lookup matches the first such polyline).
        Polyline.create [
            Polyline.name UiIds.chart
            Polyline.points pts
            Polyline.stroke (brush seriesColors.[index % seriesColors.Length])
            Polyline.strokeThickness 1.5
        ] :> IView

    /// The axis labels (x centred under the bottom axis, y at the top-left).
    let private axisLabels (xLabel : string) (yLabel : string) : IView list =
        [
            (if xLabel = "" then None
             else
                 Some (
                     TextBlock.create [
                         TextBlock.text xLabel
                         TextBlock.fontSize 9.0
                         TextBlock.left chartMarginLeft
                         TextBlock.top (chartMarginTop + plotHeight + 4.0)
                         TextBlock.width plotWidth
                         TextBlock.textAlignment TextAlignment.Center
                     ] :> IView))
            (if yLabel = "" then None
             else
                 Some (
                     TextBlock.create [
                         TextBlock.text yLabel
                         TextBlock.fontSize 9.0
                         TextBlock.left 1.0
                         TextBlock.top 0.0
                     ] :> IView))
        ]
        |> List.choose id

    /// The inline chart Canvas (axes + gridlines + series polylines + axis labels).
    let private chartCanvas (state : State) : IView =
        let xr, yr = dataRange state.series
        let children =
            gridLines ()
            @ axisLines ()
            @ (state.series |> List.mapi (fun i s -> seriesPolyline xr yr i s))
            @ axisLabels state.xLabel state.yLabel
        Canvas.create [
            Canvas.width chartWidth
            Canvas.height chartHeight
            Canvas.children children
        ] :> IView

    /// The result block under the candidates: the ellipsometer single-point Ψ/Δ text when present, plus the
    /// inline chart (double-click opens the pop-out window) and the prose description.
    let private resultBlock (state : State) (handlers : Handlers) : IView list =
        let psiBlock =
            match state.psiDelta with
            | Some (psiDeg, deltaDeg) ->
                [ TextBlock.create [
                      TextBlock.name UiIds.psiDelta
                      TextBlock.text (sprintf "Ψ = %.2f°   Δ = %.2f°" psiDeg deltaDeg)
                  ] :> IView ]
            | None -> []
        let chartBlock =
            match state.series with
            | [] -> []
            | _ ->
                [
                    Border.create [
                        Border.borderBrush (brush idleBorder)
                        Border.borderThickness 1.0
                        Border.background (brush (color 248 248 248))
                        Border.width chartWidth
                        Border.height chartHeight
                        Border.horizontalAlignment HorizontalAlignment.Left
                        Border.child (chartCanvas state)
                        // Double-click → open the pop-out interactive chart window (Part 3). `ClickCount = 2`
                        // is the second press of a double-click; `e.Handled <- true` drops the duplicate pass.
                        Border.onPointerPressed (
                            (fun e ->
                                e.Handled <- true
                                if e.ClickCount >= 2 then handlers.openChartWindow ()),
                            SubPatchOptions.Always)
                    ] :> IView
                    TextBlock.create [
                        TextBlock.name UiIds.description
                        TextBlock.text state.description
                        TextBlock.fontSize 11.0
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.maxWidth 360.0
                    ] :> IView
                ]
        psiBlock @ chartBlock

    /// The kind selector row (rotate R1 / sweep R2 / sweep λ).
    let private kindRow (state : State) (handlers : Handlers) : IView =
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children [
                kindBox UiIds.kindRotateR1 "Rotate R1" (state.kind = RotateR1) state.enabled (fun () -> handlers.chooseKind RotateR1)
                kindBox UiIds.kindSweepR2 "Sweep R2" (state.kind = SweepR2) state.enabled (fun () -> handlers.chooseKind SweepR2)
                kindBox UiIds.kindSweepLambda "Sweep λ" (state.kind = SweepLambda) state.enabled (fun () -> handlers.chooseKind SweepLambda)
            ]
        ] :> IView

    /// Parse a wavelength text (nm), accepting a plain decimal (invariant culture).
    let private parseNm (text : string) : float option =
        match System.Double.TryParse(text, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
        | true, v when v > 0.0 -> Some v
        | _ -> None

    /// A wavelength text field (nm); commits on Enter / blur, like the rotation bar's angle field.
    let private lambdaField (id : string) (value : float) (enabled : bool) (onCommit : float -> unit) : IView =
        let commit (src : obj) =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> parseNm tb.Text |> Option.iter onCommit
            | _ -> ()
        TextBox.create [
            TextBox.name id
            TextBox.width 80.0
            TextBox.isEnabled enabled
            TextBox.text (sprintf "%g" value)
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commit e.Source)
            TextBox.onLostFocus (fun e -> commit e.Source)
        ] :> IView

    /// The wavelength-range inputs (nm), shown only for the SweepLambda kind.
    let private lambdaRow (state : State) (handlers : Handlers) : IView list =
        match state.kind with
        | SweepLambda ->
            [
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing 6.0
                    StackPanel.children [
                        TextBlock.create [ TextBlock.text "λ min (nm):"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                        lambdaField UiIds.lambdaLo state.lambdaLoNm state.enabled handlers.setLambdaLo
                        TextBlock.create [ TextBlock.text "λ max (nm):"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                        lambdaField UiIds.lambdaHi state.lambdaHiNm state.enabled handlers.setLambdaHi
                    ]
                ] :> IView
            ]
        | RotateR1 | SweepR2 -> []

    /// The Experiments bay — a wrapping row of candidate elements (highlighted when chosen), an experiment-
    /// kind selector, the wavelength-range inputs (for the λ sweep), a readout of the built experiment, and
    /// the inline chart (axes + gridlines + series) with its description.
    let view (state : State) (handlers : Handlers) : IView =
        let readoutText =
            if state.experimentName = "" then "Experiment: (choose the element to sweep and the kind)"
            else state.experimentName
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                [
                    TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.text readoutText ] :> IView
                    WrapPanel.create [
                        WrapPanel.name UiIds.candidates
                        WrapPanel.orientation Orientation.Horizontal
                        WrapPanel.children (
                            state.candidates
                            |> List.map (fun c ->
                                candidateBox
                                    (UiIds.candidate c.elementId)
                                    c.label
                                    (state.chosenId = Some c.elementId)
                                    state.enabled
                                    (fun () -> handlers.chooseSwept c.elementId)))
                    ] :> IView
                    kindRow state handlers
                ]
                @ lambdaRow state handlers
                @ resultBlock state handlers)
        ] :> IView

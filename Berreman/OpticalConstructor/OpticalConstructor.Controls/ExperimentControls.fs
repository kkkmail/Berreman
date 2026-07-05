namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable EXPERIMENTS control (Spec 0027 / 028 — the "Experiments" bay), redesigned as a multi-step,
/// EDITABLE experiment builder over the live setup:
///   1. the SETUP is the scene (the present elements + their bound materials / geometry);
///   2. the user picks WHICH element to vary — and the element determines what CAN be varied (the host
///      supplies the allowed `VariableChoice`s per the element's kind; the bay never hard-codes them);
///   3. the user picks what to CAPTURE (transmitted / reflected / both), sets the numeric range, and clicks
///      Add — which appends the experiment to a COLLECTION that persists. Existing experiments can be
///      Edited (re-adding updates in place, never duplicates) and Removed.
/// This control is DOMAIN-FREE (matching `LibraryControls` / `RendererControls`): the host flattens the
/// present elements into `SweepCandidate`s, the allowed variables into `VariableChoice`s, the collection
/// into `ExperimentRow`s, pre-builds the inline chart series + labels + description, and injects the
/// behaviour as a `Handlers` function record (the functional-proxy seam; tests pass stubs).
module ExperimentControls =

    /// One present, varyable table element offered to the user (the host flattens the live scene into
    /// these; Controls carries no domain types). `elementId` is the element's serializable id; `label`
    /// is a human-readable display (e.g. "Linear polarizer #2").
    type SweepCandidate =
        {
            elementId : string
            label : string
        }

    /// The quantity to vary — a domain-free mirror of the domain `VariableParameter` (a DU, not an enum).
    /// The host maps it back to the domain type for the chosen element.
    type VariableChoice =
        | VaryWaveLength
        | VaryR1
        | VaryR2

    /// What the experiment captures — a domain-free mirror of the domain `MeasurementMode`.
    type MeasurementChoice =
        | CaptureT
        | CaptureR
        | CaptureBoth

    /// The stable code for a variable choice (drives its automation id and the host's mapping).
    let variableCode (v : VariableChoice) : string =
        match v with
        | VaryWaveLength -> "wavelength"
        | VaryR1 -> "r1"
        | VaryR2 -> "r2"

    /// A short label for a variable choice (the selector button text).
    let variableLabel (v : VariableChoice) : string =
        match v with
        | VaryWaveLength -> "Wavelength"
        | VaryR1 -> "Rotation R1"
        | VaryR2 -> "Incidence R2"

    /// The stable code for a measurement choice (drives its automation id and the host's mapping).
    let measurementCode (m : MeasurementChoice) : string =
        match m with
        | CaptureT -> "t"
        | CaptureR -> "r"
        | CaptureBoth -> "both"

    /// A short label for a measurement choice (the selector button text).
    let measurementLabel (m : MeasurementChoice) : string =
        match m with
        | CaptureT -> "Transmitted (T)"
        | CaptureR -> "Reflected (R)"
        | CaptureBoth -> "Both (T + R)"

    /// One drawn chart series: a display name (legend) and its (x, y) points in axis-display units.
    type ChartSeries =
        {
            name : string
            points : (float * float) list
        }

    /// One added experiment shown in the collection (the host flattens the domain `Experiment`s to these).
    type ExperimentRow =
        {
            /// The experiment's stable id, rendered as a string (the host maps it back to `ExperimentId`).
            id : string
            /// A human-readable one-line description of the experiment.
            description : string
            /// Whether this experiment is the one currently loaded in the editor (highlighted).
            isEditing : bool
        }

    /// The bay's pure, serializable state.
    type State =
        {
            /// The present elements, each offered as a candidate element-to-vary.
            candidates : SweepCandidate list
            /// The currently-chosen element-to-vary (by id), if any.
            chosenId : string option
            /// The variables the CHOSEN element permits varying (host-supplied per its kind; empty when the
            /// element exposes nothing to vary).
            variableChoices : VariableChoice list
            /// The chosen variable, if any.
            chosenVariable : VariableChoice option
            /// The chosen capture mode (transmitted / reflected / both).
            measurement : MeasurementChoice
            /// The variation range (in the variable's display unit) and its point count.
            rangeMin : float
            rangeMax : float
            rangePoints : int
            /// The variable's display unit ("nm" / "°"), shown next to the range inputs.
            rangeUnitLabel : string
            /// Whether the draft can be committed (an element + a variable are chosen).
            canAdd : bool
            /// Whether the editor is UPDATING an existing experiment (the button reads "Update" not "Add").
            isEditing : bool
            /// The added experiments (the persistent collection).
            collection : ExperimentRow list
            /// The built-experiment readout text.
            readout : string
            /// `false` when there are no elements to vary.
            enabled : bool
            /// The inline chart's series (one intensity series, two Ψ/Δ series, or two T/R series for a
            /// both-capture intensity run), drawn as polylines. Empty when there is no run to show.
            series : ChartSeries list
            /// Axis labels for the inline chart (and the pop-out window).
            xLabel : string
            yLabel : string
            /// A prose description of what the chart is about (the bound elements + what is varied).
            description : string
            /// The ellipsometer Ψ/Δ single-point readout (in DEGREES) for the RotateR1 kind; `None` for an
            /// intensity detector or a sweep that produces series instead.
            psiDelta : (float * float) option
        }

    let empty : State =
        {
            candidates = []
            chosenId = None
            variableChoices = []
            chosenVariable = None
            measurement = CaptureT
            rangeMin = 0.0
            rangeMax = 360.0
            rangePoints = 73
            rangeUnitLabel = "°"
            canAdd = false
            isEditing = false
            collection = []
            readout = ""
            enabled = false
            series = []
            xLabel = ""
            yLabel = ""
            description = ""
            psiDelta = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs).
    type Handlers =
        {
            /// Pick which present element to vary (by its id).
            chooseElement : string -> unit
            /// Pick the varied quantity.
            chooseVariable : VariableChoice -> unit
            /// Pick the capture mode.
            chooseMeasurement : MeasurementChoice -> unit
            /// Set the range minimum / maximum (in the variable's display unit) and the point count.
            setRangeMin : float -> unit
            setRangeMax : float -> unit
            setRangePoints : int -> unit
            /// Confirm the draft: Add a new experiment, or Update the one being edited.
            addOrUpdate : unit -> unit
            /// Start a brand-new experiment (clears the editor).
            newExperiment : unit -> unit
            /// Load an experiment (by id) into the editor for editing.
            editExperiment : string -> unit
            /// Remove an experiment (by id) from the collection.
            removeExperiment : string -> unit
            /// Open the pop-out interactive chart window (double-click on the inline chart).
            openChartWindow : unit -> unit
        }

    /// Stable automation ids (CLAUDE.md UI guidance).
    [<RequireQualifiedAccess>]
    module UiIds =
        let readout = "ExperimentReadout"
        let candidates = "ExperimentCandidates"
        /// A candidate element's clickable id — the element id, prefixed so it cannot collide.
        let candidate (elementId : string) : string = "ExperimentCandidate_" + elementId
        /// A variable-choice selector, by its code ("wavelength" / "r1" / "r2").
        let variable (code : string) : string = "ExperimentVariable_" + code
        /// A measurement-choice selector, by its code ("t" / "r" / "both").
        let measurement (code : string) : string = "ExperimentMeasurement_" + code
        /// The range inputs.
        let rangeMin = "ExperimentRangeMin"
        let rangeMax = "ExperimentRangeMax"
        let rangePoints = "ExperimentRangePoints"
        /// The Add / Update and New actions.
        let addButton = "ExperimentAddButton"
        let newButton = "ExperimentNewButton"
        /// The collection list and a row's Edit / Remove actions.
        let collection = "ExperimentCollection"
        let editButton (id : string) : string = "ExperimentEdit_" + id
        let removeButton (id : string) : string = "ExperimentRemove_" + id
        /// The explicit "open the pop-out chart window" action (also opened by double-clicking the chart).
        let openChart = "ExperimentOpenChart"
        /// The inline result polyline (the first series — the intensity / Ψ curve).
        let chart = "ExperimentChart"
        /// The ellipsometer Ψ/Δ single-point readout text.
        let psiDelta = "EllipsometerReadout"
        /// The chart description text shown under the inline chart.
        let description = "ExperimentChartDescription"

    // -- The button look, identical to the other bars' idle button (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private chosenBackground = color 150 185 235
    let private editingBackground = color 255 224 160
    let private idleBorder = color 120 120 120

    /// A clickable, styled, named Border, highlighted when it is the chosen option. `visible` toggles the
    /// box in place (rather than adding / removing it from the list): a named Avalonia control's `Name`
    /// cannot be changed once styled, so a bounded selector whose MEMBERS change (e.g. the variable set
    /// per element kind) must keep every box present with a STABLE name and only flip visibility — otherwise
    /// FuncUI recycles a named box into a differently-named one and throws "Cannot set Name : … already
    /// styled" (the same reason the Ribbon keeps all panes present). See `variableRow`.
    let private optionBoxV (id : string) (label : string) (chosen : bool) (enabled : bool) (visible : bool) (onClick : unit -> unit) : IView =
        Border.create [
            Border.name id
            Border.isVisible visible
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
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (chosen, visible)))
        ] :> IView

    /// An always-visible clickable option box (measurements — a fixed set, so a stable `Name` is safe).
    let private optionBox (id : string) (label : string) (chosen : bool) (enabled : bool) (onClick : unit -> unit) : IView =
        optionBoxV id label chosen enabled true onClick

    /// A clickable option box for a REORDERABLE list (candidates, collection rows). Such a list shifts a
    /// control onto a different item's slot when an item is removed; Avalonia forbids changing a styled
    /// control's `Name`, so these carry an `AutomationProperties.AutomationId` (freely mutable, and the
    /// automation contract CLAUDE.md prescribes) instead of `Border.name`. Tests query it via
    /// `AutomationProperties.GetAutomationId`.
    /// Set `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike `Control.Name`)
    /// through FuncUI's attr builder, so a reused control in a reorderable list can take a new id.
    let private automationId (autoId : string) : IAttr<Border> =
        AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

    let private idOptionBox (autoId : string) (label : string) (chosen : bool) (enabled : bool) (onClick : unit -> unit) : IView =
        Border.create [
            automationId autoId
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
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (autoId, chosen)))
        ] :> IView

    /// A section heading (the numbered step labels).
    let private heading (text : string) : IView =
        TextBlock.create [ TextBlock.text text; TextBlock.fontWeight FontWeight.SemiBold; TextBlock.margin (Thickness(0.0, 2.0, 0.0, 2.0)) ] :> IView

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

    /// The result block under the editor: the ellipsometer single-point Ψ/Δ text when present, plus the
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
                    // An explicit, always-reliable way to open the pop-out interactive window — the earlier
                    // double-click-only trigger was easy to miss / not fire. (Double-click still works too.)
                    Border.create [
                        Border.name UiIds.openChart
                        Border.isEnabled state.enabled
                        Border.background (brush chosenBackground)
                        Border.borderBrush (brush idleBorder)
                        Border.borderThickness 1.0
                        Border.cornerRadius (CornerRadius 3.0)
                        Border.padding (Thickness(12.0, 5.0))
                        Border.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                        Border.horizontalAlignment HorizontalAlignment.Left
                        Border.child (TextBlock.create [ TextBlock.text "Open chart window ↗" ])
                        Border.onPointerPressed ((fun e -> e.Handled <- true; handlers.openChartWindow ()), SubPatchOptions.Always)
                    ] :> IView
                    Border.create [
                        Border.borderBrush (brush idleBorder)
                        Border.borderThickness 1.0
                        Border.background (brush (color 248 248 248))
                        Border.width chartWidth
                        Border.height chartHeight
                        Border.horizontalAlignment HorizontalAlignment.Left
                        Border.child (chartCanvas state)
                        // Double-click ALSO opens the pop-out window — via Avalonia's built-in DoubleTapped
                        // gesture (more reliable than a hand-rolled PointerPressed ClickCount check).
                        Border.onDoubleTapped ((fun e -> e.Handled <- true; handlers.openChartWindow ()), SubPatchOptions.Always)
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

    /// The element-selection row (step 1 of the editor).
    let private elementRow (state : State) (handlers : Handlers) : IView =
        WrapPanel.create [
            WrapPanel.name UiIds.candidates
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (
                state.candidates
                |> List.map (fun c ->
                    idOptionBox
                        (UiIds.candidate c.elementId)
                        c.label
                        (state.chosenId = Some c.elementId)
                        state.enabled
                        (fun () -> handlers.chooseElement c.elementId)))
        ] :> IView

    /// Every variable, in a fixed order (so the selector boxes keep stable names / positions).
    let private allVariables : VariableChoice list = [ VaryWaveLength; VaryR1; VaryR2 ]

    /// The variable-selection row (step 2). ALL variable boxes are always present (stable names) and only
    /// the ones the chosen element permits are shown — see `optionBoxV` for why the set is toggled by
    /// visibility rather than by adding / removing boxes.
    let private variableRow (state : State) (handlers : Handlers) : IView =
        let hasChoices = not (List.isEmpty state.variableChoices)
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                TextBlock.create [
                    TextBlock.text "(this element has nothing to vary — pick a source, polarizer, or sample)"
                    TextBlock.foreground (brush (color 120 120 120))
                    TextBlock.isVisible (not hasChoices)
                ]
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.isVisible hasChoices
                    WrapPanel.children (
                        allVariables
                        |> List.map (fun v ->
                            let allowed = List.contains v state.variableChoices
                            optionBoxV
                                (UiIds.variable (variableCode v))
                                (variableLabel v)
                                (state.chosenVariable = Some v)
                                (state.enabled && allowed)
                                allowed
                                (fun () -> handlers.chooseVariable v)))
                ]
            ]
        ] :> IView

    /// The capture-mode selection row (step 3a: T / R / both).
    let private measurementRow (state : State) (handlers : Handlers) : IView =
        let one (m : MeasurementChoice) : IView =
            optionBox
                (UiIds.measurement (measurementCode m))
                (measurementLabel m)
                (state.measurement = m)
                state.enabled
                (fun () -> handlers.chooseMeasurement m)
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children [ one CaptureT; one CaptureR; one CaptureBoth ]
        ] :> IView

    /// Parse a plain decimal (invariant culture).
    let private parseFloat (text : string) : float option =
        match System.Double.TryParse(text, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None

    let private parseInt (text : string) : int option =
        match System.Int32.TryParse(text, System.Globalization.NumberStyles.Integer, System.Globalization.CultureInfo.InvariantCulture) with
        | true, v when v >= 2 -> Some v
        | _ -> None

    /// A numeric text field; commits on Enter / blur.
    let private numberField (id : string) (text : string) (enabled : bool) (commit : obj -> unit) : IView =
        TextBox.create [
            TextBox.name id
            TextBox.width 70.0
            TextBox.isEnabled enabled
            TextBox.text text
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commit e.Source)
            TextBox.onLostFocus (fun e -> commit e.Source)
        ] :> IView

    /// The range inputs (step 3b): min / max (in the variable's unit) and the point count. Always present
    /// (its visibility toggles with whether a variable is chosen) so the bay's child list length stays
    /// stable — a varying list length would shift named controls onto one another's slots (see `optionBoxV`).
    let private rangeRow (state : State) (handlers : Handlers) : IView =
        let hasVariable = match state.chosenVariable with Some _ -> true | None -> false
        let commitFloat (onCommit : float -> unit) (src : obj) : unit =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> parseFloat tb.Text |> Option.iter onCommit
            | _ -> ()
        let commitInt (onCommit : int -> unit) (src : obj) : unit =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> parseInt tb.Text |> Option.iter onCommit
            | _ -> ()
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.isVisible hasVariable
            StackPanel.children [
                TextBlock.create [ TextBlock.text (sprintf "min (%s):" state.rangeUnitLabel); TextBlock.verticalAlignment VerticalAlignment.Center ]
                numberField UiIds.rangeMin (sprintf "%g" state.rangeMin) state.enabled (commitFloat handlers.setRangeMin)
                TextBlock.create [ TextBlock.text (sprintf "max (%s):" state.rangeUnitLabel); TextBlock.verticalAlignment VerticalAlignment.Center ]
                numberField UiIds.rangeMax (sprintf "%g" state.rangeMax) state.enabled (commitFloat handlers.setRangeMax)
                TextBlock.create [ TextBlock.text "points:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                numberField UiIds.rangePoints (sprintf "%d" state.rangePoints) state.enabled (commitInt handlers.setRangePoints)
            ]
        ] :> IView

    // -- A plain action button (Add / Update / New). --
    let private actionButton (id : string) (label : string) (accent : bool) (enabled : bool) (onClick : unit -> unit) : IView =
        Border.create [
            Border.name id
            Border.isEnabled enabled
            Border.opacity (if enabled then 1.0 else 0.4)
            Border.background (brush (if accent then chosenBackground else idleBackground))
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(12.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id, label, enabled)))
        ] :> IView

    /// The Add / Update + New action row.
    let private actionRow (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                actionButton UiIds.addButton (if state.isEditing then "Update experiment" else "Add experiment") true state.canAdd handlers.addOrUpdate
                actionButton UiIds.newButton "New" false state.enabled handlers.newExperiment
            ]
        ] :> IView

    /// One collection row: the experiment description (click to Edit, highlighted while editing) and a
    /// Remove action.
    let private collectionRow (handlers : Handlers) (r : ExperimentRow) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 0.0
            StackPanel.margin (Thickness(0.0, 0.0, 0.0, 4.0))
            StackPanel.children [
                Border.create [
                    automationId (UiIds.editButton r.id)
                    Border.background (brush (if r.isEditing then editingBackground else idleBackground))
                    Border.borderBrush (brush idleBorder)
                    Border.borderThickness 1.0
                    Border.cornerRadius (CornerRadius 3.0)
                    Border.padding (Thickness(10.0, 4.0))
                    Border.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                    Border.maxWidth 320.0
                    Border.child (TextBlock.create [ TextBlock.text r.description; TextBlock.textWrapping TextWrapping.Wrap ])
                    Border.onPointerPressed ((fun e -> e.Handled <- true; handlers.editExperiment r.id), SubPatchOptions.OnChangeOf (box (r.id, r.isEditing)))
                ] :> IView
                Border.create [
                    automationId (UiIds.removeButton r.id)
                    Border.background (brush idleBackground)
                    Border.borderBrush (brush idleBorder)
                    Border.borderThickness 1.0
                    Border.cornerRadius (CornerRadius 3.0)
                    Border.padding (Thickness(10.0, 4.0))
                    Border.verticalAlignment VerticalAlignment.Center
                    Border.child (TextBlock.create [ TextBlock.text "Remove" ])
                    Border.onPointerPressed ((fun e -> e.Handled <- true; handlers.removeExperiment r.id), SubPatchOptions.OnChangeOf (box r.id))
                ] :> IView
            ]
        ] :> IView

    /// The collection block: a heading and the added-experiment rows (or a hint when empty).
    let private collectionBlock (state : State) (handlers : Handlers) : IView list =
        [
            heading "Experiments:"
            (match state.collection with
             | [] -> TextBlock.create [ TextBlock.text "(none added yet — build one above and click Add)"; TextBlock.foreground (brush (color 120 120 120)) ] :> IView
             | rows ->
                 StackPanel.create [
                     StackPanel.name UiIds.collection
                     StackPanel.orientation Orientation.Vertical
                     StackPanel.children (rows |> List.map (collectionRow handlers))
                 ] :> IView)
        ]

    /// The Experiments bay — a multi-step editor (choose element → variable → capture → range → Add) and
    /// the persistent collection of added experiments, with the inline chart of the current editor's run.
    let view (state : State) (handlers : Handlers) : IView =
        let readoutText =
            if state.readout = "" then "Experiment: (choose an element, a variable to vary, and click Add)"
            else state.readout
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                [
                    TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.text readoutText ] :> IView
                    heading "1. Element to vary:"
                    elementRow state handlers
                    heading "2. Vary:"
                    variableRow state handlers
                    heading "3. Capture:"
                    measurementRow state handlers
                ]
                @ [ rangeRow state handlers ]
                @ [ actionRow state handlers ]
                @ collectionBlock state handlers
                @ resultBlock state handlers)
        ] :> IView

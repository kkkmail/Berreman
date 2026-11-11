namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
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
            elementId : ConstructorUiKey
            label : ControlCaption
        }

    /// The quantity to vary — a domain-free mirror of the domain `VariableParameter` (a DU, not an enum).
    /// The host maps it back to the domain type for the chosen element.
    type VariableChoice =
        | VaryWaveLength
        | VaryR1
        | VaryR2
        /// A dial-mounted item's raw dial reading (spec 0050 S9.1).
        | VaryDialReading

        /// The stable code (drives its automation id).
        member this.uiKey : ConstructorUiKey =
            match this with
            | VaryWaveLength -> ConstructorUiKey "wavelength"
            | VaryR1 -> ConstructorUiKey "r1"
            | VaryR2 -> ConstructorUiKey "r2"
            | VaryDialReading -> ConstructorUiKey "dial"

        /// A short label (the selector button text).
        member this.caption : ControlCaption =
            match this with
            | VaryWaveLength -> ControlCaption "Wavelength"
            | VaryR1 -> ControlCaption "Rotation R1"
            | VaryR2 -> ControlCaption "Incidence R2"
            | VaryDialReading -> ControlCaption "Dial reading"

    /// What the experiment captures — a domain-free mirror of the domain `MeasurementMode`.
    type MeasurementChoice =
        | CaptureT
        | CaptureR
        | CaptureBoth

        /// The stable code (drives its automation id).
        member this.uiKey : ConstructorUiKey =
            match this with
            | CaptureT -> ConstructorUiKey "t"
            | CaptureR -> ConstructorUiKey "r"
            | CaptureBoth -> ConstructorUiKey "both"

        /// A short label (the selector button text, unless a detector's offer labels it).
        member this.caption : ControlCaption =
            match this with
            | CaptureT -> ControlCaption "Transmitted (T)"
            | CaptureR -> ControlCaption "Reflected (R)"
            | CaptureBoth -> ControlCaption "Both (T + R)"

    /// Spec 0046: one capture the scene's detectors offer — the branch choice and what its detector reads
    /// there (e.g. "Transmitted (T) — intensity", "Reflected (R) — Ψ, Δ (ellipsometer)").
    type MeasurementOffer =
        {
            choice : MeasurementChoice
            label : ControlCaption
        }

    /// A wavelength in nanometres, as the bay shows and takes the run's source wavelength.
    type BayNanometers =
        | BayNanometers of float

        member this.value = let (BayNanometers nm) = this in nm

        /// The general (`%g`) form, as the field and the tunable-limits note show it.
        member this.text : string = $"%g{this.value}"

        member this.valueCaption : ControlCaption = ControlCaption this.text

        /// A typed wavelength (invariant culture); `None` if malformed.
        static member tryCreate (text : string) : BayNanometers option =
            match System.Double.TryParse(text, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
            | true, v -> Some (BayNanometers v)
            | _ -> None

    /// A tunable source's limits.
    type SourceWaveLengthLimits =
        {
            lowest : BayNanometers
            highest : BayNanometers
        }

    /// Spec 0046: the source wavelength the run uses, shown below the range while the wavelength is NOT the
    /// varied quantity: a fixed line (read only), or a tunable source's setting (editable within its limits).
    type SourceWaveLengthState =
        | NoSourceWaveLength
        | FixedSourceWaveLength of setting : BayNanometers
        | TunableSourceWaveLength of setting : BayNanometers * limits : SourceWaveLengthLimits

    /// One point of a drawn series, in axis-display units.
    [<Softellect.Sys.NumericCarrier("an element of a chart series (R3, a sweep array): the plotted x and y of every point of the run, mapped to the screen in the inline chart's per-point loop")>]
    type ChartPoint =
        {
            x : float
            y : float
        }

        /// This angular point (x the angle in degrees, y the radius) on the cartesian plane: the pure
        /// counterpart of ScottPlot's `PolarAxis.GetCoordinates` (spec 0027/030). The chart window draws
        /// through ScottPlot's own, so its polar grid lines up with the plotted points.
        member this.polarXY : ChartPoint =
            let t = this.x * System.Math.PI / 180.0
            { x = this.y * cos t; y = this.y * sin t }

    /// One drawn chart series: a display name (legend) and its points.
    type ChartSeries =
        {
            name : ControlCaption
            points : ChartPoint list
        }

    /// Whether an added experiment is the one loaded in the editor (its row is highlighted).
    type ExperimentRowFocus =
        | LoadedInEditor
        | NotLoadedInEditor

    /// One added experiment shown in the collection (the host flattens the domain `Experiment`s to these).
    type ExperimentRow =
        {
            /// The experiment's stable id (the host maps it back to `ExperimentId`).
            id : ConstructorUiKey
            /// A human-readable one-line description of the experiment.
            description : ControlCaption
            /// The data badge's text, shown beside the description (spec 0050 S9.1).
            badge : ControlCaption
            focus : ExperimentRowFocus
        }

    /// Whether a role box is the loaded experiment's role (spec 0050 S9.1).
    type ExperimentRoleMark =
        | RoleChosen
        | RoleNotChosen

    /// One role box of the loaded experiment's role row (spec 0050 S9.1).
    type ExperimentRoleOption =
        {
            key : ConstructorUiKey
            caption : ControlCaption
            mark : ExperimentRoleMark
        }

    /// Whether Add appends a new experiment or updates the one being edited (the button reads "Update").
    type ExperimentEditorMode =
        | EditorAddsNew
        | EditorUpdatesExisting

    /// One end of the variation range, in the varied quantity's display unit (nm or °).
    type ExperimentRangeEnd =
        | ExperimentRangeEnd of float

        member this.value = let (ExperimentRangeEnd v) = this in v

        member this.valueCaption : ControlCaption = ControlCaption $"%g{this.value}"

        /// A typed range end (invariant culture); `None` if malformed.
        static member tryCreate (text : string) : ExperimentRangeEnd option =
            match System.Double.TryParse(text, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
            | true, v -> Some (ExperimentRangeEnd v)
            | _ -> None

    /// How many points the range is sampled at.
    type ExperimentPointCount =
        | ExperimentPointCount of int

        member this.value = let (ExperimentPointCount n) = this in n

        member this.valueCaption : ControlCaption = ControlCaption $"%d{this.value}"

        /// A typed count (invariant culture); `None` if malformed or below two.
        static member tryCreate (text : string) : ExperimentPointCount option =
            match System.Int32.TryParse(text, System.Globalization.NumberStyles.Integer, System.Globalization.CultureInfo.InvariantCulture) with
            | true, v when v >= 2 -> Some (ExperimentPointCount v)
            | _ -> None

    /// An ellipsometric angle (Ψ or Δ) in degrees, as the single-point readout shows it.
    type EllipsometricDegrees =
        | EllipsometricDegrees of float

        member this.value = let (EllipsometricDegrees d) = this in d

        member this.text : string = $"%.2f{this.value}"

    /// The ellipsometer's single-point reading.
    type PsiDeltaReadout =
        {
            psi : EllipsometricDegrees
            delta : EllipsometricDegrees
        }

    /// The bay's pure, serializable state.
    type ExperimentControlsState =
        {
            /// The present elements, each offered as a candidate element-to-vary.
            candidates : SweepCandidate list
            /// The currently-chosen element-to-vary (by id), if any.
            chosenId : ConstructorUiKey option
            /// The variables the CHOSEN element permits varying (host-supplied per its kind; empty when the
            /// element exposes nothing to vary).
            variableChoices : VariableChoice list
            /// The chosen variable, if any.
            chosenVariable : VariableChoice option
            /// The chosen capture mode (transmitted / reflected / both).
            measurement : MeasurementChoice
            /// Spec 0046: the captures the scene's detectors offer for the chosen element, each labelled by
            /// what its detector reads (host-supplied: a transmitted detector offers T, a reflected one R,
            /// both offer T / R / Both). Only these are shown; empty while no detector sees the element.
            measurementOffers : MeasurementOffer list
            /// Spec 0046: the source wavelength the run uses while the wavelength is not varied.
            sourceWaveLength : SourceWaveLengthState
            /// The variation range (in the variable's display unit) and its point count.
            rangeMin : ExperimentRangeEnd
            rangeMax : ExperimentRangeEnd
            rangePoints : ExperimentPointCount
            /// The variable's display unit ("nm" / "°"), shown next to the range inputs.
            rangeUnitLabel : ControlCaption
            /// Whether the draft can be committed (an element + a variable are chosen).
            addAvailability : ControlAvailability
            /// The draft's closed blocks, then its current block's held axes, one line each (spec 0050 S9.1).
            scanLines : ControlCaption list
            /// Whether Add variable and Add block are offered (spec 0050 S9.1).
            scanAvailability : ControlAvailability
            /// Whether a collected experiment's Remove is offered (spec 0050 S5.3).
            removalAvailability : ControlAvailability
            editorMode : ExperimentEditorMode
            /// The loaded experiment's role boxes, in the roles' order (spec 0050 S9.1).
            roleOptions : ExperimentRoleOption list
            /// The loaded experiment's session and notes as their fields show them (spec 0050 S9.1).
            sessionText : ControlCaption
            notesText : ControlCaption
            /// Offered while an experiment is loaded in a writable kit: its role, session and notes show (spec 0050 S9.1).
            bookkeepingAvailability : ControlAvailability
            /// The added experiments (the persistent collection).
            collection : ExperimentRow list
            /// The built-experiment readout, once the draft is complete.
            readout : ControlCaption option
            /// `ControlWithheld` when there are no elements to vary.
            availability : ControlAvailability
            /// The inline chart's series (one intensity series, two Ψ/Δ series, or two T/R series for a
            /// both-capture intensity run), drawn as polylines. Empty when there is no run to show.
            series : ChartSeries list
            /// Axis labels for the inline chart (and the pop-out window).
            xLabel : ControlCaption
            yLabel : ControlCaption
            /// A prose description of what the chart is about (the bound elements + what is varied).
            description : ControlCaption
            /// The ellipsometer Ψ/Δ single-point readout for the RotateR1 kind; `None` for an intensity
            /// detector or a sweep that produces series instead.
            psiDelta : PsiDeltaReadout option
        }

    let emptyExperimentControls : ExperimentControlsState =
        {
            candidates = []
            chosenId = None
            variableChoices = []
            chosenVariable = None
            measurement = CaptureT
            measurementOffers = []
            sourceWaveLength = NoSourceWaveLength
            rangeMin = ExperimentRangeEnd 0.0
            rangeMax = ExperimentRangeEnd 360.0
            rangePoints = ExperimentPointCount 73
            rangeUnitLabel = ControlCaption "°"
            addAvailability = ControlWithheld
            scanLines = []
            scanAvailability = ControlWithheld
            removalAvailability = ControlOffered
            editorMode = EditorAddsNew
            roleOptions = []
            sessionText = ControlCaption ""
            notesText = ControlCaption ""
            bookkeepingAvailability = ControlWithheld
            collection = []
            readout = None
            availability = ControlWithheld
            series = []
            xLabel = ControlCaption ""
            yLabel = ControlCaption ""
            description = ControlCaption ""
            psiDelta = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs).
    type ExperimentControlsHandlers =
        {
            /// Pick which present element to vary (by its id).
            chooseElement : ConstructorUiKey -> unit
            /// Pick the varied quantity.
            chooseVariable : VariableChoice -> unit
            /// Pick the capture mode.
            chooseMeasurement : MeasurementChoice -> unit
            /// Set the range minimum / maximum (in the variable's display unit) and the point count.
            setRangeMin : ExperimentRangeEnd -> unit
            setRangeMax : ExperimentRangeEnd -> unit
            setRangePoints : ExperimentPointCount -> unit
            /// Spec 0046: set a tunable source's wavelength for the run.
            setSourceWaveLength : BayNanometers -> unit
            /// Confirm the draft: Add a new experiment, or Update the one being edited.
            addOrUpdate : unit -> unit
            /// Start a brand-new experiment (clears the editor).
            newExperiment : unit -> unit
            /// Add variable: the edited axis joins the draft's current block (spec 0050 S9.1).
            addScanAxis : unit -> unit
            /// Add block: the draft's current block closes and a new one starts (spec 0050 S9.1).
            addScanBlock : unit -> unit
            /// The loaded experiment takes the role of that code, its session or its notes (spec 0050 S9.1).
            setRole : ConstructorUiKey -> unit
            setSession : ControlCaption -> unit
            setNotes : ControlCaption -> unit
            /// Load an experiment (by id) into the editor for editing.
            editExperiment : ConstructorUiKey -> unit
            /// Remove an experiment (by id) from the collection.
            removeExperiment : ConstructorUiKey -> unit
            /// VIEW an experiment (by id): open the pop-out chart window for that collected experiment. The
            /// per-row "View" button and a double-click on the row both call this — the same testable action.
            viewExperiment : ConstructorUiKey -> unit
            /// Open the pop-out interactive chart window for the current DRAFT (double-click the inline chart).
            /// Spec 0046: the separate "Open chart window" button is gone — a collected experiment's "View"
            /// is the explicit way to open its chart.
            openChartWindow : unit -> unit
        }

    // -- The button look, identical to the other bars' idle button (so they MATCH). --
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
    let private chosenBackground = Color.FromRgb(150uy, 185uy, 235uy)
    let private editingBackground = Color.FromRgb(255uy, 224uy, 160uy)
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)

    /// Whether a control of the bay is shown, or kept at its stable slot but hidden.
    type private BayPresence =
        | ShownInBay
        | HiddenInBay

    /// A clickable, styled Border, highlighted by `background` when it is the chosen option. `presence`
    /// toggles the box in place (rather than adding / removing it from the list): a bounded selector whose
    /// MEMBERS change (e.g. the variable set per element kind) keeps every box present at a STABLE slot and
    /// only flips visibility, so the boxes' click subscriptions stay coherent across kind changes (and the
    /// child-list length stays stable — see `rangeRow`). A hidden box is never enabled. The id is a
    /// reuse-safe AutomationId (never a write-once `Name`: Avalonia forbids renaming a styled control).
    let private optionBoxV (id : ConstructorUiId) (label : ControlCaption) (background : Color) (bay : ControlAvailability) (presence : BayPresence) (onClick : unit -> unit) : IView =
        let availability =
            match bay, presence with
            | ControlOffered, ShownInBay -> ControlOffered
            | ControlOffered, HiddenInBay | ControlWithheld, _ -> ControlWithheld
        Border.create [
            id.automationAttribute<Border> ()
            Border.isVisible (match presence with ShownInBay -> true | HiddenInBay -> false)
            Border.isEnabled availability.isEnabled
            Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
            Border.background (brush background)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 6.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label.value ])
            // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the
            // highlight or the presence flips so a reused box can't keep a stale handler.
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (background, presence)))
        ] :> IView

    /// A clickable option box for a REORDERABLE list (candidates, collection rows). Such a list shifts a
    /// control onto a different item's slot when an item is removed, so the box carries a reuse-safe
    /// AutomationId AND is KEYED by it (`View.withKey`) — a membership change recreates the box at the
    /// shifted slot instead of patching another item's styled control in place.
    let private idOptionBox (id : ConstructorUiId) (label : ControlCaption) (background : Color) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
        let keyedBox =
            Border.create [
                id.automationAttribute<Border> ()
                Border.isEnabled availability.isEnabled
                Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
                Border.background (brush background)
                Border.borderBrush (brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(10.0, 5.0))
                Border.margin (Thickness(0.0, 0.0, 8.0, 6.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text label.value ])
                Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id, background)))
            ]
            // Fully qualified: `Avalonia.FuncUI.Types` (opened above for `IView`) also exports a
            // `View<'t>` type, so the bare `View` name would be ambiguous with the DSL `View` module.
            |> Avalonia.FuncUI.DSL.View.withKey id.value
        keyedBox :> IView

    /// A section heading (the numbered step labels), shown only while `presence` says so (stable slot).
    let private heading (text : ControlCaption) (presence : BayPresence) : IView =
        TextBlock.create [
            TextBlock.text text.value
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.margin (Thickness(0.0, 2.0, 0.0, 2.0))
            TextBlock.isVisible (match presence with ShownInBay -> true | HiddenInBay -> false)
        ] :> IView

    // -- The inline result area: axes, major gridlines, multi-series polylines, and a description. --
    let private chartFrame : Size = Size(240.0, 120.0)
    // Room for the y label on the left and the x label below.
    let private plotArea : Rect = Rect(chartFrame).Deflate(Thickness(30.0, 6.0, 8.0, 18.0))
    let private seriesColors =
        [| Color.FromRgb(30uy, 90uy, 200uy); Color.FromRgb(220uy, 120uy, 20uy); Color.FromRgb(40uy, 150uy, 70uy) |]

    /// The data window the inline chart shows: the lowest and the highest corner over every series.
    type private PlotWindow =
        {
            lowest : ChartPoint
            highest : ChartPoint
        }

        /// Map a data point to the inner plot rectangle (screen y-down).
        member this.toScreen (p : ChartPoint) : Point =
            let px = plotArea.X + plotArea.Width * (p.x - this.lowest.x) / (this.highest.x - this.lowest.x)
            let py = plotArea.Y + plotArea.Height * (1.0 - (p.y - this.lowest.y) / (this.highest.y - this.lowest.y))
            Point(px, py)

        /// The combined data range over all series (falls back to a unit box when empty / degenerate).
        static member over (series : ChartSeries list) : PlotWindow =
            let pts = series |> List.collect (fun s -> s.points)
            match pts with
            | [] -> { lowest = { x = 0.0; y = 0.0 }; highest = { x = 1.0; y = 1.0 } }
            | _ ->
                let fixSpan lo hi = if hi - lo > 1e-12 then lo, hi else lo - 0.5, hi + 0.5
                let xlo, xhi = fixSpan (pts |> List.map (fun p -> p.x) |> List.min) (pts |> List.map (fun p -> p.x) |> List.max)
                let ylo, yhi = fixSpan (pts |> List.map (fun p -> p.y) |> List.min) (pts |> List.map (fun p -> p.y) |> List.max)
                { lowest = { x = xlo; y = ylo }; highest = { x = xhi; y = yhi } }

    /// The L-shaped axis lines (bottom + left).
    let private axisLines () : IView list =
        let line (a : Point) (b : Point) : IView =
            Line.create [
                Line.startPoint a
                Line.endPoint b
                Line.stroke (brush idleBorder)
                Line.strokeThickness 1.0
            ] :> IView
        [
            line plotArea.TopLeft plotArea.BottomLeft
            line plotArea.BottomLeft plotArea.BottomRight
        ]

    /// Light-grey major gridlines (4 across, 3 up) inside the plot rectangle.
    let private gridLines () : IView list =
        let grid = brush (Color.FromRgb(220uy, 220uy, 220uy))
        let vert =
            [ 1 .. 4 ]
            |> List.map (fun k ->
                let px = plotArea.X + plotArea.Width * float k / 5.0
                Line.create [
                    Line.startPoint (Point(px, plotArea.Y))
                    Line.endPoint (Point(px, plotArea.Bottom))
                    Line.stroke grid
                    Line.strokeThickness 0.5
                ] :> IView)
        let horiz =
            [ 1 .. 3 ]
            |> List.map (fun k ->
                let py = plotArea.Y + plotArea.Height * float k / 4.0
                Line.create [
                    Line.startPoint (Point(plotArea.X, py))
                    Line.endPoint (Point(plotArea.Right, py))
                    Line.stroke grid
                    Line.strokeThickness 0.5
                ] :> IView)
        vert @ horiz

    /// One series as a coloured polyline; the FIRST series carries the stable chart automation id.
    let private seriesPolyline (window : PlotWindow) (stroke : Color) (s : ChartSeries) : IView =
        Polyline.create [
            Polyline.name ConstructorUiId.experimentChart.value
            Polyline.points (s.points |> List.map window.toScreen)
            Polyline.stroke (brush stroke)
            Polyline.strokeThickness 1.5
        ] :> IView

    /// The axis labels (x centred under the bottom axis, y at the top-left).
    let private axisLabels (xLabel : ControlCaption) (yLabel : ControlCaption) : IView list =
        [
            (if xLabel.value = "" then None
             else
                 Some (
                     TextBlock.create [
                         TextBlock.text xLabel.value
                         TextBlock.fontSize 9.0
                         TextBlock.left plotArea.X
                         TextBlock.top (plotArea.Bottom + 4.0)
                         TextBlock.width plotArea.Width
                         TextBlock.textAlignment TextAlignment.Center
                     ] :> IView))
            (if yLabel.value = "" then None
             else
                 Some (
                     TextBlock.create [
                         TextBlock.text yLabel.value
                         TextBlock.fontSize 9.0
                         TextBlock.left 1.0
                         TextBlock.top 0.0
                     ] :> IView))
        ]
        |> List.choose id

    /// The inline chart Canvas (axes + gridlines + series polylines + axis labels).
    let private chartCanvas (state : ExperimentControlsState) : IView =
        let window = PlotWindow.over state.series
        let children =
            gridLines ()
            @ axisLines ()
            @ (state.series |> List.mapi (fun i s -> seriesPolyline window seriesColors.[i % seriesColors.Length] s))
            @ axisLabels state.xLabel state.yLabel
        Canvas.create [
            Canvas.width chartFrame.Width
            Canvas.height chartFrame.Height
            Canvas.children children
        ] :> IView

    /// The result block under the editor: the ellipsometer single-point Ψ/Δ text when present, plus the
    /// inline chart (double-click opens the pop-out window) and the prose description.
    let private resultBlock (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView list =
        // Both sub-blocks come and go with the state (variable membership in the bay's child list), so
        // every item is KEYED: when the Ψ/Δ readout toggles and the chart items shift a slot, FuncUI
        // recreates them instead of cross-patching one styled control into another.
        let psiBlock =
            match state.psiDelta with
            | Some reading ->
                let psiText =
                    TextBlock.create [
                        TextBlock.name ConstructorUiId.ellipsometerReadout.value
                        TextBlock.text $"Ψ = {reading.psi.text}°   Δ = {reading.delta.text}°"
                    ]
                    |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.ellipsometerReadout.value
                [ psiText :> IView ]
            | None -> []
        let chartBlock =
            match state.series with
            | [] -> []
            | _ ->
                let chartBox =
                    Border.create [
                        Border.borderBrush (brush idleBorder)
                        Border.borderThickness 1.0
                        Border.background (brush (Color.FromRgb(248uy, 248uy, 248uy)))
                        Border.width chartFrame.Width
                        Border.height chartFrame.Height
                        Border.horizontalAlignment HorizontalAlignment.Left
                        Border.child (chartCanvas state)
                        // A double-click opens the draft's pop-out window — via Avalonia's built-in DoubleTapped
                        // gesture (more reliable than a hand-rolled PointerPressed ClickCount check).
                        Border.onDoubleTapped ((fun e -> e.Handled <- true; handlers.openChartWindow ()), SubPatchOptions.Always)
                    ]
                    |> Avalonia.FuncUI.DSL.View.withKey "ExperimentChartBox"
                let descriptionText =
                    TextBlock.create [
                        TextBlock.name ConstructorUiId.experimentChartDescription.value
                        TextBlock.text state.description.value
                        TextBlock.fontSize 11.0
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.maxWidth 360.0
                    ]
                    |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.experimentChartDescription.value
                [ chartBox :> IView; descriptionText :> IView ]
        psiBlock @ chartBlock

    /// The element-selection row (step 1 of the editor).
    let private elementRow (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView =
        WrapPanel.create [
            WrapPanel.name ConstructorUiId.experimentCandidates.value
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (
                state.candidates
                |> List.map (fun c ->
                    idOptionBox
                        (ConstructorUiId.experimentCandidate c.elementId)
                        c.label
                        (if state.chosenId = Some c.elementId then chosenBackground else idleBackground)
                        state.availability
                        (fun () -> handlers.chooseElement c.elementId)))
        ] :> IView

    /// Every variable, in a fixed order (so the selector boxes keep stable names / positions); the dial reading is last
    /// (spec 0050 S9.1).
    let private allVariables : VariableChoice list = [ VaryWaveLength; VaryR1; VaryR2; VaryDialReading ]

    /// The variable-selection row (step 2). ALL variable boxes are always present (stable names) and only
    /// the ones the chosen element permits are shown — see `optionBoxV` for why the set is toggled by
    /// visibility rather than by adding / removing boxes.
    let private variableRow (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView =
        let hasChoices = not (List.isEmpty state.variableChoices)
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                TextBlock.create [
                    TextBlock.text "(this element has nothing to vary — pick a source, polarizer, or sample)"
                    TextBlock.foreground (brush (Color.FromRgb(120uy, 120uy, 120uy)))
                    TextBlock.isVisible (not hasChoices)
                ]
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.isVisible hasChoices
                    WrapPanel.children (
                        allVariables
                        |> List.map (fun v ->
                            optionBoxV
                                (ConstructorUiId.experimentVariable v.uiKey)
                                v.caption
                                (if state.chosenVariable = Some v then chosenBackground else idleBackground)
                                state.availability
                                (if List.contains v state.variableChoices then ShownInBay else HiddenInBay)
                                (fun () -> handlers.chooseVariable v)))
                ]
            ]
        ] :> IView

    /// The capture selection row (step 3). Spec 0046: the DETECTORS decide what is captured — only the
    /// captures they offer are shown, each labelled by what its detector reads (`measurementOffers`); every
    /// box stays at its stable slot and only its visibility flips (the `variableRow` discipline); with none,
    /// a hint says why. The whole row appears only once something to vary is chosen.
    let private measurementRow (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView =
        let hasVariable = match state.chosenVariable with Some _ -> true | None -> false
        let noneOffered = List.isEmpty state.measurementOffers
        let one (m : MeasurementChoice) : IView =
            let offer = state.measurementOffers |> List.tryFind (fun o -> o.choice = m)
            optionBoxV
                (ConstructorUiId.experimentMeasurement m.uiKey)
                (offer |> Option.map (fun o -> o.label) |> Option.defaultValue m.caption)
                (if state.measurement = m then chosenBackground else idleBackground)
                state.availability
                (match offer with Some _ -> ShownInBay | None -> HiddenInBay)
                (fun () -> handlers.chooseMeasurement m)
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.isVisible hasVariable
            StackPanel.children [
                TextBlock.create [
                    TextBlock.name ConstructorUiId.experimentNoCapture.value
                    TextBlock.text "(no detector captures this element's light — put a detector on its transmitted beam or on its reflected ray: right-click an element for its rays)"
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.maxWidth 420.0
                    TextBlock.foreground (brush (Color.FromRgb(120uy, 120uy, 120uy)))
                    TextBlock.isVisible noneOffered
                ]
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.isVisible (not noneOffered)
                    WrapPanel.children [ one CaptureT; one CaptureR; one CaptureBoth ]
                ]
            ]
        ] :> IView

    /// A text field of that width; commits on Enter / blur.
    let private committingField (id : ConstructorUiId) (width : IAttr<TextBox>) (text : ControlCaption) (availability : ControlAvailability) (commit : obj -> unit) : IView =
        TextBox.create [
            TextBox.name id.value
            width
            TextBox.isEnabled availability.isEnabled
            TextBox.text text.value
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commit e.Source)
            TextBox.onLostFocus (fun e -> commit e.Source)
        ] :> IView

    /// A numeric text field; commits on Enter / blur.
    let private numberField (id : ConstructorUiId) (text : ControlCaption) (availability : ControlAvailability) (commit : obj -> unit) : IView =
        committingField id (TextBox.width 70.0) text availability commit

    /// The range inputs (step 3b): min / max (in the variable's unit) and the point count. Always present
    /// (its visibility toggles with whether a variable is chosen) so the bay's child list length stays
    /// stable — a varying list length would shift named controls onto one another's slots (see `optionBoxV`).
    let private rangeRow (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView =
        let hasVariable = match state.chosenVariable with Some _ -> true | None -> false
        let commitRangeEnd (onCommit : ExperimentRangeEnd -> unit) (src : obj) : unit =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> ExperimentRangeEnd.tryCreate tb.Text |> Option.iter onCommit
            | _ -> ()
        let commitPointCount (onCommit : ExperimentPointCount -> unit) (src : obj) : unit =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> ExperimentPointCount.tryCreate tb.Text |> Option.iter onCommit
            | _ -> ()
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.isVisible hasVariable
            StackPanel.children [
                TextBlock.create [ TextBlock.text $"min ({state.rangeUnitLabel.value}):"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                numberField ConstructorUiId.experimentRangeMin state.rangeMin.valueCaption state.availability (commitRangeEnd handlers.setRangeMin)
                TextBlock.create [ TextBlock.text $"max ({state.rangeUnitLabel.value}):"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                numberField ConstructorUiId.experimentRangeMax state.rangeMax.valueCaption state.availability (commitRangeEnd handlers.setRangeMax)
                TextBlock.create [ TextBlock.text "points:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                numberField ConstructorUiId.experimentRangePoints state.rangePoints.valueCaption state.availability (commitPointCount handlers.setRangePoints)
            ]
        ] :> IView

    /// Spec 0046: the source-wavelength row — always present (stable slots), shown while the run's source
    /// wavelength is not the varied quantity: a fixed line read only, a tunable source's setting editable
    /// (the host clamps it to the source's limits).
    let private sourceWaveLengthRow (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView =
        let presence, availability, valueText, note =
            match state.sourceWaveLength with
            | NoSourceWaveLength -> HiddenInBay, ControlWithheld, ControlCaption "", ControlCaption ""
            | FixedSourceWaveLength setting -> ShownInBay, ControlWithheld, setting.valueCaption, ControlCaption "nm (a fixed line)"
            | TunableSourceWaveLength (setting, limits) ->
                ShownInBay, state.availability, setting.valueCaption, ControlCaption $"nm (tunable {limits.lowest.text} … {limits.highest.text} nm)"
        let commit (src : obj) : unit =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> BayNanometers.tryCreate tb.Text |> Option.iter handlers.setSourceWaveLength
            | _ -> ()
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.isVisible (match presence with ShownInBay -> true | HiddenInBay -> false)
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Source wavelength:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
                numberField ConstructorUiId.experimentSourceWaveLength valueText availability commit
                TextBlock.create [ TextBlock.name ConstructorUiId.experimentSourceWaveLengthNote.value; TextBlock.text note.value; TextBlock.verticalAlignment VerticalAlignment.Center ]
            ]
        ] :> IView

    // -- A plain action button (Add / Update / New). Regenerable (the Add label swaps to "Update
    // experiment" in place), so it carries a reuse-safe AutomationId, never a write-once `Name`. --
    let private actionButton (id : ConstructorUiId) (label : ControlCaption) (background : Color) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
        Border.create [
            id.automationAttribute<Border> ()
            Border.isEnabled availability.isEnabled
            Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
            Border.background (brush background)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(12.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label.value ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id, label, availability)))
        ] :> IView

    type ExperimentControlsState with

        /// The draft's scan lines under the range: its closed blocks, then its current block's held axes (spec 0050 S9.1).
        member internal this.scanSummaryPanel : IView =
            StackPanel.create [
                StackPanel.name ConstructorUiId.experimentScanSummary.value
                StackPanel.orientation Orientation.Vertical
                StackPanel.children (
                    this.scanLines
                    |> List.map (fun line ->
                        TextBlock.create [
                            TextBlock.text line.value
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.maxWidth 420.0
                        ] :> IView))
            ] :> IView

    /// The Add / Update + New action row, then Add variable and Add block (spec 0050 S9.1).
    let private actionRow (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView =
        let addCaption =
            match state.editorMode with
            | EditorUpdatesExisting -> ControlCaption "Update experiment"
            | EditorAddsNew -> ControlCaption "Add experiment"
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                actionButton ConstructorUiId.experimentAddButton addCaption chosenBackground state.addAvailability handlers.addOrUpdate
                actionButton ConstructorUiId.experimentNewButton (ControlCaption "New") idleBackground state.availability handlers.newExperiment
                actionButton ConstructorUiId.experimentAddScanAxisButton (ControlCaption "Add variable") idleBackground state.scanAvailability handlers.addScanAxis
                actionButton ConstructorUiId.experimentAddScanBlockButton (ControlCaption "Add block") idleBackground state.scanAvailability handlers.addScanBlock
            ]
        ] :> IView

    /// The loaded experiment's role row and its Session and Notes fields, shown while `bookkeepingAvailability` is offered
    /// (spec 0050 S9.1). Keyed by the loaded row: the fields' subscriptions never change, so a recreated block keeps a
    /// field or a box from writing to the experiment loaded before.
    let private bookkeepingBlock (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView =
        let loadedKey =
            state.collection
            |> List.tryPick (fun row ->
                match row.focus with
                | LoadedInEditor -> Some row.id.value
                | NotLoadedInEditor -> None)
            |> Option.defaultValue ""
        let commitCaption (commit : ControlCaption -> unit) (src : obj) : unit =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> commit (ControlCaption tb.Text)
            | _ -> ()
        let roleBox (option : ExperimentRoleOption) : IView =
            optionBoxV
                (ConstructorUiId.experimentRoleChoice option.key)
                option.caption
                (match option.mark with
                 | RoleChosen -> chosenBackground
                 | RoleNotChosen -> idleBackground)
                state.bookkeepingAvailability
                ShownInBay
                (fun () -> handlers.setRole option.key)
        let labelled (label : ControlCaption) (field : IView) : IView =
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.children [
                    TextBlock.create [ TextBlock.text label.value; TextBlock.verticalAlignment VerticalAlignment.Center ]
                    field
                ]
            ] :> IView
        let block =
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.spacing 4.0
                StackPanel.isVisible (
                    match state.bookkeepingAvailability with
                    | ControlOffered -> true
                    | ControlWithheld -> false)
                StackPanel.children [
                    heading (ControlCaption "Role:") ShownInBay
                    WrapPanel.create [
                        WrapPanel.orientation Orientation.Horizontal
                        WrapPanel.children (state.roleOptions |> List.map roleBox)
                    ]
                    labelled
                        (ControlCaption "Session:")
                        (committingField ConstructorUiId.experimentSessionField (TextBox.width 200.0) state.sessionText state.bookkeepingAvailability (commitCaption handlers.setSession))
                    labelled
                        (ControlCaption "Notes:")
                        (committingField ConstructorUiId.experimentNotesField (TextBox.width 320.0) state.notesText state.bookkeepingAvailability (commitCaption handlers.setNotes))
                ]
            ]
            |> Avalonia.FuncUI.DSL.View.withKey $"ExperimentBookkeeping_{loadedKey}"
        block :> IView

    // A small row action button (View / Remove), styled to stand out (accent) or plain.
    let private rowActionButton (id : ConstructorUiId) (label : ControlCaption) (background : Color) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
        Border.create [
            id.automationAttribute<Border> ()
            Border.isEnabled availability.isEnabled
            Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
            Border.background (brush background)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 4.0))
            Border.margin (Thickness(0.0, 0.0, 6.0, 0.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label.value ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id, label, availability)))
        ] :> IView

    /// One collection row: the experiment description (single-click Edit, highlighted while editing;
    /// double-click = View), its data badge (spec 0050 S9.1), then a "View" action (open the chart window for this
    /// experiment) and a Remove.
    /// The row is KEYED by its experiment id, so removing an item recreates the rows that shift slots
    /// instead of patching one row's controls into another's.
    let private collectionRow (removal : ControlAvailability) (handlers : ExperimentControlsHandlers) (r : ExperimentRow) : IView =
        let row =
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 0.0
                StackPanel.margin (Thickness(0.0, 0.0, 0.0, 4.0))
                StackPanel.children [
                    Border.create [
                        (ConstructorUiId.experimentEditButton r.id).automationAttribute<Border> ()
                        Border.background (brush (match r.focus with LoadedInEditor -> editingBackground | NotLoadedInEditor -> idleBackground))
                        Border.borderBrush (brush idleBorder)
                        Border.borderThickness 1.0
                        Border.cornerRadius (CornerRadius 3.0)
                        Border.padding (Thickness(10.0, 4.0))
                        Border.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                        Border.maxWidth 300.0
                        Border.child (TextBlock.create [ TextBlock.text r.description.value; TextBlock.textWrapping TextWrapping.Wrap ])
                        // Single-click edits; double-click opens the chart window for this experiment (the same
                        // "View" action as the button beside it — Avalonia's DoubleTapped gesture).
                        Border.onPointerPressed ((fun e -> e.Handled <- true; handlers.editExperiment r.id), SubPatchOptions.OnChangeOf (box (r.id, r.focus)))
                        Border.onDoubleTapped ((fun e -> e.Handled <- true; handlers.viewExperiment r.id), SubPatchOptions.OnChangeOf (box r.id.value))
                    ] :> IView
                    TextBlock.create [
                        (ConstructorUiId.experimentRowBadge r.id).automationAttribute<TextBlock> ()
                        TextBlock.text r.badge.value
                        TextBlock.fontSize 11.0
                        TextBlock.foreground (brush (Color.FromRgb(90uy, 90uy, 90uy)))
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                    ] :> IView
                    rowActionButton (ConstructorUiId.experimentViewButton r.id) (ControlCaption "View ↗") chosenBackground ControlOffered (fun () -> handlers.viewExperiment r.id)
                    rowActionButton (ConstructorUiId.experimentRemoveButton r.id) (ControlCaption "Remove") idleBackground removal (fun () -> handlers.removeExperiment r.id)
                ]
            ]
            |> Avalonia.FuncUI.DSL.View.withKey r.id.value
        row :> IView

    /// The collection block: a heading and the added-experiment rows (or a hint when empty).
    let private collectionBlock (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView list =
        [
            heading (ControlCaption "Experiments:") ShownInBay
            (match state.collection with
             | [] -> TextBlock.create [ TextBlock.text "(none added yet — build one above and click Add)"; TextBlock.foreground (brush (Color.FromRgb(120uy, 120uy, 120uy))) ] :> IView
             | rows ->
                 StackPanel.create [
                     StackPanel.name ConstructorUiId.experimentCollection.value
                     StackPanel.orientation Orientation.Vertical
                     StackPanel.children (rows |> List.map (collectionRow state.removalAvailability handlers))
                 ] :> IView)
        ]

    /// The Experiments bay — a multi-step editor (choose element → variable → capture → range → Add) and
    /// the persistent collection of added experiments, with the inline chart of the current editor's run.
    let experimentControlsView (state : ExperimentControlsState) (handlers : ExperimentControlsHandlers) : IView =
        let readoutText =
            match state.readout with
            | Some readout -> readout.value
            | None -> "Experiment: (choose an element, a variable to vary, and click Add)"
        // The later steps appear once something to vary is chosen (stable slots, visibility toggled).
        let laterSteps = match state.chosenVariable with Some _ -> ShownInBay | None -> HiddenInBay
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                [
                    TextBlock.create [ TextBlock.name ConstructorUiId.experimentReadout.value; TextBlock.text readoutText ] :> IView
                    heading (ControlCaption "1. Element to vary:") ShownInBay
                    elementRow state handlers
                    heading (ControlCaption "2. Vary:") ShownInBay
                    variableRow state handlers
                    heading (ControlCaption "3. Capture (the detectors decide what):") laterSteps
                    measurementRow state handlers
                    heading (ControlCaption "4. Range:") laterSteps
                ]
                @ [ rangeRow state handlers; sourceWaveLengthRow state handlers; state.scanSummaryPanel ]
                @ [ actionRow state handlers; bookkeepingBlock state handlers ]
                @ collectionBlock state handlers
                @ resultBlock state handlers)
        ] :> IView

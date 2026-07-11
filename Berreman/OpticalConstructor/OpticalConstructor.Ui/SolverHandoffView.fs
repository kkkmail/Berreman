/// Spec 0038 Part L (step 039) — the SolverHandoffView (UICOMP_XDUO_0011): the pure MVU model and
/// FuncUI projection behind `SolverHandoffWindow`, the inverse flow's TERMINAL screen. It summarizes
/// a RECEIVED collection (an `ExperimentCollectionStore.ExperimentCollectionSnapshot` — the pure
/// name-plus-experiments data shape, no live handle): per experiment its ordered `setup`, its
/// detector kind, its attached data file, and a TYPED per-experiment validation status, plus the
/// fixed message that the actual solver runs separately. Basic validation ONLY — three checks per
/// experiment, in order: every element except the sample is specified (bound); a measured-data file
/// is attached and reads schema-valid through the injected `ExperimentDataProxy`; and its range is
/// consistent with the step-25 experiment (folded in by `ExperimentDataLoad.loadAndValidate`, which
/// calls step-34 `validateAgainstExperiment`). NO solving and NO normalization happen here — the data
/// is gathered raw; corrections belong to the future solver. `update` reaches IO only through the
/// context's proxies; the whole surface is a pure `state → view` projection a headless test drives
/// over mock proxies without a real screen.
module OpticalConstructor.Ui.SolverHandoffView

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain

/// The fixed message the terminal screen always shows (the how-to's "the actual solver will be done
/// separately"). A `[<Literal>]` so a test can assert against it and step 044's id-consolidation can
/// move it mechanically.
[<Literal>]
let solverComesLaterMessage =
    "The measured data is gathered as-is — no solving or normalization happens here. The actual inverse solver runs separately."

/// Stable intent-named automation ids (CLAUDE.md UI guidance). Step 044 consolidates every id into one
/// module with a `Handoff` sub-module; these values are chosen to survive that mechanical move.
[<RequireQualifiedAccess>]
module UiIds =
    [<Literal>]
    let window = "SolverHandoffWindow"
    [<Literal>]
    let collectionName = "SolverHandoffCollectionName"
    [<Literal>]
    let solverMessage = "SolverHandoffSolverMessage"
    [<Literal>]
    let summaryList = "SolverHandoffSummaryList"
    [<Literal>]
    let closeButton = "SolverHandoffCloseButton"
    /// A collected experiment's summary block, by its id (prefixed so it cannot collide).
    let summaryRow (experimentId : int) : string = "SolverHandoffRow_" + string experimentId
    /// A collected experiment's TYPED validation-status line, by its id.
    let statusRow (experimentId : int) : string = "SolverHandoffStatus_" + string experimentId

/// The typed per-experiment handoff validation status (spec 0038 Part L, step 039). GREEN
/// (`HandoffReady`) means every non-sample element is specified, a data file is attached, it reads
/// schema-valid, and its range is consistent — the point count rides along. Every other case is a
/// TYPED error carrying a diagnostic `reason` (a bare error case is useless on the summary). Elevated
/// — never a bare status string — with a `.text` projection the row renders and an `.isReady` guard.
type ExperimentHandoffStatus =
    | HandoffReady of points : int
    | UnspecifiedElements of reason : string
    | MissingDataFile of reason : string
    | DataFileParseError of reason : string
    | DataFileValidationError of reason : string

    member this.text : string =
        match this with
        | HandoffReady n -> $"Ready — %d{n} point(s), every element specified, schema-valid, range-consistent"
        | UnspecifiedElements reason -> $"Unspecified element(s) — %s{reason}"
        | MissingDataFile reason -> $"Missing data file — %s{reason}"
        | DataFileParseError reason -> $"Parse error — %s{reason}"
        | DataFileValidationError reason -> $"Validation error — %s{reason}"

    member this.isReady : bool =
        match this with
        | HandoffReady _ -> true
        | UnspecifiedElements _ | MissingDataFile _ | DataFileParseError _ | DataFileValidationError _ -> false

/// The point count of a validated measured series — the "N point(s)" the ready status reports (the
/// `TableAndElementRotationView.measuredSeriesPointCount` shape, re-derived here so this module needs
/// no workbench type).
let private measuredSeriesPointCount (series : MeasuredData.MeasuredSeries) : int =
    match series with
    | MeasuredData.IntensityData s -> List.length s.points
    | MeasuredData.EllipsometricData s -> List.length s.points

/// The non-sample elements of an experiment's setup that are still UNBOUND — the inverse sample is
/// the unknown (allowed unbound, drawn dashed at step 30), so it is excluded; every OTHER element
/// (source / detector / polarizer / …) MUST be specified before handoff.
let private unspecifiedElements (experiment : Experiments.Experiment) : Experiments.ElementDescriptor list =
    experiment.setup
    |> List.filter (fun d ->
        match d.kind, d.binding with
        | Placement.Sample, _ -> false
        | _, Experiments.Unbound -> true
        | _, (Experiments.BoundByEntryId _ | Experiments.BoundByVersion _) -> false)

/// Spec 0038 Part L (step 039): the BASIC per-experiment validation, three checks in order (no solver
/// math). (1) Every non-sample element specified — else `UnspecifiedElements`. (2) A data file
/// attached — else `MissingDataFile`. (3) The file reads schema-valid AND range-consistent through
/// `ExperimentDataLoad.loadAndValidate` (which resolves the detector kind, reads through the
/// `ExperimentDataProxy`, then folds in step-34 `validateAgainstExperiment`) — a parse / empty-file
/// failure is `DataFileParseError`, a range / units mismatch is `DataFileValidationError`, success is
/// `HandoffReady` with the point count. Pure given the proxies — the disk read lives behind the proxy,
/// so a test drives it over in-memory mocks with no real IO.
let validateExperiment
    (library : Library.LibraryProxy)
    (dataProxy : ExperimentData.ExperimentDataProxy)
    (experiment : Experiments.Experiment)
    : ExperimentHandoffStatus =
    match unspecifiedElements experiment with
    | (_ :: _) as unbound ->
        let names = unbound |> List.map (fun d -> d.label) |> String.concat ", "
        UnspecifiedElements $"every element except the sample must be specified; still unbound: %s{names}"
    | [] ->
        match experiment.dataFileOpt with
        | None -> MissingDataFile "no measured-data file is attached to this experiment"
        | Some path ->
            match ExperimentDataLoad.loadAndValidate library dataProxy experiment path with
            | Ok series -> HandoffReady (measuredSeriesPointCount series)
            | Error (MeasuredData.MalformedDataFile reason)
            | Error (MeasuredData.EmptyDataFile reason) -> DataFileParseError reason
            | Error (MeasuredData.DataRangeMismatch reason)
            | Error (MeasuredData.DataUnitsMismatch reason) -> DataFileValidationError reason

/// One summarized experiment: the received `Experiment` (its ordered setup + attached file ride
/// along), the resolved detector kind (fixes the expected data-file shape), and its typed validation
/// status. A pure projection — no live element reference, so the summary survives the session.
type HandoffRow =
    {
        experiment : Experiments.Experiment
        detector : Library.DetectorKind
        status : ExperimentHandoffStatus
    }

/// Build the per-experiment summary rows from the received experiments, validating each through the
/// injected proxies (pure given the proxies). This is the whole computation the screen performs —
/// basic validation only, no solver math.
let buildRows
    (library : Library.LibraryProxy)
    (dataProxy : ExperimentData.ExperimentDataProxy)
    (experiments : Experiments.Experiment list)
    : HandoffRow list =
    experiments
    |> List.map (fun e ->
        {
            experiment = e
            detector = ExperimentDataLoad.detectorKindOf library e
            status = validateExperiment library dataProxy e
        })

/// The screen's IO seam (the functional-proxy Context convention, the `CategoryEditorContext`
/// precedent): the read-only Library (detector-kind resolution) and the measured-data load proxy the
/// validation reads through, the RECEIVED collection snapshot, and the host's close request (the
/// window passes `this.Close`; a test substitutes a recording stub). Function-valued fields have no
/// structural equality, so the context compares by reference — the model holding it keeps its equality.
[<ReferenceEquality>]
type SolverHandoffContext =
    {
        library : Library.LibraryProxy
        experimentData : ExperimentData.ExperimentDataProxy
        collection : ExperimentCollectionStore.ExperimentCollectionSnapshot
        requestClose : unit -> unit
    }

/// The screen's model: the context, the received collection's name, and the validated summary rows.
/// Reference-compared — the context carries function fields (no structural equality) and the screen is
/// read-only (no state change beyond the one-shot close), so the Elmish equality gate is satisfied.
[<ReferenceEquality>]
type Model =
    {
        context : SolverHandoffContext
        collectionName : ExperimentCollectionStore.CollectionName
        rows : HandoffRow list
    }

/// Build the model from the context — validate every received experiment ONCE at init (the whole
/// computation the screen runs).
let init (context : SolverHandoffContext) : Model =
    {
        context = context
        collectionName = context.collection.name
        rows = buildRows context.library context.experimentData context.collection.experiments
    }

/// The only message the read-only terminal screen dispatches: the window-level Close.
type Msg =
    | RequestClose

let update (msg : Msg) (m : Model) : Model =
    match msg with
    | RequestClose -> m.context.requestClose (); m

// ---------------------------------------------------------------------------
// View — a pure projection. AutomationId (never Name) on the keyed per-experiment rows / status
// blocks so the id survives FuncUI recycling a control onto another slot (the CategoryEditorView
// precedent).
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private readyColor = color 34 128 34
let private errorColor = color 176 40 40
let private mutedColor = color 96 96 96

/// Set `AutomationProperties.AutomationId` (freely mutable, unlike `Control.Name`) through FuncUI's
/// attr builder.
let private automationId<'t when 't :> Control> (autoId : string) : IAttr<'t> =
    AttrBuilder<'t>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

let private detectorLabel (kind : Library.DetectorKind) : string =
    match kind with
    | Library.Intensity -> "Intensity"
    | Library.Ellipsometer -> "Ellipsometer"

/// One element of the ordered setup, labelled with whether it is specified (bound) or still an
/// unspecified unknown. The sample reads "the unknown (sample)"; every other unbound element reads
/// "UNSPECIFIED".
let private setupText (experiment : Experiments.Experiment) : string =
    experiment.setup
    |> List.map (fun d ->
        let tag =
            match d.kind, d.binding with
            | Placement.Sample, Experiments.Unbound -> "the unknown (sample)"
            | Placement.Sample, _ -> "specified"
            | _, Experiments.Unbound -> "UNSPECIFIED"
            | _, (Experiments.BoundByEntryId _ | Experiments.BoundByVersion _) -> "specified"
        $"%s{d.label} [%s{tag}]")
    |> String.concat "  →  "

let private fileText (experiment : Experiments.Experiment) : string =
    match experiment.dataFileOpt with
    | Some path -> System.IO.Path.GetFileName path.value
    | None -> "(no file attached)"

let private infoLine (text : string) : IView =
    TextBlock.create [
        TextBlock.text text
        TextBlock.textWrapping TextWrapping.Wrap
        TextBlock.foreground (brush mutedColor)
        TextBlock.margin (Thickness(0.0, 1.0, 0.0, 0.0))
    ] :> IView

/// One experiment's summary block: its description, its ordered setup, its detector kind, its attached
/// file, and the TYPED validation status (green when ready, red otherwise). Keyed + AutomationId'd by
/// the experiment id.
let private summaryEntry (row : HandoffRow) : IView =
    let e = row.experiment
    let idStr = string e.id.value
    StackPanel.create [
        automationId (UiIds.summaryRow e.id.value)
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(0.0, 0.0, 0.0, 12.0))
        StackPanel.children [
            TextBlock.create [
                TextBlock.text e.description
                TextBlock.fontWeight FontWeight.SemiBold
                TextBlock.textWrapping TextWrapping.Wrap
            ]
            infoLine $"Setup: %s{setupText e}"
            infoLine $"Detector: %s{detectorLabel row.detector}"
            infoLine $"Data file: %s{fileText e}"
            TextBlock.create [
                automationId (UiIds.statusRow e.id.value)
                TextBlock.text row.status.text
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.fontWeight FontWeight.SemiBold
                TextBlock.margin (Thickness(0.0, 3.0, 0.0, 0.0))
                TextBlock.foreground (brush (if row.status.isReady then readyColor else errorColor))
            ]
        ]
    ]
    |> Avalonia.FuncUI.DSL.View.withKey ("SolverHandoffRow_" + idStr)
    :> IView

let private summaryList (m : Model) : IView =
    let children : IView list =
        match m.rows with
        | [] -> [ TextBlock.create [ TextBlock.text "(the received collection holds no experiments)"; TextBlock.foreground (brush mutedColor) ] :> IView ]
        | rows -> rows |> List.map summaryEntry
    StackPanel.create [
        StackPanel.name UiIds.summaryList
        StackPanel.orientation Orientation.Vertical
        StackPanel.children children
    ] :> IView

let private closeButton (dispatch : Msg -> unit) : IView =
    Border.create [
        automationId UiIds.closeButton
        Border.background (brush (color 226 226 226))
        Border.borderBrush (brush (color 120 120 120))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(22.0, 6.0))
        Border.horizontalAlignment HorizontalAlignment.Right
        Border.child (TextBlock.create [ TextBlock.text "Close" ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; dispatch RequestClose), SubPatchOptions.OnChangeOf (box UiIds.closeButton))
    ] :> IView

/// The whole terminal screen: a header naming the received collection, the fixed solver-comes-later
/// message, the scrollable per-experiment summary list, and a Close row.
let view (m : Model) (dispatch : Msg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Top
                Border.padding (Thickness(12.0, 10.0, 12.0, 4.0))
                Border.child (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.spacing 2.0
                        StackPanel.children [
                            TextBlock.create [
                                automationId UiIds.collectionName
                                TextBlock.text $"Solver handoff — collection '%s{m.collectionName.value}'"
                                TextBlock.fontWeight FontWeight.Bold
                                TextBlock.fontSize 16.0
                            ]
                            TextBlock.create [
                                automationId UiIds.solverMessage
                                TextBlock.text solverComesLaterMessage
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.foreground (brush mutedColor)
                                TextBlock.margin (Thickness(0.0, 2.0, 0.0, 0.0))
                            ]
                        ]
                    ]
                )
            ]
            Border.create [
                Border.dock Dock.Bottom
                Border.padding (Thickness(12.0, 6.0))
                Border.child (closeButton dispatch)
            ]
            ScrollViewer.create [
                ScrollViewer.padding (Thickness(12.0, 6.0))
                ScrollViewer.content (summaryList m)
            ]
        ]
    ] :> IView

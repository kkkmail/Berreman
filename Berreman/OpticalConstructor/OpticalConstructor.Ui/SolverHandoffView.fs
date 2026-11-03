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
open OpticalConstructor.Controls
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain

/// The fixed message the terminal screen always shows (the how-to's "the actual solver will be done
/// separately"). A `[<Literal>]` so a test can assert against it and step 044's id-consolidation can
/// move it mechanically.
[<Literal>]
let solverComesLaterMessage =
    "The measured data is gathered as-is — no solving or normalization happens here. The actual inverse solver runs separately."

/// The typed per-experiment handoff validation status (spec 0038 Part L, step 039). GREEN
/// (`HandoffReady`) means every non-sample element is specified, a data file is attached, it reads
/// schema-valid, and its range is consistent — the point count rides along. Every other case is a
/// TYPED error carrying a diagnostic `reason` (a bare error case is useless on the summary).
type ExperimentHandoffStatus =
    | HandoffReady of points : MeasuredData.MeasuredPointCount
    | UnspecifiedElements of reason : ControlCaption
    | MissingDataFile of reason : ControlCaption
    | DataFileParseError of reason : ControlCaption
    | DataFileValidationError of reason : ControlCaption

    /// The status line the summary row shows.
    member this.caption : ControlCaption =
        match this with
        | HandoffReady n -> ControlCaption $"Ready — {n.value} point(s), every element specified, schema-valid, range-consistent"
        | UnspecifiedElements reason -> ControlCaption $"Unspecified element(s) — {reason.value}"
        | MissingDataFile reason -> ControlCaption $"Missing data file — {reason.value}"
        | DataFileParseError reason -> ControlCaption $"Parse error — {reason.value}"
        | DataFileValidationError reason -> ControlCaption $"Validation error — {reason.value}"

    member this.isReady : bool =
        match this with
        | HandoffReady _ -> true
        | UnspecifiedElements _ | MissingDataFile _ | DataFileParseError _ | DataFileValidationError _ -> false

type Experiments.Experiment with

    /// The non-sample elements of the setup that are still UNBOUND — the inverse sample is the unknown
    /// (allowed unbound, drawn dashed at step 30), so it is excluded; every OTHER element (source /
    /// detector / polarizer / …) MUST be specified before handoff.
    member private this.unspecifiedElements : Experiments.ElementDescriptor list =
        this.setup
        |> List.filter (fun d ->
            match d.kind, d.binding with
            | Placement.Sample, _ -> false
            | _, Experiments.Unbound -> true
            | _, (Experiments.BoundByEntryId _ | Experiments.BoundByVersion _) -> false)

    /// The ordered setup, each element labelled with whether it is specified (bound) or still an
    /// unspecified unknown: the sample reads "the unknown (sample)", every other unbound element
    /// "UNSPECIFIED".
    member private this.handoffSetupCaption : ControlCaption =
        this.setup
        |> List.map (fun d ->
            let tag =
                match d.kind, d.binding with
                | Placement.Sample, Experiments.Unbound -> "the unknown (sample)"
                | Placement.Sample, _ -> "specified"
                | _, Experiments.Unbound -> "UNSPECIFIED"
                | _, (Experiments.BoundByEntryId _ | Experiments.BoundByVersion _) -> "specified"
            $"{d.label.value} [{tag}]")
        |> String.concat "  →  "
        |> ControlCaption

    /// The attached data file's name, or a note that none is attached.
    member private this.handoffFileCaption : ControlCaption =
        match this.dataFileOpt with
        | Some path -> ControlCaption (System.IO.Path.GetFileName path.value)
        | None -> ControlCaption "(no file attached)"

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
    match experiment.unspecifiedElements with
    | (_ :: _) as unbound ->
        let names = unbound |> List.map (fun d -> d.label.value) |> String.concat ", "
        UnspecifiedElements (ControlCaption $"every element except the sample must be specified; still unbound: {names}")
    | [] ->
        match experiment.dataFileOpt with
        | None -> MissingDataFile (ControlCaption "no measured-data file is attached to this experiment")
        | Some path ->
            match ExperimentDataLoad.loadAndValidate library dataProxy experiment path with
            | Ok series -> HandoffReady series.pointCount
            | Error ((MeasuredData.MalformedDataFileErr _ | MeasuredData.EmptyDataFileErr _) as err) -> DataFileParseError err.caption
            | Error ((MeasuredData.DataRangeMismatchErr _ | MeasuredData.DataUnitsMismatchErr _) as err) -> DataFileValidationError err.caption

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
type SolverHandoffModel =
    {
        context : SolverHandoffContext
        collectionName : ExperimentCollectionStore.CollectionName
        rows : HandoffRow list
    }

type SolverHandoffContext with

    /// The model over this context: every received experiment validated ONCE at init (the whole
    /// computation the screen runs).
    member this.initSolverHandoff : SolverHandoffModel =
        {
            context = this
            collectionName = this.collection.name
            rows = buildRows this.library this.experimentData this.collection.experiments
        }

/// The only message the read-only terminal screen dispatches: the window-level Close.
type SolverHandoffMsg =
    | RequestClose

let updateSolverHandoff (msg : SolverHandoffMsg) (m : SolverHandoffModel) : SolverHandoffModel =
    match msg with
    | RequestClose -> m.context.requestClose (); m

// ---------------------------------------------------------------------------
// View — a pure projection. AutomationId (never Name) on the keyed per-experiment rows / status
// blocks so the id survives FuncUI recycling a control onto another slot (the CategoryEditorView
// precedent).
// ---------------------------------------------------------------------------

let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private readyColor = Color.FromRgb(34uy, 128uy, 34uy)
let private errorColor = Color.FromRgb(176uy, 40uy, 40uy)
let private mutedColor = Color.FromRgb(96uy, 96uy, 96uy)
let private closeBackground = Color.FromRgb(226uy, 226uy, 226uy)
let private closeBorder = Color.FromRgb(120uy, 120uy, 120uy)

let private infoLine (text : ControlCaption) : IView =
    TextBlock.create [
        TextBlock.text text.value
        TextBlock.textWrapping TextWrapping.Wrap
        TextBlock.foreground (brush mutedColor)
        TextBlock.margin (Thickness(0.0, 1.0, 0.0, 0.0))
    ] :> IView

/// One experiment's summary block: its description, its ordered setup, its detector kind, its attached
/// file, and the TYPED validation status (green when ready, red otherwise). Keyed + AutomationId'd by
/// the experiment id.
let private summaryEntry (row : HandoffRow) : IView =
    let e = row.experiment
    let rowId = ConstructorUiId.solverHandoffRow e.id
    StackPanel.create [
        rowId.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(0.0, 0.0, 0.0, 12.0))
        StackPanel.children [
            TextBlock.create [
                TextBlock.text e.description
                TextBlock.fontWeight FontWeight.SemiBold
                TextBlock.textWrapping TextWrapping.Wrap
            ]
            infoLine (ControlCaption $"Setup: {e.handoffSetupCaption.value}")
            infoLine (ControlCaption $"Detector: {row.detector.caption.value}")
            infoLine (ControlCaption $"Data file: {e.handoffFileCaption.value}")
            TextBlock.create [
                (ConstructorUiId.solverHandoffStatus e.id).automationAttribute<TextBlock> ()
                TextBlock.text row.status.caption.value
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.fontWeight FontWeight.SemiBold
                TextBlock.margin (Thickness(0.0, 3.0, 0.0, 0.0))
                TextBlock.foreground (brush (if row.status.isReady then readyColor else errorColor))
            ]
        ]
    ]
    |> Avalonia.FuncUI.DSL.View.withKey rowId.value
    :> IView

let private summaryList (m : SolverHandoffModel) : IView =
    let children : IView list =
        match m.rows with
        | [] -> [ TextBlock.create [ TextBlock.text "(the received collection holds no experiments)"; TextBlock.foreground (brush mutedColor) ] :> IView ]
        | rows -> rows |> List.map summaryEntry
    StackPanel.create [
        StackPanel.name ConstructorUiId.solverHandoffSummaryList.value
        StackPanel.orientation Orientation.Vertical
        StackPanel.children children
    ] :> IView

let private closeButton (dispatch : SolverHandoffMsg -> unit) : IView =
    Border.create [
        ConstructorUiId.solverHandoffCloseButton.automationAttribute<Border> ()
        Border.background (brush closeBackground)
        Border.borderBrush (brush closeBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(22.0, 6.0))
        Border.horizontalAlignment HorizontalAlignment.Right
        Border.child (TextBlock.create [ TextBlock.text "Close" ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; dispatch RequestClose), SubPatchOptions.OnChangeOf (box ConstructorUiId.solverHandoffCloseButton.value))
    ] :> IView

/// The whole terminal screen: a header naming the received collection, the fixed solver-comes-later
/// message, the scrollable per-experiment summary list, and a Close row.
let solverHandoffView (m : SolverHandoffModel) (dispatch : SolverHandoffMsg -> unit) : IView =
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
                                ConstructorUiId.solverHandoffCollectionName.automationAttribute<TextBlock> ()
                                TextBlock.text $"Solver handoff — collection '{m.collectionName.value}'"
                                TextBlock.fontWeight FontWeight.Bold
                                TextBlock.fontSize 16.0
                            ]
                            TextBlock.create [
                                ConstructorUiId.solverHandoffSolverMessage.automationAttribute<TextBlock> ()
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

/// The inverse flow's terminal screen (UICOMP_XDUO_0011): a received collection summarized per experiment, its setup
/// revision's slots in light order, its detector, its data file and a typed validation status; nothing is solved here
/// (spec 0038 Part L step 039; spec 0050 S8.2).
module OpticalConstructor.Ui.SolverHandoffView

open Avalonia
open OpticalConstructor.Controls
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Setups

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

/// How the hand-off reads a slot: specified by its item or its binding, the inverse sample left unknown, or another
/// slot still unspecified (spec 0038 Part L; spec 0050 S8.2).
type private HandoffSlotState =
    | HandoffSpecifiedSlot
    | HandoffUnknownSample
    | HandoffUnspecifiedSlot

    /// The tag the setup line shows beside the slot's role.
    member this.text : string =
        match this with
        | HandoffSpecifiedSlot -> "specified"
        | HandoffUnknownSample -> "the unknown (sample)"
        | HandoffUnspecifiedSlot -> "UNSPECIFIED"

type SetupSlot with

    /// A slot holding an item or bound by its placement is specified; an empty, unbound `Sample` slot is the unknown the
    /// solver finds, any other such slot is unspecified (spec 0038 Part L; spec 0050 S8.2).
    member private this.handoffState : HandoffSlotState =
        match this.content, this.placement.valueId, this.placement.catalogueKind with
        | ItemInSlot _, _, _
        | EmptySlotContent, Some _, _ -> HandoffSpecifiedSlot
        | EmptySlotContent, None, Placement.Sample -> HandoffUnknownSample
        | EmptySlotContent, None, _ -> HandoffUnspecifiedSlot

type Experiments.Experiment with

    /// The setup revision's slots in light order, each as `<role> [<state>]`, or why the revision does not resolve
    /// (spec 0050 S8.2).
    member private this.handoffSetupCaption (setups : Setup list) : ControlCaption =
        match this.slotsIn setups with
        | Ok slots ->
            SetupSlot.inLightOrder slots
            |> List.map (fun roled -> $"{roled.role.text} [{roled.slot.handoffState.text}]")
            |> String.concat "  →  "
            |> ControlCaption
        | Error error -> ControlCaption error.describe

    /// The attached data file's kit-relative path, or a note that none is attached.
    member private this.handoffFileCaption : ControlCaption =
        match this.dataFileOpt with
        | Some attachment -> attachment.path.toControlCaption
        | None -> ControlCaption "(no file attached)"

/// The basic validation, in order and with no solver math: the setup revision resolves among `setups` and every slot but
/// the sample is specified (`UnspecifiedElements`), a data file is attached (`MissingDataFile`), and it reads and fits the
/// range through `ExperimentDataLoad.loadAndValidate` (spec 0038 Part L step 039; spec 0050 S8.2).
let validateExperiment
    (library : Library.LibraryProxy)
    (dataProxy : ExperimentData.ExperimentDataProxy)
    (setups : Setup list)
    (experiment : Experiments.Experiment)
    : ExperimentHandoffStatus =
    match experiment.slotsIn setups with
    | Error error -> UnspecifiedElements (ControlCaption $"its setup is not in the collection: {error.describe}")
    | Ok slots ->
        let unbound =
            SetupSlot.inLightOrder slots
            |> List.filter (fun roled ->
                match roled.slot.handoffState with
                | HandoffUnspecifiedSlot -> true
                | HandoffSpecifiedSlot
                | HandoffUnknownSample -> false)
        match unbound with
        | _ :: _ ->
            let names = unbound |> List.map (fun roled -> roled.role.text) |> String.concat ", "
            UnspecifiedElements (ControlCaption $"every element except the sample must be specified; still unbound: {names}")
        | [] ->
            match experiment.dataFileOpt with
            | None -> MissingDataFile (ControlCaption "no measured-data file is attached to this experiment")
            | Some attachment ->
                match ExperimentDataLoad.loadAndValidate library dataProxy slots experiment attachment with
                | Ok series -> HandoffReady series.pointCount
                | Error ((MeasuredData.MalformedDataFileErr _ | MeasuredData.EmptyDataFileErr _) as err) -> DataFileParseError err.caption
                | Error ((MeasuredData.DataRangeMismatchErr _ | MeasuredData.DataUnitsMismatchErr _) as err) -> DataFileValidationError err.caption

/// One summarized experiment: the received `Experiment` (its setup reference and attached file ride along), the detector
/// kind its slots resolve to (fixing the expected data-file shape), and its typed validation status (spec 0050 S8.2).
type HandoffRow =
    {
        experiment : Experiments.Experiment
        detector : Library.DetectorKind
        status : ExperimentHandoffStatus
    }

/// The summary rows of the received experiments over the setups they reference, each validated through the injected
/// proxies; an unresolved revision reads no detector, so `Intensity` (spec 0038 Part L; spec 0050 S8.2).
let buildRows
    (library : Library.LibraryProxy)
    (dataProxy : ExperimentData.ExperimentDataProxy)
    (setups : Setup list)
    (experiments : Experiments.Experiment list)
    : HandoffRow list =
    experiments
    |> List.map (fun e ->
        {
            experiment = e
            detector = ExperimentDataLoad.detectorKindOf library (e.slotsIn setups |> Result.defaultValue [])
            status = validateExperiment library dataProxy setups e
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
            rows = buildRows this.library this.experimentData this.collection.setups this.collection.experiments
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

/// One experiment's summary block: its description, its setup revision's slots, its detector kind, its attached
/// file, and the TYPED validation status (green when ready, red otherwise). Keyed + AutomationId'd by
/// the experiment id.
let private summaryEntry (setups : Setup list) (row : HandoffRow) : IView =
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
            infoLine (ControlCaption $"Setup: {(e.handoffSetupCaption setups).value}")
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
        | rows -> rows |> List.map (summaryEntry m.context.collection.setups)
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

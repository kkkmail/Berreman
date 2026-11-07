/// The import decision dialog's MVU and projection (spec 0050 S3.2, S4.2; 001 §B, §E).
/// Choices remain pending until Apply confirms the complete request.
module OpticalConstructor.Ui.ImportDialogView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.EntryTextDiff
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Ui.MaterialsWindowView

/// The pending import whose rows the dialog confirms.
type ImportDialogRequest =
    /// A changed catalogue awaiting settlement and import-log recording.
    | SettlePendingCatalogue of PendingCatalogueImport
    /// Entry files identified by the drop seam without writing them.
    | ApplyDroppedEntries of decisions : ImportDecision list
    /// A kit's snapshot rows awaiting confirmation before its library is opened (spec 0050 S4.2).
    | ResolveKitSnapshot of kit : KitLibraryContext * decisions : ImportDecision list

    /// The decision rows in the request's original order.
    member this.decisions : ImportDecision list =
        match this with
        | SettlePendingCatalogue pending -> pending.decisions
        | ApplyDroppedEntries decisions -> decisions
        | ResolveKitSnapshot (_, decisions) -> decisions

    /// The request's origin and number of rows awaiting confirmation.
    member this.heading : ControlCaption =
        match this with
        | SettlePendingCatalogue pending ->
            ControlCaption $"Import from {pending.entry.folder.value}: {pending.decisions.Length} entries need a decision."
        | ApplyDroppedEntries decisions -> ControlCaption $"Import {decisions.Length} dropped entry files."
        | ResolveKitSnapshot (kit, decisions) ->
            ControlCaption $"Open kit {kit.kit.kitFile.name.value}: {decisions.Length} library entries need a decision."

/// A completed Apply, returned to the opener before the dialog closes.
type ImportDialogApplied =
    {
        /// The request that was confirmed.
        request : ImportDialogRequest
        /// Every row's confirmed choice, in dialog order.
        choices : ImportChoice list
        /// The import seam's resulting rebindings and forks.
        outcome : ImportOutcome
    }

    /// The shared status text shown by any opener after a successful Apply.
    member this.summary : ControlCaption =
        match this.request with
        | SettlePendingCatalogue pending ->
            ControlCaption $"Imported {pending.entry.folder.value} (decisions applied: {this.choices.Length})."
        | ApplyDroppedEntries _ ->
            ControlCaption $"Imported the dropped entry files (decisions applied: {this.choices.Length})."
        | ResolveKitSnapshot (kit, _) ->
            ControlCaption $"Opened the library of kit {kit.kit.kitFile.name.value} (decisions applied: {this.choices.Length})."

/// The composition root supplies import behavior and the opener's callbacks.
[<ReferenceEquality>]
type ImportDialogContext =
    {
        /// The existing import seam over the shared library stores.
        imports : CatalogueImportContext
        /// The pending work this dialog decides.
        request : ImportDialogRequest
        /// Reports successful application to the opener.
        onApplied : ImportDialogApplied -> unit
        /// Closes the owning window after success or cancellation.
        requestClose : unit -> unit
    }

/// At most one row exposes its payload comparison.
type ImportDiffState =
    /// Every row's comparison is collapsed.
    | NoImportDiffShown
    /// The identified row's comparison is expanded.
    | ImportDiffShown of LibraryEntryId

/// The complete import-dialog state, independent of a window.
type ImportDialogModel =
    {
        /// The injected import behavior and window callbacks.
        context : ImportDialogContext
        /// Pending actions in request order.
        choices : ImportChoice list
        /// The currently expanded comparison, if any.
        diff : ImportDiffState
        /// The latest Apply refusal, retained until a decision changes or Apply succeeds.
        lastError : CatalogueImportError option
    }

/// Semantic actions shared by the controls and model tests.
type ImportDialogMsg =
    /// Restores the identified row's default action.
    | DecideImportRow of LibraryEntryId
    /// Chooses one of the identified row's offered alternatives.
    | OverrideImportRow of LibraryEntryId * ImportAction
    /// Expands this row's diff, or collapses it if already shown.
    | ToggleImportDiff of LibraryEntryId
    /// Applies every choice through the import seam.
    | ApplyImportChoices
    /// Closes without importing anything.
    | CancelImportDialog

type ImportDialogContext with

    /// Opens with the decision table's defaults without touching the library.
    member this.initImportDialog : ImportDialogModel =
        {
            context = this
            choices = this.request.decisions |> List.map _.byDefault
            diff = NoImportDiffShown
            lastError = None
        }

/// Updates pending choices; only Apply writes through the import seam.
let updateImportDialog (msg : ImportDialogMsg) (m : ImportDialogModel) : ImportDialogModel =
    match msg with
    | DecideImportRow id ->
        {
            m with
                choices = m.choices |> List.map (fun choice -> if choice.decision.entryId = id then choice.decision.byDefault else choice)
                lastError = None
        }
    | OverrideImportRow (id, action) ->
        {
            m with
                choices =
                    m.choices |> List.map (fun choice ->
                        if choice.decision.entryId = id && List.contains action choice.decision.alternatives then
                            { choice with action = action }
                        else choice)
                lastError = None
        }
    | ToggleImportDiff id ->
        let diff =
            match m.diff with
            | ImportDiffShown shown when shown = id -> NoImportDiffShown
            | _ -> ImportDiffShown id
        { m with diff = diff }
    | ApplyImportChoices ->
        let result =
            match m.context.request with
            | SettlePendingCatalogue pending -> m.context.imports.settle pending m.choices
            | ApplyDroppedEntries _ -> m.context.imports.apply m.choices
            | ResolveKitSnapshot (kit, _) -> kit.confirm m.choices
        match result with
        | Ok outcome ->
            m.context.onApplied
                {
                    request = m.context.request
                    choices = m.choices
                    outcome = outcome
                }
            m.context.requestClose ()
            { m with lastError = None }
        | Error error -> { m with lastError = Some error }
    | CancelImportDialog ->
        m.context.requestClose ()
        m

type ImportChoice with

    /// The incoming envelope and held identity/version selected by this row's comparison policy.
    member private this.diffBase : (LibraryEntryEnvelope * LibraryEntryId * VersionNumber) option =
        match this.decision.status with
        | DivergentContent (incoming, held) ->
            let version =
                if held.versions |> List.exists (fun version -> version.version = incoming.content.version) then incoming.content.version
                else held.latest
            Some (incoming, held.id, version)
        | VersionNotHeld (incoming, held) -> Some (incoming, held.id, held.latest)
        | NameCollision (incoming, sameName) ->
            let heldOpt =
                match this.action with
                | RebindToHeldEntry id -> sameName |> List.tryFind (fun held -> held.id = id)
                | _ -> List.tryHead sameName
            heldOpt |> Option.map (fun held -> incoming, held.id, held.latest)
        | EntryIdNotHeld _ | SameContentHash _ | EntryUnlisted _ -> None

type EntryDiffLine with

    /// Renders a canonical comparison line with its change prefix and colour.
    member private this.importDiffLineView : IView =
        TextBlock.create [
            TextBlock.textWrapping TextWrapping.Wrap
            match this with
            | SameEntryLine line -> TextBlock.text $"  {line.value}"
            | RemovedEntryLine line ->
                TextBlock.text $"- {line.value}"
                TextBlock.foreground (brush messageColor)
            | AddedEntryLine line ->
                TextBlock.text $"+ {line.value}"
                TextBlock.foreground (brush (Color.FromRgb(0uy, 110uy, 0uy)))
        ] :> IView

let private importDiffLines (imports : CatalogueImportContext) (incoming : LibraryEntryEnvelope)
    (heldId : LibraryEntryId) (version : VersionNumber) : IView list =
    let message (caption : ControlCaption) : IView list =
        [ TextBlock.create [ TextBlock.text caption.value; TextBlock.textWrapping TextWrapping.Wrap ] :> IView ]
    match imports.proxy.tryHeldEnvelope heldId version with
    | Error error -> message error.caption
    | Ok None -> message (ControlCaption "The held version is not in the library.")
    | Ok (Some held) ->
        match incoming.diffFrom held with
        | Error error -> message (ControlCaption error.describe)
        | Ok lines -> lines |> List.map _.importDiffLineView

let private importDecisionView (imports : CatalogueImportContext) (diff : ImportDiffState)
    (dispatch : ImportDialogMsg -> unit) (choice : ImportChoice) : IView =
    let decision = choice.decision
    let entry = decision.entryId.uiKey
    let version =
        match decision.status with
        | EntryUnlisted held -> held.latest
        | EntryIdNotHeld incoming | SameContentHash (incoming, _) | VersionNotHeld (incoming, _)
        | DivergentContent (incoming, _) | NameCollision (incoming, _) -> incoming.content.version
    let rowId = ConstructorUiId.importDecisionRow entry
    StackPanel.create [
        rowId.automationAttribute<StackPanel> ()
        StackPanel.margin (Thickness(0.0, 0.0, 0.0, 16.0))
        StackPanel.children [
            TextBlock.create [
                TextBlock.text $"{decision.rowName.value} — {decision.kind.caption.value} — version {version.value} — {decision.status.caption.value}"
                TextBlock.textWrapping TextWrapping.Wrap
            ]
            TextBlock.create [
                TextBlock.text $"Default: {(decision.actionCaption decision.defaultAction).value}"
                TextBlock.textWrapping TextWrapping.Wrap
            ]
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    decision.defaultAction :: decision.alternatives
                    |> List.map (fun action ->
                        let id = ConstructorUiId.importActionOption entry action.uiKey
                        let label = decision.actionCaption action
                        let onClick () =
                            if action = decision.defaultAction then dispatch (DecideImportRow decision.entryId)
                            else dispatch (OverrideImportRow (decision.entryId, action))
                        if action = choice.action then actionButton id label positiveBackground onClick
                        else verbButton id label onClick))
            ]
            match choice.diffBase with
            | None -> ()
            | Some (incoming, heldId, heldVersion) ->
                let label =
                    match diff with
                    | ImportDiffShown id when id = decision.entryId -> ControlCaption "Hide diff"
                    | _ -> ControlCaption "Show diff"
                verbButton (ConstructorUiId.importDiffToggle entry) label (fun () -> dispatch (ToggleImportDiff decision.entryId))
                match diff with
                | ImportDiffShown id when id = decision.entryId ->
                    StackPanel.create [
                        (ConstructorUiId.importDiffPreview entry).automationAttribute<StackPanel> ()
                        StackPanel.children (importDiffLines imports incoming heldId heldVersion)
                    ]
                    |> View.withKey (ConstructorUiId.importDiffPreview entry).value
                    :> IView
                | _ -> ()
        ]
    ]
    |> View.withKey rowId.value
    :> IView

/// The scrollable decisions with a fixed heading, refusal and Apply/Cancel action row.
let importDialogView (m : ImportDialogModel) (dispatch : ImportDialogMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.margin 12.0
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Bottom
                StackPanel.orientation Orientation.Horizontal
                StackPanel.children [
                    actionButton ConstructorUiId.importApplyButton (ControlCaption "Apply") positiveBackground (fun () -> dispatch ApplyImportChoices)
                    actionButton ConstructorUiId.importCancelButton (ControlCaption "Cancel") negativeBackground (fun () -> dispatch CancelImportDialog)
                ]
            ]
            match m.lastError with
            | Some error ->
                TextBlock.create [
                    ConstructorUiId.importDialogMessage.automationAttribute<TextBlock> ()
                    TextBlock.dock Dock.Bottom
                    TextBlock.text error.caption.value
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.foreground (brush messageColor)
                ]
                |> View.withKey ConstructorUiId.importDialogMessage.value
                :> IView
            | None -> ()
            TextBlock.create [
                ConstructorUiId.importDialogHeading.automationAttribute<TextBlock> ()
                TextBlock.dock Dock.Top
                TextBlock.text $"{m.context.request.heading.value}\nNothing is imported until you press Apply."
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.margin (Thickness(0.0, 0.0, 0.0, 12.0))
            ]
            |> View.withKey ConstructorUiId.importDialogHeading.value
            ScrollViewer.create [
                ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Disabled
                ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                ScrollViewer.content (
                    StackPanel.create [
                        ConstructorUiId.importDecisionList.automationAttribute<StackPanel> ()
                        StackPanel.children (m.choices |> List.map (importDecisionView m.context.imports m.diff dispatch))
                    ])
            ]
        ]
    ] :> IView

namespace OpticalConstructor.Ui

open Berreman.Dispersion
open OpticalConstructor.Domain
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.FacetBuckets
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.WorkbenchSettings

/// The Controls values derived from a Domain value: the automation ids (spec 0048 R1) and a control's
/// keys, counts and captions. Controls references no Domain type, so they are extension members here,
/// the lowest project that sees both.
[<AutoOpen>]
module ConstructorUiIdOfDomain =

    type LibraryEntryId with

        /// The entry's GUID text as its import-row key.
        member this.uiKey : ConstructorUiKey = ConstructorUiKey (string this.value)

    type LibraryEntryKind with

        /// The entry kind shown in an import row.
        member this.caption : ControlCaption =
            match this with
            | MaterialEntryKind -> ControlCaption "Material"
            | SampleEntryKind -> ControlCaption "Sample"
            | SourceEntryKind -> ControlCaption "Source"
            | PolarizerEntryKind -> ControlCaption "Polarizer"
            | DetectorEntryKind -> ControlCaption "Detector"
            | CategoryEntryKind -> ControlCaption "Category"
            | ItemModelEntryKind -> ControlCaption "Item model"

    type ImportStatus with

        /// The decision-table status shown before an import is confirmed.
        member this.caption : ControlCaption =
            match this with
            | EntryIdNotHeld _ -> ControlCaption "New entry"
            | SameContentHash _ -> ControlCaption "Already in the library"
            | VersionNotHeld _ -> ControlCaption "New version of a held entry"
            | DivergentContent _ -> ControlCaption "Differs from the held version"
            | NameCollision _ -> ControlCaption "Same name as a held entry"
            | EntryUnlisted _ -> ControlCaption "No longer in its catalogue"

    type ImportAction with

        /// The action's stable import-option key, including a rebind target's identity.
        member this.uiKey : ConstructorUiKey =
            match this with
            | AddIncomingEntry -> ConstructorUiKey "add"
            | ReuseHeldEntry -> ConstructorUiKey "reuse"
            | AddIncomingVersion MoveLatestVersion -> ConstructorUiKey "version-latest"
            | AddIncomingVersion KeepLatestVersion -> ConstructorUiKey "version-kept"
            | ForkIncomingEntry -> ConstructorUiKey "fork"
            | RebindToHeldEntry id -> ConstructorUiKey $"rebind-{id.value}"
            | SkipIncomingEntry -> ConstructorUiKey "skip"
            | RetireHeldEntry -> ConstructorUiKey "retire"

    type ImportDecision with

        /// Names an offered action by origin, including the held name for rebinding (001 §B, §E).
        member this.actionCaption (action : ImportAction) : ControlCaption =
            match this.origin, action with
            | KitSnapshotOrigin _, SkipIncomingEntry -> ControlCaption "Keep the kit's copy (read-only)"
            | KitSnapshotOrigin _, AddIncomingEntry ->
                match this.status with
                | NameCollision _ -> ControlCaption "Add to my library, side by side"
                | _ -> ControlCaption "Add to my library"
            | KitSnapshotOrigin _, AddIncomingVersion _ -> ControlCaption "Add this version to my library"
            | KitSnapshotOrigin _, ForkIncomingEntry -> ControlCaption "Add to my library as a fork"
            | _, AddIncomingEntry ->
                match this.status with
                | NameCollision _ -> ControlCaption "Side by side"
                | _ -> ControlCaption "Add"
            | _, ReuseHeldEntry -> ControlCaption "Reuse the held entry"
            | _, AddIncomingVersion MoveLatestVersion -> ControlCaption "Add version, made latest"
            | _, AddIncomingVersion KeepLatestVersion -> ControlCaption "Add version, latest kept"
            | _, ForkIncomingEntry -> ControlCaption "Fork as a new entry"
            | _, RebindToHeldEntry id ->
                match this.status with
                | NameCollision (_, sameName) ->
                    match sameName |> List.tryFind (fun held -> held.id = id) with
                    | Some held -> ControlCaption $"Use my {held.name.value} instead"
                    | None -> ControlCaption "Use the held entry instead"
                | _ -> ControlCaption "Use the held entry instead"
            | _, SkipIncomingEntry -> ControlCaption "Skip"
            | _, RetireHeldEntry -> ControlCaption "Mark inactive"

    type CatalogueId with

        /// The catalogue's identity as its Library-window row key.
        member this.uiKey : ConstructorUiKey = ConstructorUiKey (string this.value)

    type CatalogueImportError with

        /// The import refusal as the Library-window message.
        member this.caption : ControlCaption = ControlCaption this.describe

    type LibraryExportError with

        /// The library-export refusal shown by either library window.
        member this.caption : ControlCaption = ControlCaption this.describe

    type LibraryEntryEnvelope with

        /// The exported entry, destination and newly pinned version shown by either library window.
        member this.exportedCaption (file : FileName) : ControlCaption =
            ControlCaption $"Exported {this.content.name.value} version {this.content.version.value} to {file.value}; that version is now pinned."

    type CatalogueData with

        /// The exported catalogue's entry count, destination and pinned versions shown by either library window.
        member this.exportedCaption (folder : FolderName) : ControlCaption =
            ControlCaption $"Exported {this.envelopes.Length} entries to the catalogue folder {folder.value}; their versions are now pinned."

    type LibraryBackupName with

        /// The backup's name as its Library-window row key.
        member this.uiKey : ConstructorUiKey = ConstructorUiKey this.value

        /// The backup's file name as the Library-window row caption.
        member this.caption : ControlCaption = ControlCaption this.value

    type LibraryBackupError with

        /// The backup or restore refusal as the Library-window message.
        member this.caption : ControlCaption = ControlCaption this.describe

    type MaterialId with

        /// The material as its Materials-window corpus leaf's tree-node code (`entry:<guid>`).
        member this.materialsEntryCode : ConstructorUiKey = ConstructorUiKey $"entry:{this.value}"

    type OpticalConstructor.Domain.Placement.ElementValueId with

        /// The Library entry as the key of its Select-bay leaf row.
        member this.uiKey : ConstructorUiKey = ConstructorUiKey this.value

        /// The entry as its Library-window corpus leaf's tree-node code (`entry:<entryId>`).
        member this.libraryEntryCode : ConstructorUiKey = ConstructorUiKey $"entry:{this.value}"

    type NumericRange with

        /// The range as the code of its offered bucket and tree branch: round-trip endpoints joined by ':',
        /// a separator no number's text contains (an exponent's '-' could split a hyphen scheme).
        member this.uiKey : ConstructorUiKey =
            let part (v : UnitMagnitude) : string = v.value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
            ConstructorUiKey $"{part this.lower}:{part this.upper}"

        /// The range as the faceted tree shows it: its bucket label without the count, so the nm / µm
        /// switch has one home (`NumericBucket.label`).
        member this.caption : ControlCaption =
            let labelled = ({ range = this; count = ItemCount 0 } : NumericBucket).label
            match labelled.LastIndexOf " (" with
            | index when index > 0 -> ControlCaption (labelled.Substring(0, index))
            | _ -> ControlCaption labelled

    type Setups.SlotRole with

        /// The role as the key of its slot-bar ids: its `text` without spaces (`Source`, `Prep1`, `Obj2`) (spec 0050 S8.4).
        member this.uiKey : ConstructorUiKey = ConstructorUiKey (this.text.Replace(" ", ""))

    type ConstructorUiId with

        /// Draws this kit setup on the table (spec 0050 S8.2).
        static member savedSetup (name : Kit.SetupName) : ConstructorUiId = ConstructorUiId $"SavedSetup_{name.value}"

        /// An element's invisible hover target on the Main table, which carries its tooltip (spec 0046).
        static member elementHover (elementId : ElementId) : ConstructorUiId = ConstructorUiId $"ElementHover_{elementId.value}"

        /// A slot's chip on the slot bar: `SlotChip_<role.uiKey>` (spec 0050 S8.4; 001 §O "Automation").
        static member slotChip (role : Setups.SlotRole) : ConstructorUiId = ConstructorUiId $"SlotChip_{role.uiKey.value}"

        /// An item of a chip's open list: `SlotChipChoice_<role.uiKey>_<the item's GUID>` (spec 0050 S8.4).
        static member slotChipChoice (role : Setups.SlotRole) (item : Kit.ItemId) : ConstructorUiId =
            ConstructorUiId $"SlotChipChoice_{role.uiKey.value}_{item.value}"

        /// The element menu's "Place on the reflected ray of" this element (spec 0046).
        static member elementMenuPlaceOnReflectedOf (elementId : ElementId) : ConstructorUiId =
            ConstructorUiId $"ElementMenuPlaceOnReflectedOf_{elementId.value}"

        /// A collected experiment's summary block and its typed validation-status line (`SolverHandoffView`).
        static member solverHandoffRow (experimentId : ExperimentId) : ConstructorUiId = ConstructorUiId $"SolverHandoffRow_{experimentId.value}"

        static member solverHandoffStatus (experimentId : ExperimentId) : ConstructorUiId = ConstructorUiId $"SolverHandoffStatus_{experimentId.value}"

        /// A collected experiment's "Attach data file…" verb and its inline parse / validation status (the
        /// Main workbench's collection builder).
        static member attachDataFile (experimentId : ExperimentId) : ConstructorUiId = ConstructorUiId $"AttachDataFile_{experimentId.value}"

        static member dataFileStatus (experimentId : ExperimentId) : ConstructorUiId = ConstructorUiId $"DataFileStatus_{experimentId.value}"

        /// The copy panel's toggle choosing this experiment of the source kit (spec 0050 S6).
        static member kitCopyExperimentToggle (experimentId : ExperimentId) : ConstructorUiId =
            ConstructorUiId $"KitCopyExperimentToggle_{experimentId.value}"

        /// The mapping table's choice for a source item: `_Add`, or `_<the kit item's GUID>` it maps onto (spec 0050 S7.2).
        static member kitCopyItemChoice (sourceItem : Kit.ItemId) (choice : Kit.ItemCopyChoice) : ConstructorUiId =
            match choice with
            | Kit.AddCopiedItem -> ConstructorUiId $"KitCopyItemChoice_{sourceItem.value}_Add"
            | Kit.MapOntoKitItem kitItem -> ConstructorUiId $"KitCopyItemChoice_{sourceItem.value}_{kitItem.value}"

        /// A material-category option of the Material editor.
        static member materialEditorCategoryOption (categoryId : CategoryId) : ConstructorUiId =
            ConstructorUiId $"MaterialCategoryOption_{categoryId.value}"

        /// A material's corpus leaf in the Materials window's faceted tree.
        static member materialsEntryNode (materialId : MaterialId) : ConstructorUiId = ConstructorUiId.facetTreeNode materialId.materialsEntryCode

        /// One stored version's row in the Materials window's version list.
        static member materialsVersionRow (version : VersionNumber) : ConstructorUiId = ConstructorUiId $"MaterialsVersionRow_{version.value}"

        /// A Library entry's corpus leaf in the Library window's faceted tree.
        static member libraryEntryNode (entryId : OpticalConstructor.Domain.Placement.ElementValueId) : ConstructorUiId =
            ConstructorUiId.facetTreeNode entryId.libraryEntryCode

        /// One stored sample version's row in the Library window's version list.
        static member libraryVersionRow (version : VersionNumber) : ConstructorUiId = ConstructorUiId $"LibraryVersionRow_{version.value}"

    type ConstructorUiKey with

        /// The category a Category-manager row key stands for (the inverse of `CategoryId.uiKey`).
        member this.tryCategoryId : CategoryId option =
            match System.Guid.TryParse this.value with
            | true, g -> Some (CategoryId g)
            | _ -> None

        /// The id text of an entry leaf: the corpus `entry:<id>` or a branch-nested `…:entry:<id>`; a
        /// `facet:` / `branch:` / `entries` code carries none.
        member private this.entryLeafText : string option =
            let flatPrefix = "entry:"
            let nestedMarker = ":entry:"
            let code = this.value
            if code.StartsWith flatPrefix then Some (code.Substring flatPrefix.Length)
            else
                match code.LastIndexOf(nestedMarker, System.StringComparison.Ordinal) with
                | -1 -> None
                | idx -> Some (code.Substring (idx + nestedMarker.Length))

        /// A facet branch's member leaf, under this branch code: `<branch code>:entry:<guid>`.
        member this.materialsBranchLeaf (materialId : MaterialId) : ConstructorUiKey =
            ConstructorUiKey $"{this.value}:{materialId.materialsEntryCode.value}"

        /// The material a Materials-window entry leaf stands for; a `facet:` / `branch:` / `entries` code is `None`.
        member this.tryMaterialsEntryId : MaterialId option =
            match this.entryLeafText with
            | Some t ->
                match System.Guid.TryParse t with
                | true, g -> Some (MaterialId g)
                | _ -> None
            | None -> None

        /// A facet branch's member leaf in the Library window, under this branch code: `<branch code>:entry:<entryId>`.
        member this.libraryBranchLeaf (entryId : OpticalConstructor.Domain.Placement.ElementValueId) : ConstructorUiKey =
            ConstructorUiKey $"{this.value}:{entryId.libraryEntryCode.value}"

        /// The Library entry an entry leaf stands for; a `facet:` / `branch:` / `entries` code is `None`.
        member this.tryLibraryEntryId : OpticalConstructor.Domain.Placement.ElementValueId option =
            this.entryLeafText |> Option.map OpticalConstructor.Domain.Placement.ElementValueId

        /// A numeric bucket's code (`NumericRange.uiKey`) read back as its range.
        member this.tryNumericRange : NumericRange option =
            let tryEndpoint (text : string) : UnitMagnitude option =
                match System.Double.TryParse(text.Trim(), System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                | true, v -> Some (UnitMagnitude v)
                | _ -> None
            match this.value.Split ':' with
            | [| lo; hi |] ->
                match tryEndpoint lo, tryEndpoint hi with
                | Some lower, Some upper -> Some ({ lower = lower; upper = upper } : NumericRange)
                | _ -> None
            | _ -> None

    type ControlCaption with

        /// The text typed into a Category-manager row's name box, as the category's name.
        member this.toCategoryName : CategoryName = CategoryName this.value

        /// The text committed in a filter box, as the query it filters by.
        member this.toTextQuery : TextQuery = TextQuery this.value

        /// The manual min–max box's text as a range: `lo-hi` (both in nm), or one value as the exact-value
        /// range; anything else is `None`, the control's no-op (spec 0038 §D.0).
        member this.tryManualNumericRange : NumericRange option =
            let tryEndpoint (text : string) : UnitMagnitude option =
                match System.Double.TryParse(text.Trim(), System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                | true, v -> Some (UnitMagnitude v)
                | _ -> None
            let text = this.value.Trim()
            match text with
            | "" -> None
            | _ ->
                match text.Split '-' with
                | [| single |] -> tryEndpoint single |> Option.map (fun v -> ({ lower = v; upper = v } : NumericRange))
                | [| lo; hi |] ->
                    match tryEndpoint lo, tryEndpoint hi with
                    | Some lower, Some upper -> Some ({ lower = lower; upper = upper } : NumericRange)
                    | _ -> None
                | _ -> None

    type ElementId with

        /// The element as the key of its repeated controls (an Experiments-bay candidate).
        member this.uiKey : ConstructorUiKey = ConstructorUiKey this.value

    type CategoryId with

        /// The category as the key of its Category-manager row (its Guid's text).
        member this.uiKey : ConstructorUiKey = ConstructorUiKey (string this.value)

    type CategoryName with

        /// The name as its Category-manager row shows it.
        member this.caption : ControlCaption = ControlCaption this.value

    type AttributeKey with

        /// The facet as the key of its faceted-tree offer group and breadcrumb chip.
        member this.uiKey : ConstructorUiKey = ConstructorUiKey this.value

    type DiscreteKey with

        /// The value as the key of its faceted-tree offered value.
        member this.uiKey : ConstructorUiKey = ConstructorUiKey this.value

        /// The value as its offered box's label (at the engine's layer a key doubles as its label).
        member this.caption : ControlCaption = ControlCaption this.value

    type ModelParameterKey with

        /// The coefficient as the key of its Material-editor box.
        member this.uiKey : ConstructorUiKey = ConstructorUiKey this.value

    type CoefficientUnit with

        /// The unit as the label trailing its coefficient box shows it.
        member this.caption : ControlCaption = ControlCaption this.value

    type ItemCount with

        /// The count as the faceted tree shows it.
        member this.facetCount : FacetedTreeControls.FacetItemCount = FacetedTreeControls.FacetItemCount this.value

    type TextQuery with

        /// The query as the text its filter box shows.
        member this.caption : ControlCaption = ControlCaption this.value

    type FacetCaption with

        /// A facet's name or a branch's label as its faceted-tree caption.
        member this.caption : ControlCaption = ControlCaption this.value

    type OpticalConstructor.Domain.Propagation.SweptValue with

        /// One end of the draft's range as the Experiments bay's box shows it.
        member this.rangeEnd : ExperimentControls.ExperimentRangeEnd = ExperimentControls.ExperimentRangeEnd this.value

    type OpticalConstructor.Domain.Propagation.CurvePointCount with

        /// The draft's point count as the Experiments bay's box shows it.
        member this.pointCount : ExperimentControls.ExperimentPointCount = ExperimentControls.ExperimentPointCount this.value

    type ExperimentControls.ExperimentRangeEnd with

        /// The value typed into the Experiments bay's range box as the draft's range end.
        member this.sweptValue : OpticalConstructor.Domain.Propagation.SweptValue = OpticalConstructor.Domain.Propagation.SweptValue this.value

    type ExperimentControls.ExperimentPointCount with

        /// The count typed into the Experiments bay's points box as the draft's point count.
        member this.curvePointCount : OpticalConstructor.Domain.Propagation.CurvePointCount =
            OpticalConstructor.Domain.Propagation.CurvePointCount this.value

    type ExperimentId with

        /// The experiment as the key of its Experiments-bay collection row (its number's text).
        member this.uiKey : ConstructorUiKey = ConstructorUiKey (string this.value)

    type Kit.SetupName with

        /// The setup name as its saved-setup button's caption (spec 0050 S8.2).
        member this.caption : ControlCaption = ControlCaption this.value

    type Kit.NamingTemplate with

        /// The template's text as the kit bar's naming-template field shows it (spec 0050 S8.3).
        member this.caption : ControlCaption = ControlCaption this.value

    type EntryName with

        /// The name as a row, a summary or a name box shows it.
        member this.caption : ControlCaption = ControlCaption this.value

        /// The name as a retired entry's tree leaf shows it, badged.
        member this.inactiveCaption : ControlCaption = ControlCaption $"{this.value} — inactive"

    type SubstrateKind with

        /// The geometry as the key of its Sample-editor option box.
        member this.uiKey : ConstructorUiKey =
            match this with
            | ThinFilm -> ConstructorUiKey "ThinFilm"
            | ThickPlate -> ConstructorUiKey "Plate"
            | Mirror -> ConstructorUiKey "Mirror"

        /// The geometry as its Sample-editor option box shows it.
        member this.caption : ControlCaption =
            match this with
            | ThinFilm -> ControlCaption "Thin film"
            | ThickPlate -> ControlCaption "Plate"
            | Mirror -> ControlCaption "Mirror"

    type OpticalConstructor.Domain.SampleStackEditor.FilmItemIndex with

        /// The films item as the position of its repeated Sample-editor controls.
        member this.uiPosition : ConstructorUiPosition = ConstructorUiPosition this.value

    type OpticalConstructor.Domain.SampleStackEditor.CellLayerIndex with

        /// The cell layer as the position of its repeated Sample-editor controls.
        member this.uiPosition : ConstructorUiPosition = ConstructorUiPosition this.value

    type MaterialError with

        /// The typed rejection's reason as a status line shows it.
        member this.caption : ControlCaption =
            match this with
            | UnknownMaterialIdErr reason
            | DuplicateMaterialIdErr reason
            | MaterialStillReferencedErr reason
            | MaterialVersionPinnedErr reason
            | MaterialStoreUnavailableErr reason
            | InvalidMaterialErr reason -> ControlCaption reason

    type CategoryError with

        /// The typed rejection's reason as the Category editor's block slot shows it.
        member this.caption : ControlCaption =
            match this with
            | UnknownCategoryIdErr reason
            | DuplicateCategoryIdErr reason
            | CategoryStillReferencedErr reason
            | BuiltInNotRemovableErr reason
            | CategoryStoreUnavailableErr reason
            | InvalidCategoryErr reason -> ControlCaption reason

    type SourceError with

        /// The typed rejection's reason as a status line shows it.
        member this.caption : ControlCaption = ControlCaption this.reason

    type DetectorKind with

        /// The detector kind as a summary line names it.
        member this.caption : ControlCaption =
            match this with
            | DetectorKind.Intensity -> ControlCaption "Intensity"
            | DetectorKind.Ellipsometer -> ControlCaption "Ellipsometer"

    type OpticalConstructor.Domain.Kit.KitError with

        /// The kit failure as the measured-data attachment status (spec 0050 S4.2).
        member this.caption : ControlCaption = ControlCaption this.describe

    type OpticalConstructor.Domain.KitLibrary.KitLibraryError with

        /// The failed kit open as the start screen and the launcher show it (spec 0050 S5.2).
        member this.openFailedCaption : ControlCaption = ControlCaption $"The kit could not be opened: {this.describe}"

    type OpticalConstructor.Domain.Kit.KitLockHolder with

        /// The kit held by another session as the start screen and the launcher show it (spec 0050 S5.2).
        member this.openElsewhereCaption : ControlCaption = ControlCaption $"The kit is open elsewhere: {this.describe}."

    type OpticalConstructor.Domain.KitHistory.KitHistoryError with

        /// The failed kit-history write as the start screen shows it (spec 0050 S5.2).
        member this.notSavedCaption : ControlCaption = ControlCaption $"The kit history was not saved — {this.describe}"

    type OpticalConstructor.Domain.KitHistory.KitHistoryEntry with

        /// The kit's last open as its history row shows it, in the invariant culture as `KitLockHolder.describe` (spec 0050 S5.2).
        member this.lastOpenedCaption : ControlCaption =
            let instant = this.lastOpened.value.ToString("yyyy-MM-dd HH:mm", System.Globalization.CultureInfo.InvariantCulture)
            ControlCaption $"Last opened {instant} UTC"

    type OpticalConstructor.Domain.Kit.KitFolder with

        /// Open kit…'s offer for a folder without `.kit-json` (spec 0050 S5.2).
        member this.notAKitCaption : ControlCaption = ControlCaption $"{this.value.value} holds no kit."

    type OpticalConstructor.Domain.Kit.KitRelativePath with

        /// The kit-relative path as its data-file caption (spec 0050 S4.2).
        member this.toControlCaption : ControlCaption = ControlCaption this.value

    type OpticalConstructor.Domain.MeasuredData.ExperimentDataError with

        /// The typed rejection's reason as a status line shows it.
        member this.caption : ControlCaption =
            match this with
            | OpticalConstructor.Domain.MeasuredData.MalformedDataFileErr reason
            | OpticalConstructor.Domain.MeasuredData.EmptyDataFileErr reason
            | OpticalConstructor.Domain.MeasuredData.DataRangeMismatchErr reason
            | OpticalConstructor.Domain.MeasuredData.DataUnitsMismatchErr reason -> ControlCaption reason

    type UnitOfMeasure with

        /// The unit as the ASCII key of its choice box (the display symbols carry µ / ⁻¹).
        member this.uiKey : ConstructorUiKey =
            match this with
            | Nanometer -> ConstructorUiKey "nm"
            | Micrometer -> ConstructorUiKey "um"
            | Angstrom -> ConstructorUiKey "A"
            | ElectronVolt -> ConstructorUiKey "eV"
            | Wavenumber -> ConstructorUiKey "cm-1"
            | Meter -> ConstructorUiKey "m"
            | Millimeter -> ConstructorUiKey "mm"

        /// The unit as its choice box shows it.
        member this.caption : ControlCaption = ControlCaption this.abbreviation

    type SampleError with

        /// The typed rejection's reason as a status line shows it.
        member this.caption : ControlCaption =
            match this with
            | UnknownSampleIdErr reason
            | DuplicateSampleIdErr reason
            | SampleVersionPinnedErr reason
            | SampleStoreUnavailableErr reason
            | InvalidSampleErr reason -> ControlCaption reason

    type OpticalConstructor.Domain.SampleStackEditor.SampleStackEditError with

        /// The typed rejection's reason as a status line shows it.
        member this.caption : ControlCaption =
            match this with
            | OpticalConstructor.Domain.SampleStackEditor.InvalidRepeatCountErr reason
            | OpticalConstructor.Domain.SampleStackEditor.SelectionNotFoldableErr reason
            | OpticalConstructor.Domain.SampleStackEditor.NotARepeatGroupErr reason -> ControlCaption reason

    type MaterialComplexityEditError with

        /// The typed rejection's reason as a status line or the derived-model readout shows it.
        member this.caption : ControlCaption =
            match this with
            | NoSuchSegmentErr reason
            | LastSegmentNotRemovableErr reason
            | PublishedGyrationNotEditableErr reason -> ControlCaption reason

    type SegmentIndex with

        /// The dispersion segment as the position of its repeated Material-editor controls.
        member this.uiPosition : ConstructorUiPosition = ConstructorUiPosition this.value

    type Anisotropy with

        /// The choice as the key of its Material-editor option box.
        member this.uiKey : ConstructorUiKey =
            match this with
            | Isotropic -> ConstructorUiKey "Isotropic"
            | Uniaxial -> ConstructorUiKey "Uniaxial"
            | Biaxial -> ConstructorUiKey "Biaxial"

        /// The choice as its Material-editor option box shows it.
        member this.caption : ControlCaption =
            match this with
            | Isotropic -> ControlCaption "Isotropic"
            | Uniaxial -> ControlCaption "Uniaxial"
            | Biaxial -> ControlCaption "Biaxial"

    type Handedness with

        /// The enantiomorph as the key of its Material-editor option box.
        member this.uiKey : ConstructorUiKey =
            match this with
            | RightHanded -> ConstructorUiKey "Right"
            | LeftHanded -> ConstructorUiKey "Left"

        /// The enantiomorph as its Material-editor option box shows it.
        member this.caption : ControlCaption =
            match this with
            | RightHanded -> ControlCaption "Right"
            | LeftHanded -> ControlCaption "Left"

    type GyrationAxis with

        /// The magnetization axis as the key of its Material-editor option box.
        member this.uiKey : ConstructorUiKey =
            match this with
            | AlongX -> ConstructorUiKey "X"
            | AlongY -> ConstructorUiKey "Y"
            | AlongZ -> ConstructorUiKey "Z"

        /// The magnetization axis as its Material-editor option box shows it.
        member this.caption : ControlCaption =
            match this with
            | AlongX -> ControlCaption "X"
            | AlongY -> ControlCaption "Y"
            | AlongZ -> ControlCaption "Z"

    type MuKind with

        /// The Polder-μ kind as the key of its Material-editor option box.
        member this.uiKey : ConstructorUiKey =
            match this with
            | ScalarMuKind -> ConstructorUiKey "Scalar"
            | GyromagneticMuKind -> ConstructorUiKey "Gyromagnetic"

        /// The Polder-μ kind as its Material-editor option box shows it.
        member this.caption : ControlCaption =
            match this with
            | ScalarMuKind -> ControlCaption "Scalar"
            | GyromagneticMuKind -> ControlCaption "Gyromagnetic"

    type FolderName with

        /// The folder's text as its Data locations row shows it (spec 0050 S5.3).
        member this.pathCaption : ControlCaption = ControlCaption this.value

    type FileName with

        /// The library file's full path as its Data locations row shows it (spec 0050 S5.3).
        member this.pathCaption : ControlCaption = ControlCaption this.value

    type DataLocationKind with

        /// The location as its Data locations row names it (spec 0050 S5.3).
        member this.caption : ControlCaption =
            match this with
            | LibraryDatabaseLocation -> ControlCaption "Library database"
            | LibraryBackupsLocation -> ControlCaption "Library backups"
            | PreferencesLocation -> ControlCaption "Preferences"
            | KitsLocation -> ControlCaption "Kit folders (suggested parent)"
            | ExportsLocation -> ControlCaption "Exports"
            | CataloguesLocation -> ControlCaption "Catalogue exports"
            | ShippedCatalogueLocation -> ControlCaption "Shipped catalogue"

        /// The location as the key of its Data locations row's ids (spec 0050 S5.3).
        member this.uiKey : ConstructorUiKey =
            match this with
            | LibraryDatabaseLocation -> ConstructorUiKey "LibraryDatabase"
            | LibraryBackupsLocation -> ConstructorUiKey "LibraryBackups"
            | PreferencesLocation -> ConstructorUiKey "Preferences"
            | KitsLocation -> ConstructorUiKey "Kits"
            | ExportsLocation -> ConstructorUiKey "Exports"
            | CataloguesLocation -> ConstructorUiKey "Catalogues"
            | ShippedCatalogueLocation -> ConstructorUiKey "ShippedCatalogue"

    type ConstructorUiId with

        /// A location's row in the Data locations window (spec 0050 S5.3).
        static member dataLocationRow (kind : DataLocationKind) : ConstructorUiId = ConstructorUiId $"DataLocationRow_{kind.uiKey.value}"

        /// The path a location's row shows (spec 0050 S5.3).
        static member dataLocationPathText (kind : DataLocationKind) : ConstructorUiId = ConstructorUiId $"DataLocationPath_{kind.uiKey.value}"

        /// A location's Open folder, present only where the row has a folder (spec 0050 S5.3).
        static member dataLocationOpenFolderButton (kind : DataLocationKind) : ConstructorUiId =
            ConstructorUiId $"DataLocationOpenFolder_{kind.uiKey.value}"

    type ConstructorUiId with

        /// A kit item's row in the Items panel's list: `KitItemRow_<the item's GUID>` (spec 0050 S7.2).
        static member kitItemRow (itemId : Kit.ItemId) : ConstructorUiId = ConstructorUiId $"KitItemRow_{itemId.value}"

        /// An add-form offer: `AddItemOffer_<model GUID>_v<n>`, then its preset's id or sample's `<GUID>_v<n>` (spec 0050 S7.2).
        static member addItemOffer (choice : Kit.ItemModelChoice) : ConstructorUiId =
            let modelPart (model : ItemModels.ItemModelVersionId) : string = $"{model.itemModelId.value}_v{model.version.value}"
            match choice.instance with
            | Kit.ItemModelVersionInstance model -> ConstructorUiId $"AddItemOffer_{modelPart model}"
            | Kit.PresetInstance (model, preset) -> ConstructorUiId $"AddItemOffer_{modelPart model}_{preset.value}"
            | Kit.SampleInstance (model, sample) ->
                ConstructorUiId $"AddItemOffer_{modelPart model}_{sample.sampleId.value}_v{sample.version.value}"

        /// The add form's choice of a mounting: `NewItemDial_Fixed`, `_Positive` or `_Negative` (spec 0050 S7.2).
        static member newItemDialChoice (dial : Kit.ItemDial) : ConstructorUiId =
            match dial with
            | Kit.FixedMounting -> ConstructorUiId "NewItemDial_Fixed"
            | Kit.DialMounted ItemModels.DialPositive -> ConstructorUiId "NewItemDial_Positive"
            | Kit.DialMounted ItemModels.DialNegative -> ConstructorUiId "NewItemDial_Negative"

        /// A parameter's row of the selected item's grid: `ItemParameterRow_<the name>` (spec 0050 S7.2).
        static member itemParameterRow (name : ItemModels.ParameterName) : ConstructorUiId = ConstructorUiId $"ItemParameterRow_{name.value}"

        /// A revision's line of the selected item's revisions: `ItemRevisionRow_<n>` (spec 0050 S7.2).
        static member itemRevisionRow (revision : Kit.ItemRevision) : ConstructorUiId = ConstructorUiId $"ItemRevisionRow_{revision.value}"

    type Kit.ItemModelChoice with

        /// The offer as the add form shows it: the model's name, version and GUID, after its binding's (spec 0050 S7.2).
        member this.caption : ControlCaption =
            let modelCaption (model : ItemModels.ItemModelVersion) : ControlCaption =
                ControlCaption $"{model.entry.name.value} v{model.version.value} ({model.entry.id.value})"
            match this with
            | Kit.LibraryModelChoice model -> modelCaption model
            | Kit.SourcePresetChoice (model, source) ->
                ControlCaption $"{source.name.value} ({source.id.value}) — {(modelCaption model).value}"
            | Kit.PolarizerPresetChoice (model, polarizer) ->
                ControlCaption $"{polarizer.name.value} ({polarizer.id.value}) — {(modelCaption model).value}"
            | Kit.StackSampleChoice (model, version, sample) ->
                ControlCaption $"{sample.name.value} v{version.version.value} ({version.sampleId.value}) — {(modelCaption model).value}"

    type Kit.ItemDial with

        /// The mounting as the Items panel names it (spec 0050 S7.2).
        member this.caption : ControlCaption =
            match this with
            | Kit.FixedMounting -> ControlCaption "Fixed mounting"
            | Kit.DialMounted ItemModels.DialPositive -> ControlCaption "On a dial, sign +1"
            | Kit.DialMounted ItemModels.DialNegative -> ControlCaption "On a dial, sign −1"

    type ItemModels.ParameterName with

        /// The parameter's name as its grid row shows it (spec 0050 S7.2).
        member this.caption : ControlCaption = ControlCaption this.value

    /// A parameter's cells after its name in the Items panel's grid (spec 0050 S7.2; 001 §O).
    type ParameterGridCells =
        {
            state : ControlCaption
            value : ControlCaption
            bounds : ControlCaption
            sigma : ControlCaption
            provenance : ControlCaption
        }

    type Kit.Parameter with

        /// The grid's state, value, bounds, σ and provenance, each number G6 in its own unit (spec 0050 S7.2, step 207).
        member this.gridCells : ParameterGridCells =
            let number (value : ItemModels.ParameterValue) : ControlCaption =
                ControlCaption (value.value.ToString("G6", System.Globalization.CultureInfo.InvariantCulture))
            let sigmaText (sigma : Kit.ParameterSigma) : ControlCaption =
                ControlCaption (sigma.value.ToString("G6", System.Globalization.CultureInfo.InvariantCulture))
            let absent = ControlCaption "—"
            match this.state with
            | Kit.KnownParameter known ->
                {
                    state = ControlCaption "Known"
                    value = number known
                    bounds = absent
                    sigma = absent
                    provenance = ControlCaption "Given"
                }
            | Kit.UnknownParameter unknown ->
                {
                    state = ControlCaption "Unknown"
                    value = number unknown.start
                    bounds = ControlCaption $"[{(number unknown.bounds.lower).value}, {(number unknown.bounds.upper).value}]"
                    sigma =
                        match unknown.priorOpt with
                        | Some prior -> sigmaText prior.sigma
                        | None -> absent
                    provenance = ControlCaption "To be fitted"
                }
            | Kit.CalibratedParameter calibrated ->
                {
                    state = ControlCaption "Calibrated"
                    value = number calibrated.value
                    bounds = absent
                    sigma = sigmaText calibrated.sigma
                    provenance = ControlCaption $"Fit run {calibrated.fitRun.value}"
                }

    type Kit.ItemRevisionRecord with

        /// An earlier revision as the Items panel lists it: `Revision <n>: <name> <value>, …` (spec 0050 S7.2).
        member this.caption : ControlCaption =
            match this.parameters with
            | [] -> ControlCaption $"Revision {this.revision.value}: no parameters"
            | parameters ->
                let values =
                    parameters
                    |> List.map (fun parameter -> $"{parameter.name.value} {parameter.gridCells.value.value}")
                    |> String.concat ", "
                ControlCaption $"Revision {this.revision.value}: {values}"

    type Kit.ItemLabel with

        /// The label as a slot-bar chip or a chip's list names the item (spec 0050 S8.4).
        member this.caption : ControlCaption = ControlCaption this.value

    type Berreman.Geometry.Angle with

        /// The angle in degrees as the rotation bar writes them (`BarDegrees.text`), then `°` (spec 0050 S8.4).
        member this.degreeCaption : ControlCaption = ControlCaption $"{(BarDegrees this.degrees).text}°"

    type Setups.RoledSlot with

        /// The chip's `<role>: <item label>`, `—` for nothing and `(not in this kit)` for an item `items` lacks; an
        /// object holding an item adds ` @ ` and its `turnAngle`, the angle a name writes (spec 0050 S8.3, S8.4).
        member this.chipCaption (items : Kit.Item list) : ControlCaption =
            match this.slot.content.itemOpt with
            | None -> ControlCaption $"{this.role.text}: —"
            | Some held ->
                let label =
                    match items |> List.tryFind (fun item -> item.id = held) with
                    | Some item -> item.label.value
                    | None -> "(not in this kit)"
                match this.role with
                | Setups.ObjectSlotRole _ -> ControlCaption $"{this.role.text}: {label} @ {this.slot.turnAngle.degreeCaption.value}"
                | Setups.SourceSlotRole
                | Setups.PrepSlotRole _
                | Setups.AnalyzerSlotRole _
                | Setups.DetectorSlotRole _ -> ControlCaption $"{this.role.text}: {label}"

namespace OpticalConstructor.Ui

open Berreman.Dispersion
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

    type ConstructorUiId with

        /// An element's invisible hover target on the Main table, which carries its tooltip (spec 0046).
        static member elementHover (elementId : ElementId) : ConstructorUiId = ConstructorUiId $"ElementHover_{elementId.value}"

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

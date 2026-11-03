namespace OpticalConstructor.Ui

open Berreman.Dispersion
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.Units

/// The Controls values derived from a Domain value: the automation ids (spec 0048 R1) and a control's
/// keys, counts and captions. Controls references no Domain type, so they are extension members here,
/// the lowest project that sees both.
[<AutoOpen>]
module ConstructorUiIdOfDomain =

    type MaterialId with

        /// The material as its Materials-window corpus leaf's tree-node code (`entry:<guid>`).
        member this.materialsEntryCode : ConstructorUiKey = ConstructorUiKey $"entry:{this.value}"

    type ConstructorUiId with

        /// An element's invisible hover target on the Main table, which carries its tooltip (spec 0046).
        static member elementHover (elementId : ElementId) : ConstructorUiId = ConstructorUiId $"ElementHover_{elementId.value}"

        /// The element menu's "Place on the reflected ray of" this element (spec 0046).
        static member elementMenuPlaceOnReflectedOf (elementId : ElementId) : ConstructorUiId =
            ConstructorUiId $"ElementMenuPlaceOnReflectedOf_{elementId.value}"

        /// A collected experiment's summary block and its typed validation-status line (`SolverHandoffView`).
        static member solverHandoffRow (experimentId : ExperimentId) : ConstructorUiId = ConstructorUiId $"SolverHandoffRow_{experimentId.value}"

        static member solverHandoffStatus (experimentId : ExperimentId) : ConstructorUiId = ConstructorUiId $"SolverHandoffStatus_{experimentId.value}"

        /// A material-category option of the Material editor.
        static member materialEditorCategoryOption (categoryId : CategoryId) : ConstructorUiId =
            ConstructorUiId $"MaterialCategoryOption_{categoryId.value}"

        /// A material's corpus leaf in the Materials window's faceted tree.
        static member materialsEntryNode (materialId : MaterialId) : ConstructorUiId = ConstructorUiId.facetTreeNode materialId.materialsEntryCode

        /// One stored version's row in the Materials window's version list.
        static member materialsVersionRow (version : VersionNumber) : ConstructorUiId = ConstructorUiId $"MaterialsVersionRow_{version.value}"

    type ConstructorUiKey with

        /// The category a Category-manager row key stands for (the inverse of `CategoryId.uiKey`).
        member this.tryCategoryId : CategoryId option =
            match System.Guid.TryParse this.value with
            | true, g -> Some (CategoryId g)
            | _ -> None

        /// A facet branch's member leaf, under this branch code: `<branch code>:entry:<guid>`.
        member this.materialsBranchLeaf (materialId : MaterialId) : ConstructorUiKey =
            ConstructorUiKey $"{this.value}:{materialId.materialsEntryCode.value}"

        /// The material a Materials-window entry leaf stands for: the corpus `entry:<guid>` or a
        /// branch-nested `…:entry:<guid>`; a `facet:` / `branch:` / `entries` code is `None`.
        member this.tryMaterialsEntryId : MaterialId option =
            let flatPrefix = "entry:"
            let nestedMarker = ":entry:"
            let code = this.value
            let idText =
                if code.StartsWith flatPrefix then Some (code.Substring flatPrefix.Length)
                else
                    match code.LastIndexOf(nestedMarker, System.StringComparison.Ordinal) with
                    | -1 -> None
                    | idx -> Some (code.Substring (idx + nestedMarker.Length))
            match idText with
            | Some t ->
                match System.Guid.TryParse t with
                | true, g -> Some (MaterialId g)
                | _ -> None
            | None -> None

    type ControlCaption with

        /// The text typed into a Category-manager row's name box, as the category's name.
        member this.toCategoryName : CategoryName = CategoryName this.value

        /// The text committed in a filter box, as the query it filters by.
        member this.toTextQuery : TextQuery = TextQuery this.value

    type ElementId with

        /// The element as the key of its repeated controls (an Experiments-bay candidate).
        member this.uiKey : ConstructorUiKey = ConstructorUiKey this.value

    type OpticalConstructor.Domain.Placement.ElementValueId with

        /// The Library entry as the key of its Select-bay leaf row.
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
            | MaterialVersionInUseErr reason
            | InvalidMaterialErr reason -> ControlCaption reason

    type CategoryError with

        /// The typed rejection's reason as the Category editor's block slot shows it.
        member this.caption : ControlCaption =
            match this with
            | UnknownCategoryIdErr reason
            | DuplicateCategoryIdErr reason
            | CategoryStillReferencedErr reason
            | BuiltInNotRemovableErr reason
            | InvalidCategoryErr reason -> ControlCaption reason

    type SourceError with

        /// The typed rejection's reason as a status line shows it.
        member this.caption : ControlCaption = ControlCaption this.reason

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
            | SampleVersionInUseErr reason
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
            | LastSegmentNotRemovableErr reason -> ControlCaption reason

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

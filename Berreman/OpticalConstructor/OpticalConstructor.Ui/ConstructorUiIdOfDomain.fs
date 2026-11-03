namespace OpticalConstructor.Ui

open OpticalConstructor.Controls
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.DispersionModels

/// The Controls values derived from a Domain value: the automation ids (spec 0048 R1) and a control's
/// keys, counts and captions. Controls references no Domain type, so they are extension members here,
/// the lowest project that sees both.
[<AutoOpen>]
module ConstructorUiIdOfDomain =

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

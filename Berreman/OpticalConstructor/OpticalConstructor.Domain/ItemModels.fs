namespace OpticalConstructor.Domain

open System
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

/// The parameter vocabulary of items and item models, shared with the fitter, the parameters the existing
/// models publish, the parameterised item models as data and the item-model store's seam (spec 0050 S7.1).
module ItemModels =

    /// A number a published parameter name carries: a Mueller row or column, 0 to 3, or a 1-based film, cell-layer or
    /// compound-component position (spec 0050 S8.1; elevated because ELEV010 refuses an `int` beside `ParameterName`).
    type ParameterNameIndex =
        | ParameterNameIndex of int

        member this.value : int = let (ParameterNameIndex i) = this in i

    /// The name a fitted parameter is reported under — elevated so that a report cannot silently pair a
    /// name with the wrong column, and so that a name can never be passed where a reason or a description
    /// is expected. Spec 0050 S7.1 moved it here from `MuellerInverse`; spec 0050 S8.1 writes every name an item model
    /// publishes once, as a member below.
    type ParameterName =
        | ParameterName of string

        /// The name as text (the reporting seam).
        member this.value = let (ParameterName n) = this in n

        /// The light source's polarization azimuth (spec 0050 S8.1).
        static member azimuth : ParameterName = ParameterName "azimuth"

        /// The light source's ellipticity angle (spec 0050 S8.1).
        static member ellipticity : ParameterName = ParameterName "ellipticity"

        /// The light source's degree of polarization (spec 0050 S8.1).
        static member degreeOfPolarization : ParameterName = ParameterName "degreeOfPolarization"

        /// The light source's intensity scale (spec 0050 S8.1).
        static member intensityScale : ParameterName = ParameterName "intensityScale"

        /// The real linear polarizer's extinction ratio (spec 0050 S8.1).
        static member extinctionRatio : ParameterName = ParameterName "extinctionRatio"

        /// The real linear polarizer's diattenuation (spec 0050 S8.1).
        static member diattenuation : ParameterName = ParameterName "diattenuation"

        /// The real linear polarizer's axis offset (spec 0050 S8.1).
        static member axisOffset : ParameterName = ParameterName "axisOffset"

        /// The retarder plate's retardance (spec 0050 S8.1).
        static member retardance : ParameterName = ParameterName "retardance"

        /// The retarder plate's fast-axis offset (spec 0050 S8.1).
        static member fastAxisOffset : ParameterName = ParameterName "fastAxisOffset"

        /// The black box's scale, published in m00's place (spec 0050 S8.1).
        static member blackBoxScale : ParameterName = ParameterName "scale"

        /// The intensity detector's gain (spec 0050 S8.1).
        static member gain : ParameterName = ParameterName "gain"

        /// The intensity detector's dark offset (spec 0050 S8.1).
        static member darkOffset : ParameterName = ParameterName "darkOffset"

        /// The noise budget's rotation uncertainty (spec 0050 S8.1).
        static member rotationUncertainty : ParameterName = ParameterName "rotationUncertainty"

        /// The noise budget's element uncertainty (spec 0050 S8.1).
        static member elementUncertainty : ParameterName = ParameterName "elementUncertainty"

        /// The noise budget's throughput uncertainty (spec 0050 S8.1).
        static member throughputUncertainty : ParameterName = ParameterName "throughputUncertainty"

        /// A dial-mounted item's dial zero (spec 0050 S8.1).
        static member dialZero : ParameterName = ParameterName "dialZero"

        /// A sample substrate's thickness (spec 0050 S8.1).
        static member substrateThickness : ParameterName = ParameterName "substrate.thickness"

        /// The black box's Mueller element `m<row><column>`, rows and columns 0 to 3 (spec 0050 S8.1).
        static member muellerElement (row : ParameterNameIndex) (column : ParameterNameIndex) : ParameterName =
            let (ParameterNameIndex i) = row
            let (ParameterNameIndex j) = column
            ParameterName $"m{i}{j}"

        /// A single film's thickness `film<k>.thickness`, k 1-based in stack order (spec 0050 S8.1).
        static member filmThickness (position : ParameterNameIndex) : ParameterName =
            let (ParameterNameIndex k) = position
            ParameterName $"film{k}.thickness"

        /// One layer of a repeated film, shared by every period: `film<k>.layer<j>.thickness`, both 1-based
        /// (spec 0050 S8.1).
        static member repeatedLayerThickness (position : ParameterNameIndex) (layer : ParameterNameIndex) : ParameterName =
            let (ParameterNameIndex k) = position
            let (ParameterNameIndex j) = layer
            ParameterName $"film{k}.layer{j}.thickness"

        /// The name inside a compound's component k (1-based, light order): `c<k>.<name>`, so a nested compound
        /// prefixes twice (spec 0050 S8.1).
        member this.inComponent (position : ParameterNameIndex) : ParameterName =
            let (ParameterName name) = this
            let (ParameterNameIndex k) = position
            ParameterName $"c{k}.{name}"

    /// A named two-case sign for the analyzer dial → physical-angle map — never a naked `int`. `.value` is the
    /// multiplicative sign (`+1` / `−1`) the reference `AnalyzerModel.dial_sign` carries (matrix_glue.py:152).
    /// Spec 0050 S7.1 moved it here from `MuellerReconstruction`.
    type DialSign =
        | DialPositive
        | DialNegative

        /// The multiplicative sign (+1 / −1) — read at the arithmetic seam.
        member this.value : float =
            match this with
            | DialPositive -> 1.0
            | DialNegative -> -1.0

    /// One parameter's number in its kind's unit (spec 0050 001 §I).
    type ParameterValue =
        | ParameterValue of double

        member this.value : double = let (ParameterValue v) = this in v

    /// What a parameter measures, which fixes its unit (spec 0050 001 §I). Detector model files store their axis
    /// descriptors, so the wire tag is an int, 1-based in declaration order; `.text` is the separate rendering.
    type ParameterKind =
        /// Radians: an azimuth, an axis or dial offset, an ellipticity angle, a rotation uncertainty.
        | AngleParameter
        /// Radians: a retardance.
        | PhaseParameter
        /// Dimensionless in [0, 1]: an extinction ratio, a diattenuation, a degree of polarization, an element or
        /// throughput uncertainty.
        | FractionParameter
        /// A dimensionless multiplier: an intensity scale, a gain.
        | ScaleParameter
        /// One Mueller-matrix element.
        | MuellerElementParameter
        /// Metres.
        | ThicknessParameter
        /// A detector reading in its own units: a dark offset.
        | ReadingParameter

        member this.value : int =
            match this with
            | AngleParameter -> 1
            | PhaseParameter -> 2
            | FractionParameter -> 3
            | ScaleParameter -> 4
            | MuellerElementParameter -> 5
            | ThicknessParameter -> 6
            | ReadingParameter -> 7

        static member tryOfWire (tag : int) : ParameterKind option =
            match tag with
            | 1 -> Some AngleParameter
            | 2 -> Some PhaseParameter
            | 3 -> Some FractionParameter
            | 4 -> Some ScaleParameter
            | 5 -> Some MuellerElementParameter
            | 6 -> Some ThicknessParameter
            | 7 -> Some ReadingParameter
            | _ -> None

        member this.text : string =
            match this with
            | AngleParameter -> "angle"
            | PhaseParameter -> "phase"
            | FractionParameter -> "fraction"
            | ScaleParameter -> "scale"
            | MuellerElementParameter -> "Mueller element"
            | ThicknessParameter -> "thickness"
            | ReadingParameter -> "reading"

    /// One parameter a model publishes; the fitter's axes are generated from these (spec 0050 001 §I). A detector
    /// model carries its noise axes; an instantiated item carries the corresponding ordinary parameter states.
    type ItemParameterAxis =
        {
            name : ParameterName
            kind : ParameterKind
            defaultValue : ParameterValue
        }

        /// A detector's noise model as axes, in this order: the rotation uncertainty (an angle), then the element
        /// and the throughput uncertainties (fractions), each defaulting to the supplied value (spec 0050 001 §I, §Q).
        static member detectorNoiseAxes
            (rotation : ParameterValue)
            (element : ParameterValue)
            (throughput : ParameterValue)
            : ItemParameterAxis list =
            [
                { name = ParameterName.rotationUncertainty; kind = AngleParameter; defaultValue = rotation }
                { name = ParameterName.elementUncertainty; kind = FractionParameter; defaultValue = element }
                { name = ParameterName.throughputUncertainty; kind = FractionParameter; defaultValue = throughput }
            ]

    type Thickness with

        /// A finite thickness in metres as a parameter value; `Infinity` has none (spec 0050 S7.1).
        member this.parameterValueOpt : ParameterValue option =
            match this with
            | Thickness t -> Some (ParameterValue (float t))
            | Infinity -> None

    type SourcePreset with

        /// The light source's four axes in this order: azimuth and ellipticity (angles), degree of polarization, intensity
        /// scale, defaulting to unpolarized light at unit scale; a run with no bound source reads these (spec 0050 S8.1).
        static member lightSourceAxes : ItemParameterAxis list =
            [
                { name = ParameterName.azimuth; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName.ellipticity; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName.degreeOfPolarization; kind = FractionParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName.intensityScale; kind = ScaleParameter; defaultValue = ParameterValue 1.0 }
            ]

        /// The output polarization and intensity scale over the preset's own λ and tunable limits: the light-source axes
        /// (spec 0050 S7.1, S8.1; 001 §I, §N).
        member _.parameterAxes : ItemParameterAxis list = SourcePreset.lightSourceAxes

    type PolarizerPreset with

        /// None: an ideal LP or CP, and a constant-Mueller compound, has no parameter (spec 0050 S7.1; 001 §I).
        member _.parameterAxes : ItemParameterAxis list = []

    type Sample with

        /// One thickness per finite layer in stack order, in metres: `film<k>.thickness`, `film<k>.layer<j>.thickness`
        /// (shared by every period), `substrate.thickness`. Materials publish none: a study owns them (spec 0050 S7.1).
        member this.parameterAxes : ItemParameterAxis list =
            let filmLayers : (ParameterName * SampleLayer) list =
                this.structure.films
                |> List.mapi (fun i item ->
                    let position = ParameterNameIndex (i + 1)
                    match item with
                    | SingleLayer layer -> [ ParameterName.filmThickness position, layer ]
                    | Repeated group ->
                        group.cell
                        |> List.mapi (fun j layer ->
                            let cellLayer = ParameterNameIndex (j + 1)
                            ParameterName.repeatedLayerThickness position cellLayer, layer))
                |> List.concat
            let substrateLayers : (ParameterName * SampleLayer) list =
                this.structure.substrate |> Option.toList |> List.map (fun layer -> ParameterName.substrateThickness, layer)
            filmLayers @ substrateLayers
            |> List.choose (fun (name, layer) ->
                layer.thickness.parameterValueOpt
                |> Option.map (fun value -> { name = name; kind = ThicknessParameter; defaultValue = value }))

    /// How a real linear polarizer states its leakage (spec 0050 S7.1; 001 §I): by the extinction ratio
    /// T_min / T_max, or by the diattenuation (T_max − T_min) / (T_max + T_min).
    type PolarizerLeakage =
        | ExtinctionRatioLeakage
        | DiattenuationLeakage

    /// A retarder's dispersion, "dispersive where the model says so" (spec 0050 S7.1; 001 §I): δ is the same at every
    /// λ, or δ is the retardance at the reference λ and scales as λ_ref / λ, a zero-order plate's dispersion.
    type RetardanceDispersion =
        | ConstantRetardance
        | InverseWaveLengthRetardance of WaveLength

    /// A black-box Mueller matrix's parameters (spec 0050 S7.1; 001 §I): all sixteen elements, or a scale and the
    /// fifteen elements normalised by m00.
    type BlackBoxNormalisation =
        | SixteenElementMueller
        | FifteenElementsAndScale

    /// What one optical element does to light (spec 0050 S7.1; 001 §I). A compound generalises `ConstantMueller`
    /// with parameters; a black box is held at reference orientation and turned as R(−φ) M R(φ) like any element.
    type OpticalForm =
        | RealPolarizerModel of PolarizerLeakage
        | RetarderPlateModel of RetardanceDispersion
        | BlackBoxMuellerModel of BlackBoxNormalisation
        | CompoundModel of CompoundComponent list

        /// Its parameters at the ideal element's values: a compound's component k (1-based, light order) publishes
        /// its form's axes as `c<k>.<name>`, kind and default kept (spec 0050 S7.1; 001 §I).
        member this.parameterAxes : ItemParameterAxis list =
            match this with
            | RealPolarizerModel leakage ->
                let leakageAxis : ItemParameterAxis =
                    match leakage with
                    | ExtinctionRatioLeakage ->
                        { name = ParameterName.extinctionRatio; kind = FractionParameter; defaultValue = ParameterValue 0.0 }
                    | DiattenuationLeakage ->
                        { name = ParameterName.diattenuation; kind = FractionParameter; defaultValue = ParameterValue 1.0 }
                [
                    leakageAxis
                    { name = ParameterName.axisOffset; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
                ]
            | RetarderPlateModel _ ->
                [
                    { name = ParameterName.retardance; kind = PhaseParameter; defaultValue = ParameterValue (Math.PI / 2.0) }
                    { name = ParameterName.fastAxisOffset; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
                ]
            | BlackBoxMuellerModel normalisation ->
                let indices : ParameterNameIndex list = [ 0 .. 3 ] |> List.map ParameterNameIndex
                let elements : ItemParameterAxis list =
                    [
                        for row in indices do
                            for column in indices do
                                {
                                    name = ParameterName.muellerElement row column
                                    kind = MuellerElementParameter
                                    defaultValue = if row = column then ParameterValue 1.0 else ParameterValue 0.0
                                }
                    ]
                match normalisation with
                | SixteenElementMueller -> elements
                | FifteenElementsAndScale ->
                    // Row-major, so m00 leads `elements`: the scale takes its place.
                    { name = ParameterName.blackBoxScale; kind = ScaleParameter; defaultValue = ParameterValue 1.0 } :: List.tail elements
            | CompoundModel components ->
                components
                |> List.mapi (fun i part ->
                    let position = ParameterNameIndex (i + 1)
                    part.form.parameterAxes |> List.map (fun axis -> { axis with name = axis.name.inComponent position }))
                |> List.concat

    /// One component of a compound: its form, turned by its fixed offset within the compound (spec 0050 S7.1).
    and CompoundComponent =
        {
            form : OpticalForm
            offset : Angle
        }

    /// The library physics a binding item-model form takes its parameters from (spec 0050 S7.1; .spec-md §3.14).
    type ItemModelBindingKind =
        | SourcePresetBinding
        | PolarizerPresetBinding
        | StackSampleBinding

    /// The preset or sample a binding form is resolved against; its axes are that entry's own (spec 0050 S7.1).
    type ItemModelBinding =
        | SourcePresetBound of SourcePreset
        | PolarizerPresetBound of PolarizerPreset
        | StackSampleBound of Sample

        member this.bindingKind : ItemModelBindingKind =
            match this with
            | SourcePresetBound _ -> SourcePresetBinding
            | PolarizerPresetBound _ -> PolarizerPresetBinding
            | StackSampleBound _ -> StackSampleBinding

        /// The bound entry's axes, by the preset's or the sample's own `parameterAxes` (spec 0050 S7.1).
        member this.parameterAxes : ItemParameterAxis list =
            match this with
            | SourcePresetBound source -> source.parameterAxes
            | PolarizerPresetBound polarizer -> polarizer.parameterAxes
            | StackSampleBound sample -> sample.parameterAxes

    /// The payload of an item-model library entry (spec 0050 S7.1; 001 §I); a detector's budget is its noise axes'
    /// defaults. Evaluating a form to a Mueller matrix or a reading is the run law's (001 §N, S8.1).
    type ItemModelForm =
        | SourcePresetItemModel
        | IdealPolarizerItemModel
        | StackSampleItemModel
        | OpticalElementModel of OpticalForm
        /// The identity, the calibration reference.
        | EmptySlotModel
        /// Records S₀ through a gain and a dark offset.
        | IntensityDetectorModel of ItemParameterAxis list
        /// Records a full Mueller matrix.
        | MuellerPolarimeterModel of ItemParameterAxis list
        /// Records Ψ and Δ.
        | EllipsometerModel of ItemParameterAxis list

        /// The binding a source, ideal-polarizer or stack form requires; `None` for a standalone form (spec 0050 S7.1).
        member this.bindingKindOpt : ItemModelBindingKind option =
            match this with
            | SourcePresetItemModel -> Some SourcePresetBinding
            | IdealPolarizerItemModel -> Some PolarizerPresetBinding
            | StackSampleItemModel -> Some StackSampleBinding
            | OpticalElementModel _
            | EmptySlotModel
            | IntensityDetectorModel _
            | MuellerPolarimeterModel _
            | EllipsometerModel _ -> None

        /// The form's own axes: none for a binding form, whose axes its binding supplies; a detector's gain and dark
        /// offset, then its noise axes unchanged (spec 0050 S7.1; 001 §I, §L).
        member this.parameterAxes : ItemParameterAxis list =
            match this with
            | SourcePresetItemModel
            | IdealPolarizerItemModel
            | StackSampleItemModel
            | EmptySlotModel -> []
            | OpticalElementModel form -> form.parameterAxes
            | IntensityDetectorModel noise ->
                { name = ParameterName.gain; kind = ScaleParameter; defaultValue = ParameterValue 1.0 }
                :: { name = ParameterName.darkOffset; kind = ReadingParameter; defaultValue = ParameterValue 0.0 }
                :: noise
            | MuellerPolarimeterModel noise
            | EllipsometerModel noise -> noise

        /// The axes over a binding of the form's own kind, or `None` when the form takes no such binding
        /// (spec 0050 S7.1).
        member this.tryBoundParameterAxes (binding : ItemModelBinding) : ItemParameterAxis list option =
            match this.bindingKindOpt with
            | Some kind when kind = binding.bindingKind -> Some (this.parameterAxes @ binding.parameterAxes)
            | Some _
            | None -> None

    /// An item model's frozen identity, the GUID its library entry carries (spec 0050 S7.1; 001 §I).
    type ItemModelId =
        | ItemModelId of Guid

        member this.value : Guid = let (ItemModelId g) = this in g

    /// One version of one item model: what an item pins and snapshots (spec 0050 S7.1; 001 §I).
    type ItemModelVersionId =
        {
            itemModelId : ItemModelId
            version : VersionNumber
        }

    /// An item-model library entry, shaped like `PolarizerPreset`; its protection derives from its provenance at the
    /// file seam (spec 0050 S7.1).
    type ItemModelEntry =
        {
            id : ItemModelId
            name : EntryName
            description : EntryDescription
            model : ItemModelForm
            protection : EntryProtection
        }

    /// One stored version of an item model (spec 0050 S7.1).
    type ItemModelVersion =
        {
            version : VersionNumber
            entry : ItemModelEntry
        }

        member this.versionId : ItemModelVersionId = { itemModelId = this.entry.id; version = this.version }

    /// The frozen identities of the built-in item models the Built-in catalogue ships (spec 0050 S7.1; .spec-md §3.3).
    module ItemModelIds =
        let realLinearPolarizerModelId : ItemModelId = Guid.Parse "940bda4e-db9c-4103-b59c-dab2f3ee5875" |> ItemModelId
        let retarderPlateModelId : ItemModelId = Guid.Parse "8e9797b7-f0db-443f-b33d-61f9901881cd" |> ItemModelId
        let circularPolarizerModelId : ItemModelId = Guid.Parse "33486eec-836a-4cbf-ac79-e3f14439ad4b" |> ItemModelId
        let blackBoxMuellerModelId : ItemModelId = Guid.Parse "852cbb95-e9f8-4963-8098-866303071e67" |> ItemModelId
        let emptySlotModelId : ItemModelId = Guid.Parse "628b87f6-8e8c-460b-8a91-1815063aa9dd" |> ItemModelId
        let intensityDetectorModelId : ItemModelId = Guid.Parse "01cfdc64-d07e-4e2b-963e-ea3caa8428e6" |> ItemModelId
        let muellerPolarimeterModelId : ItemModelId = Guid.Parse "7e74324f-ded2-4607-b9d8-a40d477ac437" |> ItemModelId
        let ellipsometerModelId : ItemModelId = Guid.Parse "608caa06-1b53-464e-962f-073730270af6" |> ItemModelId
        let sourcePresetItemModelId : ItemModelId = Guid.Parse "239b0cfb-9938-5f3b-a767-c4252ea671b6" |> ItemModelId
        let idealPolarizerItemModelId : ItemModelId = Guid.Parse "d47e5ce3-3e74-5058-848e-3aa54930e377" |> ItemModelId
        let stackSampleItemModelId : ItemModelId = Guid.Parse "4d13ecb1-2006-576f-99e7-e8bc266e8624" |> ItemModelId

    /// Why an item-model store refused or failed (spec 0050 S7.1): the id is already held, the store cannot hold the
    /// entry (a protection or an id it refuses), or the store beneath failed, carrying that failure's `describe`.
    type ItemModelError =
        | DuplicateItemModelErr of reason : string
        | InvalidItemModelErr of reason : string
        | ItemModelStoreUnavailableErr of reason : string

        member this.describe : string =
            match this with
            | DuplicateItemModelErr reason -> $"the item model is already held: {reason}"
            | InvalidItemModelErr reason -> $"the item model cannot be stored: {reason}"
            | ItemModelStoreUnavailableErr reason -> $"the item-model store is unavailable: {reason}"

    /// The item-model store (spec 0050 S7.1): list answers each item model's latest version by name, then id; resolve
    /// answers that exact version or `None`; save stores a new item model and refuses a held id with
    /// `DuplicateItemModelErr`, changing nothing, because a later version arrives only through import (step 95).
    [<ReferenceEquality>]
    type ItemModelProxy =
        {
            listItemModels : unit -> Result<ItemModelVersion list, ItemModelError>
            resolveItemModelVersion : ItemModelVersionId -> Result<ItemModelVersion option, ItemModelError>
            saveItemModel : ItemModelVersion -> Result<unit, ItemModelError>
        }

        /// The in-memory double: it holds what it is given, built-ins included, in a map mutated only inside the closure,
        /// as `CatalogueProxy.createInMemory` keeps its store; it never answers the other two errors (spec 0050 S7.1).
        static member createInMemory () : ItemModelProxy =
            let store = ref (Map.empty : Map<ItemModelId, ItemModelVersion>)
            {
                listItemModels =
                    fun () ->
                        store.Value.Values
                        |> List.ofSeq
                        |> List.sortBy (fun (held : ItemModelVersion) -> held.entry.name, held.entry.id)
                        |> Ok
                resolveItemModelVersion =
                    fun versionId ->
                        store.Value
                        |> Map.tryFind versionId.itemModelId
                        |> Option.filter (fun (held : ItemModelVersion) -> held.version = versionId.version)
                        |> Ok
                saveItemModel =
                    fun itemModel ->
                        match store.Value |> Map.tryFind itemModel.entry.id with
                        | Some held ->
                            let entry = held.entry
                            Error (DuplicateItemModelErr $"'{entry.name.value}' ({entry.id.value}) at version {held.version.value}")
                        | None ->
                            store.Value <- store.Value |> Map.add itemModel.entry.id itemModel
                            Ok ()
            }

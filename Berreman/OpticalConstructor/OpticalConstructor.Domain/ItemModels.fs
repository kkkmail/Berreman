namespace OpticalConstructor.Domain

open Berreman.Media
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

/// The parameter vocabulary of items and item models, shared with the fitter, and the parameters the existing
/// models publish (spec 0050 S7.1).
module ItemModels =

    /// The name a fitted parameter is reported under — elevated so that a report cannot silently pair a
    /// name with the wrong column, and so that a name can never be passed where a reason or a description
    /// is expected. Spec 0050 S7.1 moved it here from `MuellerInverse`.
    type ParameterName =
        | ParameterName of string

        /// The name as text (the reporting seam).
        member this.value = let (ParameterName n) = this in n

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
                { name = ParameterName "rotationUncertainty"; kind = AngleParameter; defaultValue = rotation }
                { name = ParameterName "elementUncertainty"; kind = FractionParameter; defaultValue = element }
                { name = ParameterName "throughputUncertainty"; kind = FractionParameter; defaultValue = throughput }
            ]

    type Thickness with

        /// A finite thickness in metres as a parameter value; `Infinity` has none (spec 0050 S7.1).
        member this.parameterValueOpt : ParameterValue option =
            match this with
            | Thickness t -> Some (ParameterValue (float t))
            | Infinity -> None

    type SourcePreset with

        /// The output polarization and intensity scale over the preset's own λ and tunable limits, defaulting to
        /// unpolarized light at unit scale (spec 0050 S7.1; 001 §I, §N).
        member _.parameterAxes : ItemParameterAxis list =
            [
                { name = ParameterName "azimuth"; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "ellipticity"; kind = AngleParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "degreeOfPolarization"; kind = FractionParameter; defaultValue = ParameterValue 0.0 }
                { name = ParameterName "intensityScale"; kind = ScaleParameter; defaultValue = ParameterValue 1.0 }
            ]

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
                    match item with
                    | SingleLayer layer -> [ ParameterName $"film{i + 1}.thickness", layer ]
                    | Repeated group ->
                        group.cell |> List.mapi (fun j layer -> ParameterName $"film{i + 1}.layer{j + 1}.thickness", layer))
                |> List.concat
            let substrateLayers : (ParameterName * SampleLayer) list =
                this.structure.substrate |> Option.toList |> List.map (fun layer -> ParameterName "substrate.thickness", layer)
            filmLayers @ substrateLayers
            |> List.choose (fun (name, layer) ->
                layer.thickness.parameterValueOpt
                |> Option.map (fun value -> { name = name; kind = ThicknessParameter; defaultValue = value }))

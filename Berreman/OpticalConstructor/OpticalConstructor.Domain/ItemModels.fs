namespace OpticalConstructor.Domain

/// The parameter vocabulary of items and item models, shared with the fitter (spec 0050 S7.1).
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

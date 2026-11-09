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

namespace OpticalConstructor.Domain

open Berreman.Fields
open Berreman.Dispersion
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Experiments

/// Spec 0038 Part I (step 031) — the OUT-OF-BAND dispersion diagnostic. Pure and
/// Avalonia-free: given the materials reachable through an element's binding and
/// the wavelengths the actual experiment REQUESTS, it flags when some reachable
/// material carries DEFINED dispersion segments (`MaterialComplexityEditor.EditSegment.interval`,
/// realised on disk as each `EpsDispersiveValue` segment's `wavelengthInterval`)
/// and the requested wavelengths fall PARTLY OUTSIDE the union of those segments.
///
/// This is deliberately INDEPENDENT of the step-30 bound/unbound render cue — a
/// bound element whose material is defined only over 300–700 nm and a sweep out
/// to 800 nm is just as out-of-band as an unbound one. A CONSTANT material (a
/// `ConstantEpsValue` eps, no segments) NEVER flags — its physics is
/// wavelength-independent, so no request can leave its "defined range". A
/// coded-preset material (silicon / langasite, whose dispersion is a coded closure
/// valid at every wavelength) declares an UNBOUNDED band — its single re-seeded
/// segment spans the whole spectrum — so no request can leave its defined range and
/// it never flags: the diagnostic can only warn where a material DECLARES a finite band.
module OutOfBandDiagnostic =

    type WaveLengthInterval with

        /// The interval as a human range in nanometres (spec 0038 step 031), through the sole `Units`
        /// conversion seam. A degenerate interval (a fixed single λ) reads as one value, not "x–x".
        member this.nmRangeLabel : string =
            let lo = (wavelengthToUnit Nanometer this.lower).value
            let hi = (wavelengthToUnit Nanometer this.upper).value
            if lo = hi then $"%g{lo} nm"
            else $"%g{lo}–%g{hi} nm"

    /// The wavelengths an experiment actually requests (spec 0038 step 031): a
    /// single fixed λ (a fixed setting — the scene source's monochromatic
    /// wavelength), or the FULL swept range of a wavelength sweep. A DU, never a
    /// bare pair — a fixed request is not merely a degenerate range at the call
    /// site, it is a distinct intent.
    type RequestedWavelengths =
        | FixedWavelength of WaveLength
        | SweptWavelengths of WaveLengthInterval

        /// The request as a contiguous interval: a fixed λ is the degenerate
        /// `{ lower = λ; upper = λ }`; a sweep is its range verbatim. The coverage
        /// check reads this; the badge/Details text reports it.
        member this.span : WaveLengthInterval =
            match this with
            | FixedWavelength w -> { lower = w; upper = w }
            | SweptWavelengths interval -> interval

    /// A material reachable through an element's binding, reduced to exactly what
    /// the out-of-band check needs (spec 0038 step 031): a display name and the
    /// wavelength intervals of its DEFINED dispersion segments. A constant or
    /// coded-preset material carries NO defined segments, so it never contributes
    /// an offender.
    type ReachableMaterial =
        {
            materialName : EntryName
            definedSegments : WaveLengthInterval list
        }

        /// The wavelength intervals of a material's DEFINED eps dispersion segments
        /// (spec 0038 step 031): each segment's `wavelengthInterval` when the eps is
        /// segment-dispersive; EMPTY for a constant eps (`EpsWithoutDispValue`). This
        /// is the union input the coverage check merges — a constant material yields
        /// no segments and therefore never flags.
        static member definedSegmentsOf (complexity : MaterialComplexity) : WaveLengthInterval list =
            match complexity.eps with
            | EpsWithoutDispValue _ -> []
            | EpsWithDispValue dispersive ->
                match dispersive with
                | IsotropicDispersive segments -> segments |> List.map (fun s -> s.wavelengthInterval)
                | UniaxialDispersive segments -> segments |> List.map (fun s -> s.wavelengthInterval)
                | BiaxialDispersive segments -> segments |> List.map (fun s -> s.wavelengthInterval)

        /// The reachable-material view of a library material entry (spec 0038 step 031):
        /// its display name and its defined dispersion segments. An entry with coded
        /// dispersion (a single spectrum-spanning segment — silicon / langasite) declares
        /// an unbounded band, so no request leaves it and it never flags — the diagnostic
        /// only warns where a material declares a finite band. A crystal's tensors hold at every
        /// wavelength, so it defines no segment and never flags, as a constant complexity (spec 0050 S11.1).
        static member ofEntry (entry : MaterialEntry) : ReachableMaterial =
            {
                materialName = entry.name
                definedSegments =
                    match entry.physics with
                    | ComplexityPhysics complexity -> ReachableMaterial.definedSegmentsOf complexity
                    | CrystalParameterPhysics _ -> []
            }

    /// One material flagged out-of-band (spec 0038 step 031): its name and both
    /// ranges — the union span of its DEFINED dispersion and the experiment's
    /// REQUESTED range — so the tooltip / Details text can name the material and
    /// state where its physics is defined versus what the experiment asked for.
    type OutOfBandFinding =
        {
            materialName : EntryName
            definedRange : WaveLengthInterval
            requestedRange : WaveLengthInterval
        }

        /// The warning text for one offender (spec 0038 step 031): names the material
        /// and both ranges — where its dispersion is defined and what the experiment
        /// requests.
        member this.text : string =
            $"%s{this.materialName.value}: dispersion defined for %s{this.definedRange.nmRangeLabel}, but the experiment requests %s{this.requestedRange.nmRangeLabel}"

    /// The out-of-band coverage verdict for an element (spec 0038 step 031): every
    /// reachable material's defined dispersion covers the request (`InBand`), or
    /// some material leaves part of the request uncovered (`OutOfBand`, naming each
    /// offender and both ranges). An `OutOfBand` is only ever built with a non-empty
    /// offender list.
    type DispersionCoverage =
        | InBand
        | OutOfBand of OutOfBandFinding list

        /// The full warning text of a coverage verdict (spec 0038 step 031): one clause
        /// per offender, `None` when in-band. The SAME text is shown in the badge
        /// tooltip and the Details bay.
        member this.warningOpt : string option =
            match this with
            | InBand -> None
            | OutOfBand findings -> findings |> List.map (fun f -> f.text) |> String.concat "; " |> Some

    /// Merge a segment-interval list into the minimal set of disjoint intervals it
    /// covers, sorted by lower bound. Endpoints are INCLUSIVE, so two segments that
    /// meet at a single wavelength (300–500 nm and 500–700 nm) fuse into one
    /// (300–700 nm); a genuine gap (300–400 nm and 600–700 nm) stays two intervals.
    let private mergeIntervals (intervals : WaveLengthInterval list) : WaveLengthInterval list =
        intervals
        |> List.sortBy (fun i -> i.lower.value)
        |> List.fold
            (fun merged i ->
                match merged with
                | last :: rest when i.lower.value <= last.upper.value ->
                    // Overlap or touch: extend the current cluster's upper if this reaches further.
                    let upper = if i.upper.value > last.upper.value then i.upper else last.upper
                    { last with upper = upper } :: rest
                | _ -> i :: merged)
            []
        |> List.rev

    /// The merged interval of the defined segments that holds a contiguous requested
    /// span ENTIRELY, if any. Because the request is contiguous, full coverage means it
    /// fits inside ONE merged interval (a request that straddles a gap is not
    /// covered even if both its ends land in segments).
    let private coveringInterval (segments : WaveLengthInterval list) (request : WaveLengthInterval) : WaveLengthInterval option =
        let lo = min request.lower.value request.upper.value
        let hi = max request.lower.value request.upper.value
        mergeIntervals segments
        |> List.tryFind (fun i -> i.lower.value <= lo && hi <= i.upper.value)

    /// The union span of a material's defined segments — the widest lower and upper
    /// across them — reported as the material's "defined range" in a finding.
    let private definedSpan (segments : WaveLengthInterval list) : WaveLengthInterval =
        {
            lower = segments |> List.map (fun s -> s.lower) |> List.minBy (fun w -> w.value)
            upper = segments |> List.map (fun s -> s.upper) |> List.maxBy (fun w -> w.value)
        }

    /// The finding for ONE reachable material, if the request is not fully covered
    /// by its defined segments. A material with no defined segments (constant /
    /// coded) is never a finding.
    let private findingFor (requested : RequestedWavelengths) (material : ReachableMaterial) : OutOfBandFinding option =
        match material.definedSegments with
        | [] -> None
        | segments ->
            match coveringInterval segments requested.span with
            | Some _ -> None
            | None ->
                Some
                    {
                        materialName = material.materialName
                        definedRange = definedSpan segments
                        requestedRange = requested.span
                    }

    /// The out-of-band diagnostic (spec 0038 step 031): flag every reachable
    /// material whose DEFINED dispersion segments do not fully cover the
    /// experiment's requested wavelengths. Constant / coded materials never flag;
    /// when every reachable material covers the request the verdict is `InBand`.
    /// Independent of the element's bound/unbound state.
    let checkOutOfBandDispersion (materials : ReachableMaterial list) (requested : RequestedWavelengths) : DispersionCoverage =
        match materials |> List.choose (findingFor requested) with
        | [] -> InBand
        | findings -> OutOfBand findings

    /// The diagnostic over library material entries (spec 0038 step 031): resolve
    /// each entry to its reachable-material view, then run `checkOutOfBandDispersion`.
    let checkMaterialsOutOfBand (entries : MaterialEntry list) (requested : RequestedWavelengths) : DispersionCoverage =
        checkOutOfBandDispersion (entries |> List.map ReachableMaterial.ofEntry) requested

    /// The wavelengths a swept setting requests (spec 0038 step 031). A WAVELENGTH
    /// sweep (`VaryWaveLength`) requests its full range (`range.min…range.max`, in
    /// NANOMETRES — the `VariableRange` display unit for wavelength); every other
    /// variable (or none) is FIXED at `fixedWavelength` (the scene source's single
    /// λ). The nm endpoints cross to the engine `WaveLength` through the sole
    /// `Units.toWaveLength` seam.
    let requestedWavelengthsFor
        (fixedWavelength : WaveLength)
        (varied : VariableParameter option)
        (range : VariableRange)
        : RequestedWavelengths =
        match varied with
        | Some VaryWaveLength ->
            SweptWavelengths
                {
                    lower = toWaveLength Nanometer range.min.toUnitMagnitude
                    upper = toWaveLength Nanometer range.max.toUnitMagnitude
                }
        | Some VaryR1
        | Some VaryR2
        | Some VaryDialReading
        | None -> FixedWavelength fixedWavelength

    /// The wavelengths an `Experiment` requests (spec 0038 step 031): its scan's wavelength axes from the lowest `min`
    /// to the highest `max`, read as one range by `requestedWavelengthsFor`, else the fixed scene-source λ (spec 0050 S9.1).
    let requestedWavelengthsOf (fixedWavelength : WaveLength) (experiment : Experiment) : RequestedWavelengths =
        match experiment.scan.axes |> List.filter (fun axis -> axis.variable = VaryWaveLength) with
        | [] -> FixedWavelength fixedWavelength
        | first :: _ as swept ->
            let span : VariableRange =
                {
                    first.range with
                        min = swept |> List.map (fun axis -> axis.range.min) |> List.minBy (fun value -> value.value)
                        max = swept |> List.map (fun axis -> axis.range.max) |> List.maxBy (fun value -> value.value)
                }
            requestedWavelengthsFor fixedWavelength (Some VaryWaveLength) span


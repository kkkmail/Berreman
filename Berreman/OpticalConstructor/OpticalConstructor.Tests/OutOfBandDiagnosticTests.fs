namespace OpticalConstructor.Tests

open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.OutOfBandDiagnostic
open Xunit

/// Spec 0038 Part I (step 031) — the pure OUT-OF-BAND dispersion diagnostic. A fixed 600 nm request
/// inside a 300–700 nm segment MUST NOT flag; a 200–800 nm sweep against the same segment MUST flag,
/// naming the material and both ranges; multi-segment unions cover a request spanning them (a gap does
/// not); a CONSTANT material (and a coded-preset `complexity = None`) NEVER flags. The requested-
/// wavelengths derivation and the tooltip/Details text are proven here too — all without a window.
module OutOfBandDiagnosticTests =

    /// A `WaveLength` from a nm scalar through the sole `Units` seam.
    let private nm (x : float) : WaveLength = toWaveLength Nanometer x

    /// A wavelength interval from nm endpoints.
    let private band (lo : float) (hi : float) : WaveLengthInterval = { lower = nm lo; upper = nm hi }

    /// A reachable material with the given defined dispersion segments.
    let private reachable (name : string) (segments : WaveLengthInterval list) : ReachableMaterial =
        { materialName = name; definedSegments = segments }

    /// A CONSTANT-eps material complexity (isotropic transparent) — no dispersion segments.
    let private constantComplexity : MaterialComplexity =
        { eps = EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.5)); magnetic = None; active = None }

    /// A dispersive material complexity whose eps carries exactly the given segment intervals (the
    /// per-axis dispersion content is irrelevant to the diagnostic, so a flat evaluated axis seeds it).
    let private dispersiveComplexity (segments : WaveLengthInterval list) : MaterialComplexity =
        let axis = EpsAxisEvaluated (fun _ -> ComplexRefractionIndex (createComplex 1.5 0.0))
        {
            eps =
                segments
                |> List.map (fun iv -> { wavelengthInterval = iv; dispersion = axis })
                |> IsotropicDispersive
                |> EpsWithDispValue
            magnetic = None
            active = None
        }

    /// A library material entry wrapping a complexity (its `properties` derived from it, as the seeds do).
    let private entryOf (name : string) (c : MaterialComplexity) : MaterialEntry =
        {
            id = newMaterialId ()
            name = name
            category = CategoryIds.glass
            description = None
            properties = c.toProperties
            complexity = Some c
        }

    // ============================ the acceptance cases ============================

    [<Fact>]
    let ``a fixed 600 nm request inside a 300-700 nm segment does not flag`` () =
        let material = reachable "Silica" [ band 300.0 700.0 ]
        let coverage = checkOutOfBandDispersion [ material ] (FixedWavelength (nm 600.0))
        Assert.Equal(InBand, coverage)
        Assert.Equal(None, coverageWarning coverage)

    [<Fact>]
    let ``a 200-800 nm sweep against a 300-700 nm segment flags, naming the material and both ranges`` () =
        let material = reachable "Silica" [ band 300.0 700.0 ]
        let coverage = checkOutOfBandDispersion [ material ] (SweptWavelengths (band 200.0 800.0))
        match coverage with
        | OutOfBand [ finding ] ->
            Assert.Equal("Silica", finding.materialName)
            Assert.Equal(band 300.0 700.0, finding.definedRange)
            Assert.Equal(band 200.0 800.0, finding.requestedRange)
        | other -> failwith $"expected exactly one offender, got %A{other}"

    [<Fact>]
    let ``an adjacent multi-segment union covers a request spanning both segments`` () =
        // 300-500 and 500-700 fuse at 500 into 300-700; a 350-650 sweep sits inside the union.
        let material = reachable "TwoBand" [ band 300.0 500.0; band 500.0 700.0 ]
        Assert.Equal(InBand, checkOutOfBandDispersion [ material ] (SweptWavelengths (band 350.0 650.0)))
        // A 200-800 sweep still overruns the fused union on both ends.
        Assert.NotEqual<DispersionCoverage>(InBand, checkOutOfBandDispersion [ material ] (SweptWavelengths (band 200.0 800.0)))

    [<Fact>]
    let ``a request landing in the gap between two disjoint segments flags`` () =
        // 300-400 and 600-700 leave a gap; a fixed 500 nm falls in it.
        let material = reachable "GapBand" [ band 300.0 400.0; band 600.0 700.0 ]
        Assert.NotEqual<DispersionCoverage>(InBand, checkOutOfBandDispersion [ material ] (FixedWavelength (nm 500.0)))
        // A wavelength that lands inside the second segment is covered.
        Assert.Equal(InBand, checkOutOfBandDispersion [ material ] (FixedWavelength (nm 650.0)))

    [<Fact>]
    let ``a constant material never flags, even for a wide sweep`` () =
        // No defined segments, so no request can be out of band.
        let material = reachable "Glass" []
        Assert.Equal(InBand, checkOutOfBandDispersion [ material ] (SweptWavelengths (band 100.0 2000.0)))
        // The same through the MaterialComplexity extractor: a constant eps yields no segments.
        Assert.Empty(definedSegmentIntervals constantComplexity)
        let entry = entryOf "Glass" constantComplexity
        Assert.Empty((reachableMaterialOf entry).definedSegments)
        Assert.Equal(InBand, checkMaterialsOutOfBand [ entry ] (SweptWavelengths (band 100.0 2000.0)))

    [<Fact>]
    let ``a coded-preset material (spectrum-spanning band) never flags`` () =
        // Silicon is dispersive but its dispersion is a CODED closure valid at every
        // wavelength (spec 0040 step 004 re-seeds it with a single spectrum-spanning
        // segment — an UNBOUNDED band), so no request can leave its defined range and
        // the diagnostic never flags it — exactly as the former `complexity = None`
        // coded preset never flagged.
        let silicon = builtInEntries |> List.find (fun e -> e.id = MaterialIds.silicon)
        match silicon.complexity with
        | Some _ -> ()
        | None -> Assert.Fail "silicon must carry a complexity value tree after the step-004 re-seed"
        Assert.Equal(InBand, checkMaterialsOutOfBand [ silicon ] (SweptWavelengths (band 200.0 800.0)))
        // Even an extreme sweep is covered by the unbounded band.
        Assert.Equal(InBand, checkMaterialsOutOfBand [ silicon ] (SweptWavelengths (band 1.0 100000.0)))

    // ============================ extractor + entry path ============================

    [<Fact>]
    let ``definedSegmentIntervals reads each dispersive segment's interval`` () =
        let intervals = [ band 300.0 500.0; band 500.0 700.0 ]
        let c = dispersiveComplexity intervals
        Assert.Equal<WaveLengthInterval list>(intervals, definedSegmentIntervals c)

    [<Fact>]
    let ``checkMaterialsOutOfBand flags a dispersive entry whose defined band the sweep overruns`` () =
        let entry = entryOf "Sellmeier glass" (dispersiveComplexity [ band 300.0 700.0 ])
        match checkMaterialsOutOfBand [ entry ] (SweptWavelengths (band 200.0 800.0)) with
        | OutOfBand [ finding ] ->
            Assert.Equal("Sellmeier glass", finding.materialName)
            Assert.Equal(band 300.0 700.0, finding.definedRange)
        | other -> failwith $"expected exactly one offender, got %A{other}"

    [<Fact>]
    let ``only the offending materials among several reachable are flagged`` () =
        // A constant, an in-band dispersive, and an out-of-band dispersive material reachable together;
        // only the last flags.
        let materials =
            [
                reachable "Glass" []                              // constant → never
                reachable "InBand" [ band 200.0 900.0 ]           // covers 200-800 → not flagged
                reachable "OutOfBand" [ band 400.0 700.0 ]        // 200-800 overruns → flagged
            ]
        match checkOutOfBandDispersion materials (SweptWavelengths (band 200.0 800.0)) with
        | OutOfBand [ finding ] -> Assert.Equal("OutOfBand", finding.materialName)
        | other -> failwith $"expected exactly one offender, got %A{other}"

    // ============================ requested-wavelengths derivation ============================

    [<Fact>]
    let ``requestedWavelengthsFor: a wavelength sweep requests the nm range; other variables stay fixed`` () =
        let range : VariableRange = { min = 200.0; max = 800.0; points = 91 }
        let fixedλ = nm 600.0
        Assert.Equal(SweptWavelengths (band 200.0 800.0), requestedWavelengthsFor fixedλ (Some VaryWaveLength) range)
        Assert.Equal(FixedWavelength fixedλ, requestedWavelengthsFor fixedλ (Some VaryR1) range)
        Assert.Equal(FixedWavelength fixedλ, requestedWavelengthsFor fixedλ (Some VaryR2) range)
        Assert.Equal(FixedWavelength fixedλ, requestedWavelengthsFor fixedλ None range)

    [<Fact>]
    let ``requestedWavelengthsOf: a wavelength-sweep experiment requests its range; a dark line is fixed`` () =
        let fixedλ = nm 600.0
        let sweep : Experiment =
            {
                id = ExperimentId 1
                setup = []
                varied = Some { VariedElement.elementId = OpticalConstructor.Domain.Library.elementId "src"; variable = VaryWaveLength }
                measurement = CaptureTransmitted
                range = { min = 200.0; max = 800.0; points = 91 }
                dataFileOpt = None
            }
        Assert.Equal(SweptWavelengths (band 200.0 800.0), requestedWavelengthsOf fixedλ sweep)
        // A dark line (varied = None) has no sweep, so it runs at the fixed source λ.
        let darkLine = seedExperiments |> List.head
        Assert.Equal(FixedWavelength fixedλ, requestedWavelengthsOf fixedλ darkLine)

    // ============================ the tooltip / Details text ============================

    [<Fact>]
    let ``coverageWarning names the material and both nm ranges`` () =
        let material = reachable "Silica" [ band 300.0 700.0 ]
        match coverageWarning (checkOutOfBandDispersion [ material ] (SweptWavelengths (band 200.0 800.0))) with
        | Some text ->
            // Names the material and states both the DEFINED (300..700) and REQUESTED (200..800) ranges.
            Assert.Contains("Silica", text)
            Assert.Contains("300", text)
            Assert.Contains("700", text)
            Assert.Contains("200", text)
            Assert.Contains("800", text)
        | None -> failwith "expected an out-of-band warning"

    [<Fact>]
    let ``nmRangeLabel reads a fixed single wavelength as one value and a range as two`` () =
        Assert.Equal("600 nm", nmRangeLabel (band 600.0 600.0))
        let ranged = nmRangeLabel (band 300.0 700.0)
        Assert.Contains("300", ranged)
        Assert.Contains("700", ranged)
        Assert.NotEqual<string>("300 nm", ranged)

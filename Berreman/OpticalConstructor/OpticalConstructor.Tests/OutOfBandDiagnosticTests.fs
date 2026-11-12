namespace OpticalConstructor.Tests

open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.OutOfBandDiagnostic
open Xunit

/// Spec 0038 Part I (step 031) — the pure OUT-OF-BAND dispersion diagnostic. A fixed 600 nm request
/// inside a 300–700 nm segment MUST NOT flag; a 200–800 nm sweep against the same segment MUST flag,
/// naming the material and both ranges; multi-segment unions cover a request spanning them (a gap does
/// not); a CONSTANT material (and BuiltinCatalogue.material's unbounded band) NEVER flags. The requested-
/// wavelengths derivation and the tooltip/Details text are proven here too — all without a window.
module OutOfBandDiagnosticTests =

    /// A `WaveLength` from a nm scalar through the sole `Units` seam.
    let private nm (x : float) : WaveLength = toWaveLength Nanometer (UnitMagnitude x)

    /// A wavelength interval from nm endpoints.
    let private band (lo : float) (hi : float) : WaveLengthInterval = { lower = nm lo; upper = nm hi }

    /// A reachable material with the given defined dispersion segments.
    let private reachable (name : string) (segments : WaveLengthInterval list) : ReachableMaterial =
        { materialName = EntryName name; definedSegments = segments }

    /// A CONSTANT-eps material complexity (isotropic transparent) — no dispersion segments.
    let private constantComplexity : MaterialComplexity =
        { eps = EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.5)); magnetic = None; active = None }

    /// A dispersive material complexity whose eps carries exactly the given segment intervals (the
    /// per-axis dispersion content is irrelevant to the diagnostic, so a flat data axis seeds it: the
    /// constant `RealNK` n = 1.5, k = 0 that `ConstantNK` lowers to, spec 0050 step 9).
    let private dispersiveComplexity (segments : WaveLengthInterval list) : MaterialComplexity =
        let axis = (ConstantNK { n = 1.5; k = 0.0; wavelengthUnit = Nanometer; thermoOptic = None }).toEpsAxis
        {
            eps =
                segments
                |> List.map (fun iv -> { wavelengthInterval = iv; dispersion = axis })
                |> IsotropicDispersive
                |> EpsWithDispValue
            magnetic = None
            active = None
        }

    /// A library material entry wrapping a complexity.
    let private entryOf (name : string) (c : MaterialComplexity) : MaterialEntry =
        {
            id = newMaterialId ()
            name = EntryName name
            category = CategoryIds.glass
            description = None
            complexity = c
        }

    // ============================ the acceptance cases ============================

    [<Fact>]
    let ``a fixed 600 nm request inside a 300-700 nm segment does not flag`` () =
        let material = reachable "Silica" [ band 300.0 700.0 ]
        let coverage = checkOutOfBandDispersion [ material ] (FixedWavelength (nm 600.0))
        Assert.Equal(InBand, coverage)
        Assert.Equal(None, coverage.warningOpt)

    [<Fact>]
    let ``a 200-800 nm sweep against a 300-700 nm segment flags, naming the material and both ranges`` () =
        let material = reachable "Silica" [ band 300.0 700.0 ]
        let coverage = checkOutOfBandDispersion [ material ] (SweptWavelengths (band 200.0 800.0))
        match coverage with
        | OutOfBand [ finding ] ->
            Assert.Equal(EntryName "Silica", finding.materialName)
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
        Assert.Empty(ReachableMaterial.definedSegmentsOf constantComplexity)
        let entry = entryOf "Glass" constantComplexity
        Assert.Empty((ReachableMaterial.ofEntry entry).definedSegments)
        Assert.Equal(InBand, checkMaterialsOutOfBand [ entry ] (SweptWavelengths (band 100.0 2000.0)))

    [<Fact>]
    let ``a coded-preset material (spectrum-spanning band) never flags`` () =
        // BuiltinCatalogue.material retains Silicon's spectrum-spanning segment, so no request
        // leaves its defined range (spec 0040 step 004; spec 0050 S1.7).
        let silicon = BuiltinCatalogue.material MaterialIds.silicon
        Assert.Equal(InBand, checkMaterialsOutOfBand [ silicon ] (SweptWavelengths (band 200.0 800.0)))
        // Even an extreme sweep is covered by the unbounded band.
        Assert.Equal(InBand, checkMaterialsOutOfBand [ silicon ] (SweptWavelengths (band 1.0 100000.0)))

    // ============================ extractor + entry path ============================

    [<Fact>]
    let ``ReachableMaterial.definedSegmentsOf reads each dispersive segment's interval`` () =
        let intervals = [ band 300.0 500.0; band 500.0 700.0 ]
        let c = dispersiveComplexity intervals
        Assert.Equal<WaveLengthInterval list>(intervals, ReachableMaterial.definedSegmentsOf c)

    [<Fact>]
    let ``checkMaterialsOutOfBand flags a dispersive entry whose defined band the sweep overruns`` () =
        let entry = entryOf "Sellmeier glass" (dispersiveComplexity [ band 300.0 700.0 ])
        match checkMaterialsOutOfBand [ entry ] (SweptWavelengths (band 200.0 800.0)) with
        | OutOfBand [ finding ] ->
            Assert.Equal(EntryName "Sellmeier glass", finding.materialName)
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
        | OutOfBand [ finding ] -> Assert.Equal(EntryName "OutOfBand", finding.materialName)
        | other -> failwith $"expected exactly one offender, got %A{other}"

    // ============================ requested-wavelengths derivation ============================

    [<Fact>]
    let ``requestedWavelengthsFor: a wavelength sweep requests the nm range; other variables stay fixed`` () =
        let range : VariableRange = { min = OpticalConstructor.Domain.Propagation.SweptValue 200.0; max = OpticalConstructor.Domain.Propagation.SweptValue 800.0; points = OpticalConstructor.Domain.Propagation.CurvePointCount 91 }
        let fixedλ = nm 600.0
        Assert.Equal(SweptWavelengths (band 200.0 800.0), requestedWavelengthsFor fixedλ (Some VaryWaveLength) range)
        Assert.Equal(FixedWavelength fixedλ, requestedWavelengthsFor fixedλ (Some VaryR1) range)
        Assert.Equal(FixedWavelength fixedλ, requestedWavelengthsFor fixedλ (Some VaryR2) range)
        Assert.Equal(FixedWavelength fixedλ, requestedWavelengthsFor fixedλ None range)

    [<Fact>]
    let ``requestedWavelengthsOf: a wavelength-sweep experiment requests its range; a dark line is fixed`` () =
        let fixedλ = nm 600.0
        // A test-local setup revision both runs reference (spec 0050 S8.2).
        let bench : OpticalConstructor.Domain.Kit.SetupReference =
            {
                name = OpticalConstructor.Domain.Kit.SetupName "Bench"
                revision = OpticalConstructor.Domain.Kit.SetupRevision 1
            }
        // The source's wavelength over `lo`…`hi` nm.
        let sourceSweep (lo : float) (hi : float) : ScanAxis =
            {
                elementId = OpticalConstructor.Domain.Library.ElementId "src"
                label = ElementLabel "Light source #1"
                variable = VaryWaveLength
                range = { min = OpticalConstructor.Domain.Propagation.SweptValue lo; max = OpticalConstructor.Domain.Propagation.SweptValue hi; points = OpticalConstructor.Domain.Propagation.CurvePointCount 91 }
            }
        let sweep : Experiment =
            {
                id = ExperimentId 1
                name = (ExperimentId 1).defaultName
                setup = bench
                scan = ExperimentScan.oneAxis (sourceSweep 200.0 800.0) CaptureTransmitted None
                role = ScienceRun
                sessionOpt = None
                notes = ExperimentNotes.none
                dataOpt = None
                corrections = []
            }
        Assert.Equal(SweptWavelengths (band 200.0 800.0), requestedWavelengthsOf fixedλ sweep)
        // A dark line (a block of no axis) has no sweep, so it runs at the fixed source λ.
        let darkLine : Experiment =
            {
                sweep with scan = ExperimentScan.darkLine CaptureTransmitted
            }
        Assert.Equal(FixedWavelength fixedλ, requestedWavelengthsOf fixedλ darkLine)
        // Spec 0050 S9.1: two blocks sweeping 300…500 nm and 400…700 nm request their span, 300…700 nm; an R1 axis
        // beside them requests nothing more.
        let analyzerTurn : ScanAxis =
            {
                elementId = OpticalConstructor.Domain.Library.ElementId "lp"
                label = ElementLabel "Linear polarizer #2"
                variable = VaryR1
                range = VariableRange.forVariable VaryR1
            }
        let twoSweeps : Experiment =
            {
                sweep with
                    scan =
                        {
                            blocks =
                                [
                                    { axes = [ sourceSweep 300.0 500.0 ]; capture = CaptureTransmitted }
                                    { axes = [ analyzerTurn; sourceSweep 400.0 700.0 ]; capture = CaptureBoth }
                                ]
                            sourceWaveLengthOpt = None
                        }
            }
        Assert.Equal(SweptWavelengths (band 300.0 700.0), requestedWavelengthsOf fixedλ twoSweeps)

    // ============================ the tooltip / Details text ============================

    [<Fact>]
    let ``coverageWarning names the material and both nm ranges`` () =
        let material = reachable "Silica" [ band 300.0 700.0 ]
        match (checkOutOfBandDispersion [ material ] (SweptWavelengths (band 200.0 800.0))).warningOpt with
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
        Assert.Equal("600 nm", (band 600.0 600.0).nmRangeLabel)
        let ranged = (band 300.0 700.0).nmRangeLabel
        Assert.Contains("300", ranged)
        Assert.Contains("700", ranged)
        Assert.NotEqual<string>("300 nm", ranged)

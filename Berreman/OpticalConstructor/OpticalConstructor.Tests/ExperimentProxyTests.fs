namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary       // VersionNumber (spec 0038 Part H)
open OpticalConstructor.Domain.Lifecycle             // VersionRef / VersionsInUse / SampleVersionRef
open OpticalConstructor.Domain.SampleStore           // SampleProxy.createInMemory (versioned store)
open OpticalConstructor.Domain.Experiments

/// Spec 0027 (028) / spec 0038 Part I (step 025) — pure domain tests for the Experiments module. The
/// element-constrained variable set (`variablesFor`, DATA per kind), the T/R/both capture
/// (`MeasurementMode`, defaulted from emission), the ranges, and the editable `ExperimentCollection`
/// (choose → commit adds; re-commit updates in place; edit loads; remove deletes) are unchanged. Step 025
/// adds: the grown `Experiment` carrying an ordered `ElementDescriptor` setup (kind + placement/orientation
/// summary + versioned binding) with the varied element identified within the list — sample and source
/// optional (E1/E2/E3), the detector part of the chain; the `DataFilePath` attachment; and the
/// `boundVersions` / `versionsInUseSeam` builders wired into the versioned sample store's used-version rule.
module ExperimentProxyTests =

    let private proxy = Experiments.createInMemory ()
    let private el (s : string) : ElementId = Library.elementId s

    /// A placement/orientation summary for a rest-pose element of the given kind.
    let private placementOf (kind : CatalogueKind) : PlacementSummary =
        PlacementSummary.ofPlacement (ElementPlacement.create kind TablePoint.origin)

    /// A captured setup descriptor (id / label / kind / rest placement / binding).
    let private desc (idStr : string) (label : string) (kind : CatalogueKind) (binding : ElementBinding) : ElementDescriptor =
        { ElementDescriptor.elementId = el idStr; label = label; kind = kind; placement = placementOf kind; binding = binding }

    /// The version-one binding of the seeded 1 mm glass plate — a sample bound BY VERSION.
    let private glassPlateBinding : ElementBinding =
        BoundByVersion (SampleVersionRef (SampleVersionId.firstOf SeedSamples.glassPlate1mm.id))

    // A single-source setup and a single-polarizer setup for the interactive-commit tests.
    let private srcSetup : ElementDescriptor list = [ desc "src" "Light source #1" LightSource (BoundByEntryId "src-600") ]
    let private polSetup : ElementDescriptor list = [ desc "pol" "Linear polarizer #2" LinearPolarizer (BoundByEntryId "pol-lp") ]

    // ============================ variable capability (attached to the kind, as DATA) ============================

    [<Fact>]
    let ``variablesFor is data-driven per kind (source=λ, polarizer=R1, sample=R1+R2, others none)`` () =
        Assert.Equal<VariableParameter list>([ VaryWaveLength ], variablesFor LightSource)
        Assert.Equal<VariableParameter list>([ VaryR1 ], variablesFor LinearPolarizer)
        Assert.Equal<VariableParameter list>([ VaryR1 ], variablesFor CircularPolarizer)
        Assert.Equal<VariableParameter list>([ VaryR1; VaryR2 ], variablesFor Sample)
        Assert.Empty(variablesFor Lens)
        Assert.Empty(variablesFor FlatMirror)
        Assert.Empty(variablesFor Detector)

    [<Fact>]
    let ``each variable carries a code, a label, and a display unit`` () =
        Assert.Equal("wavelength", VaryWaveLength.code)
        Assert.Equal("r1", VaryR1.code)
        Assert.Equal("r2", VaryR2.code)
        Assert.Equal("nm", VaryWaveLength.unitLabel)
        Assert.Equal("°", VaryR1.unitLabel)
        Assert.False(System.String.IsNullOrEmpty VaryR2.label)

    [<Fact>]
    let ``the default range depends on the variable`` () =
        Assert.Equal({ min = 0.0; max = 360.0; points = 73 }, VariableRange.forVariable VaryR1)
        Assert.Equal({ min = 0.0; max = 89.0; points = 91 }, VariableRange.forVariable VaryR2)
        Assert.Equal({ min = 200.0; max = 800.0; points = 91 }, VariableRange.forVariable VaryWaveLength)

    // ============================ measurement (T / R / both), defaulted from emission ============================

    [<Fact>]
    let ``MeasurementMode captures the right branches`` () =
        Assert.True(CaptureTransmitted.capturesTransmitted)
        Assert.False(CaptureTransmitted.capturesReflected)
        Assert.True(CaptureReflected.capturesReflected)
        Assert.False(CaptureReflected.capturesTransmitted)
        Assert.True(CaptureBoth.capturesTransmitted)
        Assert.True(CaptureBoth.capturesReflected)

    [<Fact>]
    let ``ofEmission defaults a mirror to R and everything else to both`` () =
        Assert.Equal(CaptureReflected, MeasurementMode.ofEmission EmitReflectedOnly)
        Assert.Equal(CaptureTransmitted, MeasurementMode.ofEmission EmitTransmittedOnly)
        Assert.Equal(CaptureBoth, MeasurementMode.ofEmission EmitBoth)
        // A freshly-placed mirror emits reflected only, so a varied mirror defaults to R (not hard-coded).
        Assert.Equal(CaptureReflected, MeasurementMode.ofEmission (defaultEmission FlatMirror))
        Assert.Equal(CaptureBoth, MeasurementMode.ofEmission (defaultEmission Sample))

    // ============================ the editable collection (choose / commit / edit / remove) ============================

    /// A collection with the source picked (λ variable, both capture allowed) — the common editing start.
    let private withSource () : ExperimentCollection =
        ExperimentCollection.empty
        |> chooseElement (el "src") "Light source #1" (variablesFor LightSource) CaptureTransmitted

    [<Fact>]
    let ``chooseElement defaults the variable to the first allowed and the range to its default`` () =
        let c = withSource ()
        Assert.Equal(Some (el "src"), c.draft.elementId)
        Assert.Equal(Some VaryWaveLength, c.draft.variable)
        Assert.Equal(VariableRange.forVariable VaryWaveLength, c.draft.range)
        Assert.True(canCommit c)

    [<Fact>]
    let ``an element with nothing to vary leaves the variable unset and uncommittable`` () =
        let c = ExperimentCollection.empty |> chooseElement (el "lens") "Lens #1" (variablesFor Lens) CaptureTransmitted
        Assert.Equal(None, c.draft.variable)
        Assert.False(canCommit c)

    [<Fact>]
    let ``chooseVariable resets the range to the new variable's default`` () =
        let c =
            ExperimentCollection.empty
            |> chooseElement (el "s") "Sample #1" (variablesFor Sample) CaptureBoth
            |> chooseVariable VaryR2
        Assert.Equal(Some VaryR2, c.draft.variable)
        Assert.Equal(VariableRange.forVariable VaryR2, c.draft.range)

    [<Fact>]
    let ``commit APPENDS a new experiment and then keeps editing it (no duplicate on re-commit)`` () =
        let c0 = withSource ()
        let c1 = commit srcSetup c0
        Assert.Equal(1, List.length c1.experiments)
        let added = List.head c1.experiments
        Assert.Equal(Some (el "src"), added.varied |> Option.map (fun v -> v.elementId))
        Assert.Equal(Some VaryWaveLength, added.varied |> Option.map (fun v -> v.variable))
        // The draft now edits the added experiment, so committing again UPDATES rather than appends.
        let c2 = commit srcSetup c1
        Assert.Equal(1, List.length c2.experiments)

    [<Fact>]
    let ``editing the draft after add updates the stored experiment in place`` () =
        let c1 = withSource () |> commit srcSetup
        let id = (List.head c1.experiments).id
        let c2 = c1 |> chooseMeasurement CaptureReflected |> setRangeMin 300.0 |> setRangeMax 500.0 |> commit srcSetup
        Assert.Equal(1, List.length c2.experiments)
        let updated = c2.experiments |> List.find (fun e -> e.id = id)
        Assert.Equal(CaptureReflected, updated.measurement)
        Assert.Equal(300.0, updated.range.min)
        Assert.Equal(500.0, updated.range.max)

    [<Fact>]
    let ``newDraft after add lets a second commit APPEND a distinct experiment`` () =
        let c1 = withSource () |> commit srcSetup
        let c2 =
            c1
            |> newDraft
            |> chooseElement (el "pol") "Linear polarizer #2" (variablesFor LinearPolarizer) CaptureBoth
            |> commit polSetup
        Assert.Equal(2, List.length c2.experiments)
        let ids = c2.experiments |> List.map (fun e -> e.id.value)
        Assert.Equal(List.length ids, ids |> List.distinct |> List.length)   // distinct ids

    [<Fact>]
    let ``edit loads an experiment into the draft for re-editing`` () =
        let c1 =
            withSource () |> commit srcSetup
            |> newDraft
            |> chooseElement (el "pol") "Linear polarizer #2" (variablesFor LinearPolarizer) CaptureTransmitted
            |> commit polSetup
        let target = c1.experiments |> List.find (fun e -> (e.varied |> Option.map (fun v -> v.elementId)) = Some (el "pol"))
        let c2 = edit target.id c1
        Assert.Equal(Some target.id, c2.draft.editingId)
        Assert.Equal(Some (el "pol"), c2.draft.elementId)
        Assert.Equal(Some VaryR1, c2.draft.variable)

    [<Fact>]
    let ``remove deletes the experiment and clears the draft when it was being edited`` () =
        let c1 = withSource () |> commit srcSetup
        let id = (List.head c1.experiments).id
        let c2 = remove id c1
        Assert.Empty(c2.experiments)
        // It was the one being edited, so the draft resets (editingId cleared).
        Assert.Equal(None, c2.draft.editingId)
        Assert.Equal(None, c2.draft.elementId)

    [<Fact>]
    let ``an experiment description mentions the element, the variable, and the capture`` () =
        let c1 = withSource () |> chooseMeasurement CaptureBoth |> commit srcSetup
        let exp = List.head c1.experiments
        Assert.Contains("Light source", exp.description)
        Assert.Contains("Wavelength", exp.description)
        Assert.Contains("Both", exp.description)

    [<Fact>]
    let ``setRangePoints keeps at least two points`` () =
        let c = withSource () |> setRangePoints 1
        Assert.True(c.draft.range.points >= 2)

    // ============================ step 025: the full-setup experiment (E1 / E2 / E3) ============================

    /// E1 = source + polarizer + sample + rotating polarizer + detector (the rotating polarizer varied).
    let private e1 : Experiment =
        {
            id = ExperimentId 1
            setup =
                [
                    desc "src" "Light source #1" LightSource (BoundByEntryId "src-600")
                    desc "lp1" "Linear polarizer #2" LinearPolarizer (BoundByEntryId "pol-lp")
                    desc "s" "Sample #3" Sample glassPlateBinding
                    desc "lp2" "Linear polarizer #4" LinearPolarizer (BoundByEntryId "pol-lp")
                    desc "det" "Detector #5" Detector (BoundByEntryId "det-intensity")
                ]
            varied = Some { elementId = el "lp2"; variable = VaryR1 }
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = None
        }

    /// E2 = the same WITHOUT a sample (captures that the polarizers are not ideal).
    let private e2 : Experiment =
        { e1 with
            id = ExperimentId 2
            setup =
                [
                    desc "src" "Light source #1" LightSource (BoundByEntryId "src-600")
                    desc "lp1" "Linear polarizer #2" LinearPolarizer (BoundByEntryId "pol-lp")
                    desc "lp2" "Linear polarizer #3" LinearPolarizer (BoundByEntryId "pol-lp")
                    desc "det" "Detector #4" Detector (BoundByEntryId "det-intensity")
                ] }

    /// E3 = an empty dark line (nothing varied) — how much ambient light reaches the detector with no source.
    let private e3 : Experiment =
        { id = ExperimentId 3; setup = []; varied = None; measurement = CaptureTransmitted; range = VariableRange.forVariable VaryR1; dataFileOpt = None }

    [<Fact>]
    let ``E1 carries the ordered chain incl. the detector, with the varied polarizer identified within it`` () =
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; Sample; LinearPolarizer; Detector ],
            e1.setup |> List.map (fun d -> d.kind))
        // the varied element resolves to a descriptor inside the setup
        Assert.Equal(Some (el "lp2"), e1.varied |> Option.map (fun v -> v.elementId))
        Assert.Equal("Linear polarizer #4", e1.variedLabel)
        // the detector is part of the chain and carries its kind (via its preset binding)
        match e1.detectorDescriptorOpt with
        | Some d ->
            Assert.Equal(Detector, d.kind)
            Assert.Equal(BoundByEntryId "det-intensity", d.binding)
        | None -> Assert.Fail "E1 lost its detector from the captured chain"

    [<Fact>]
    let ``E2 is constructible without a sample (source + two polarizers + detector)`` () =
        Assert.DoesNotContain(Sample, e2.setup |> List.map (fun d -> d.kind))
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; LinearPolarizer; Detector ],
            e2.setup |> List.map (fun d -> d.kind))

    [<Fact>]
    let ``E3 is an empty dark line — nothing varied, still describable`` () =
        Assert.Empty(e3.setup)
        Assert.Equal(None, e3.varied)
        Assert.Equal("", e3.variedLabel)
        Assert.Contains("Dark line", e3.description)

    [<Fact>]
    let ``E1 and E2 describe their varied element, variable and capture`` () =
        Assert.Contains("Linear polarizer #4", e1.description)
        Assert.Contains("Rotation R1", e1.description)
        Assert.Contains("Transmitted", e1.description)
        Assert.Contains("Linear polarizer #3", e2.description)

    // ============================ step 025: the DataFilePath attachment ============================

    [<Fact>]
    let ``DataFilePath elevates a raw path and attachDataFile binds one per experiment`` () =
        let path = DataFilePath.create "C:/data/run1.csv"
        Assert.Equal("C:/data/run1.csv", path.value)
        let c = withSource () |> commit srcSetup
        let id = (List.head c.experiments).id
        Assert.Equal(None, (List.head c.experiments).dataFileOpt)
        let c2 = attachDataFile id path c
        let exp = c2.experiments |> List.find (fun e -> e.id = id)
        Assert.Equal(Some path, exp.dataFileOpt)

    // ============================ step 025: versionsInUse over the descriptors ============================

    [<Fact>]
    let ``boundVersions collects the versioned bindings of the setup (samples yes, presets no)`` () =
        let bound = boundVersions [ e1 ]
        Assert.Contains(SampleVersionRef (SampleVersionId.firstOf SeedSamples.glassPlate1mm.id), bound)
        // E2 has no sample, so it binds no version; a dark line binds nothing.
        Assert.Empty(boundVersions [ e2 ])
        Assert.Empty(boundVersions [ e3 ])

    [<Fact>]
    let ``a bound version appears in versionsInUse and BLOCKS removal of that sample`` () =
        let seam = versionsInUseSeam (fun () -> [ e1 ])
        // The seam reports the bound version.
        Assert.Contains(SampleVersionRef (SampleVersionId.firstOf SeedSamples.glassPlate1mm.id), seam.versionsInUse ())
        // A store over the real seam refuses to remove the sample whose version an experiment binds …
        let bound = SampleProxy.createInMemory seam
        match bound.removeSample SeedSamples.glassPlate1mm.id with
        | Error (SampleVersionInUse _) -> ()
        | other -> Assert.Fail($"expected SampleVersionInUse, got %A{other}")
        // … while a store over the empty seam removes it freely (the rule is UNLOCKED only by the binding).
        let free = SampleProxy.createInMemory VersionsInUse.empty
        match free.removeSample SeedSamples.glassPlate1mm.id with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok, got %A{other}")

    [<Fact>]
    let ``editing a USED sample version mints the next version instead of rewriting it`` () =
        let seam = versionsInUseSeam (fun () -> [ e1 ])
        let store = SampleProxy.createInMemory seam
        // Change the bound sample's physics (1 mm plate → the 2 mm plate's structure) under the SAME id.
        let edited = { SeedSamples.glassPlate1mm with structure = SeedSamples.glassPlate2mm.structure }
        match store.saveSample edited with
        | Ok () ->
            // v1 is frozen with the original physics …
            match store.resolveVersion (SampleVersionId.firstOf SeedSamples.glassPlate1mm.id) with
            | Ok (Some v1) -> Assert.Equal<SampleStructure>(SeedSamples.glassPlate1mm.structure, v1.structure)
            | other -> Assert.Fail($"expected the frozen v1, got %A{other}")
            // … and v2 carries the edited physics as the new latest.
            let v2Id = { sampleId = SeedSamples.glassPlate1mm.id; version = VersionNumber.first.next }
            match store.resolveVersion v2Id with
            | Ok (Some v2) -> Assert.Equal<SampleStructure>(SeedSamples.glassPlate2mm.structure, v2.structure)
            | other -> Assert.Fail($"expected a minted v2, got %A{other}")
        | other -> Assert.Fail($"expected Ok on the versioning save, got %A{other}")

    // ============================ step 025: committing captures the live chain ============================

    [<Fact>]
    let ``committing from a scene captures the ordered chain including the detector kind`` () =
        // The bay picks the rotating polarizer to vary and commits the scene setup (E1's chain).
        let c =
            ExperimentCollection.empty
            |> chooseElement (el "lp2") "Linear polarizer #4" (variablesFor LinearPolarizer) CaptureTransmitted
            |> commit e1.setup
        Assert.Equal(1, List.length c.experiments)
        let exp = List.head c.experiments
        // the whole ordered chain is captured, detector included
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; Sample; LinearPolarizer; Detector ],
            exp.setup |> List.map (fun d -> d.kind))
        Assert.Equal(Some (el "lp2"), exp.varied |> Option.map (fun v -> v.elementId))
        Assert.Equal(Some VaryR1, exp.varied |> Option.map (fun v -> v.variable))
        match exp.detectorDescriptorOpt with
        | Some d -> Assert.Equal(BoundByEntryId "det-intensity", d.binding)
        | None -> Assert.Fail "the committed chain lost its detector"
        // the sample binding rides along BY VERSION, so it counts as in-use
        Assert.Contains(SampleVersionRef (SampleVersionId.firstOf SeedSamples.glassPlate1mm.id), boundVersions c.experiments)

    // ============================ the mock proxy seam ============================

    [<Fact>]
    let ``listExperiments returns at least one seed template`` () =
        match proxy.listExperiments () with
        | Ok experiments -> Assert.NotEmpty experiments
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``tryGetExperiment hits a known seed id and misses an unknown one`` () =
        match proxy.tryGetExperiment 1 with
        | Ok (Some e) -> Assert.Equal(1, e.id.value)
        | other -> Assert.Fail($"expected seed 1, got %A{other}")
        match proxy.tryGetExperiment 9999 with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")

    [<Fact>]
    let ``a STUB proxy of the same shape drives the same listing logic`` () =
        let only : Experiment =
            {
                id = ExperimentId 7
                setup = [ desc "stub-el" "Stub" LinearPolarizer (BoundByEntryId "pol-lp") ]
                varied = Some { elementId = el "stub-el"; variable = VaryR1 }
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
                dataFileOpt = None
            }
        let stub : ExperimentProxy =
            {
                listExperiments = fun () -> Ok [ only ]
                tryGetExperiment = fun i -> Ok (if i = 7 then Some only else None)
            }
        match stub.listExperiments () with
        | Ok [ s ] -> Assert.Equal(7, s.id.value)
        | other -> Assert.Fail($"%A{other}")
        match stub.tryGetExperiment 7 with
        | Ok (Some s) -> Assert.Equal(Some (el "stub-el"), s.varied |> Option.map (fun v -> v.elementId))
        | other -> Assert.Fail($"%A{other}")
        match stub.tryGetExperiment 0 with
        | Ok None -> ()
        | other -> Assert.Fail($"%A{other}")

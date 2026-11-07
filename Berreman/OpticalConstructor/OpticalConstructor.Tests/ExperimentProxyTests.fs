namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary       // VersionNumber (spec 0038 Part H)
open OpticalConstructor.Domain.Lifecycle             // VersionRef / SampleVersionRef
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles

/// Experiment capture, collection editing and explicit pins on bound sample versions
/// (spec 0027; spec 0038 Part I; spec 0050 S2.2).
module ExperimentProxyTests =

    let private proxy = Experiments.createInMemoryExperiments ()
    let private el (s : string) : ElementId = Library.ElementId s

    /// A placement/orientation summary for a rest-pose element of the given kind.
    let private placementOf (kind : CatalogueKind) : PlacementSummary =
        PlacementSummary.ofPlacement (ElementPlacement.create kind TablePoint.origin)

    /// A captured setup descriptor (id / label / kind / rest placement / binding).
    let private desc (idStr : string) (label : string) (kind : CatalogueKind) (binding : ElementBinding) : ElementDescriptor =
        { ElementDescriptor.elementId = el idStr; label = ElementLabel label; kind = kind; placement = placementOf kind; binding = binding }

    /// The version-one binding of `BuiltinCatalogue.sample`'s 1 mm glass plate.
    let private glassPlateBinding : ElementBinding =
        BoundByVersion (SampleVersionRef (SampleVersionId.firstOf SampleIds.glassPlate1mmId))

    // A single-source setup and a single-polarizer setup for the interactive-commit tests.
    let private srcSetup : ElementDescriptor list = [ desc "src" "Light source #1" LightSource (BoundByEntryId PresetIds.source600) ]
    let private polSetup : ElementDescriptor list = [ desc "pol" "Linear polarizer #2" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp) ]

    // ============================ variable capability (attached to the kind, as DATA) ============================

    [<Fact>]
    let ``variablesFor is data-driven per kind (source=λ, polarizer=R1, sample=R1+R2, others none)`` () =
        Assert.Equal<VariableParameter list>([ VaryWaveLength ], VariableParameter.allowedFor LightSource)
        Assert.Equal<VariableParameter list>([ VaryR1 ], VariableParameter.allowedFor LinearPolarizer)
        Assert.Equal<VariableParameter list>([ VaryR1 ], VariableParameter.allowedFor CircularPolarizer)
        Assert.Equal<VariableParameter list>([ VaryR1; VaryR2 ], VariableParameter.allowedFor Sample)
        Assert.Empty(VariableParameter.allowedFor Lens)
        Assert.Empty(VariableParameter.allowedFor FlatMirror)
        Assert.Empty(VariableParameter.allowedFor Detector)

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
        Assert.Equal({ min = Propagation.SweptValue 0.0; max = Propagation.SweptValue 360.0; points = Propagation.CurvePointCount 73 }, VariableRange.forVariable VaryR1)
        Assert.Equal({ min = Propagation.SweptValue 0.0; max = Propagation.SweptValue 89.0; points = Propagation.CurvePointCount 91 }, VariableRange.forVariable VaryR2)
        Assert.Equal({ min = Propagation.SweptValue 200.0; max = Propagation.SweptValue 800.0; points = Propagation.CurvePointCount 91 }, VariableRange.forVariable VaryWaveLength)

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
        Assert.Equal(CaptureReflected, MeasurementMode.ofEmission (FlatMirror.defaultEmission))
        Assert.Equal(CaptureBoth, MeasurementMode.ofEmission (Sample.defaultEmission))

    // ============================ the editable collection (choose / commit / edit / remove) ============================

    /// A collection with the source picked (λ variable, both capture allowed) — the common editing start.
    let private withSource () : ExperimentCollection =
        ExperimentCollection.empty
        |> chooseElement (el "src") (ElementLabel "Light source #1") (VariableParameter.allowedFor LightSource) CaptureTransmitted

    [<Fact>]
    let ``chooseElement defaults the variable to the first allowed and the range to its default`` () =
        let c = withSource ()
        Assert.Equal(Some (el "src"), c.draft.elementId)
        Assert.Equal(Some VaryWaveLength, c.draft.variable)
        Assert.Equal(VariableRange.forVariable VaryWaveLength, c.draft.range)
        Assert.True(c.canCommit)

    [<Fact>]
    let ``an element with nothing to vary leaves the variable unset and uncommittable`` () =
        let c = ExperimentCollection.empty |> chooseElement (el "lens") (ElementLabel "Lens #1") (VariableParameter.allowedFor Lens) CaptureTransmitted
        Assert.Equal(None, c.draft.variable)
        Assert.False(c.canCommit)

    [<Fact>]
    let ``chooseVariable resets the range to the new variable's default`` () =
        let c =
            ExperimentCollection.empty
            |> chooseElement (el "s") (ElementLabel "Sample #1") (VariableParameter.allowedFor Sample) CaptureBoth
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
        let c2 = c1 |> chooseMeasurement CaptureReflected |> setRangeMin (Propagation.SweptValue 300.0) |> setRangeMax (Propagation.SweptValue 500.0) |> commit srcSetup
        Assert.Equal(1, List.length c2.experiments)
        let updated = c2.experiments |> List.find (fun e -> e.id = id)
        Assert.Equal(CaptureReflected, updated.measurement)
        Assert.Equal(300.0, updated.range.min.value)
        Assert.Equal(500.0, updated.range.max.value)

    [<Fact>]
    let ``newDraft after add lets a second commit APPEND a distinct experiment`` () =
        let c1 = withSource () |> commit srcSetup
        let c2 =
            c1
            |> (fun c -> c.newDraft)
            |> chooseElement (el "pol") (ElementLabel "Linear polarizer #2") (VariableParameter.allowedFor LinearPolarizer) CaptureBoth
            |> commit polSetup
        Assert.Equal(2, List.length c2.experiments)
        let ids = c2.experiments |> List.map (fun e -> e.id.value)
        Assert.Equal(List.length ids, ids |> List.distinct |> List.length)   // distinct ids

    [<Fact>]
    let ``edit loads an experiment into the draft for re-editing`` () =
        let c1 =
            withSource () |> commit srcSetup
            |> (fun c -> c.newDraft)
            |> chooseElement (el "pol") (ElementLabel "Linear polarizer #2") (VariableParameter.allowedFor LinearPolarizer) CaptureTransmitted
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
        let c = withSource () |> setRangePoints (Propagation.CurvePointCount 1)
        Assert.True(c.draft.range.points.value >= 2)

    // ============================ step 025: the full-setup experiment (E1 / E2 / E3) ============================

    /// E1 = source + polarizer + sample + rotating polarizer + detector (the rotating polarizer varied).
    let private e1 : Experiment =
        {
            id = ExperimentId 1
            setup =
                [
                    desc "src" "Light source #1" LightSource (BoundByEntryId PresetIds.source600)
                    desc "lp1" "Linear polarizer #2" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp)
                    desc "s" "Sample #3" Sample glassPlateBinding
                    desc "lp2" "Linear polarizer #4" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp)
                    desc "det" "Detector #5" Detector (BoundByEntryId PresetIds.detectorIntensity)
                ]
            varied = Some { elementId = el "lp2"; variable = VaryR1 }
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = None
            sourceWaveLengthOpt = None
        }

    /// E2 = the same WITHOUT a sample (captures that the polarizers are not ideal).
    let private e2 : Experiment =
        { e1 with
            id = ExperimentId 2
            setup =
                [
                    desc "src" "Light source #1" LightSource (BoundByEntryId PresetIds.source600)
                    desc "lp1" "Linear polarizer #2" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp)
                    desc "lp2" "Linear polarizer #3" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp)
                    desc "det" "Detector #4" Detector (BoundByEntryId PresetIds.detectorIntensity)
                ] }

    /// E3 = an empty dark line (nothing varied) — how much ambient light reaches the detector with no source.
    let private e3 : Experiment =
        { id = ExperimentId 3; setup = []; varied = None; measurement = CaptureTransmitted; range = VariableRange.forVariable VaryR1; dataFileOpt = None; sourceWaveLengthOpt = None }

    [<Fact>]
    let ``E1 carries the ordered chain incl. the detector, with the varied polarizer identified within it`` () =
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; Sample; LinearPolarizer; Detector ],
            e1.setup |> List.map (fun d -> d.kind))
        // the varied element resolves to a descriptor inside the setup
        Assert.Equal(Some (el "lp2"), e1.varied |> Option.map (fun v -> v.elementId))
        Assert.Equal(ElementLabel "Linear polarizer #4", e1.variedLabel)
        // the detector is part of the chain and carries its kind (via its preset binding)
        match e1.detectorDescriptorOpt with
        | Some d ->
            Assert.Equal(Detector, d.kind)
            Assert.Equal(BoundByEntryId PresetIds.detectorIntensity, d.binding)
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
        Assert.Equal(ElementLabel.blank, e3.variedLabel)
        Assert.Contains("Dark line", e3.description)

    [<Fact>]
    let ``E1 and E2 describe their varied element, variable and capture`` () =
        Assert.Contains("Linear polarizer #4", e1.description)
        Assert.Contains("Rotation R1", e1.description)
        Assert.Contains("Transmitted", e1.description)
        Assert.Contains("Linear polarizer #3", e2.description)

    [<Fact>]
    let ``attachDataFile binds one data attachment per experiment`` () =
        let attachment : DataAttachment =
            {
                path = KitRelativePath "data/run1.csv"
                contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
            }
        Assert.Equal("data/run1.csv", attachment.path.value)
        let c = withSource () |> commit srcSetup
        let id = (List.head c.experiments).id
        Assert.Equal(None, (List.head c.experiments).dataFileOpt)
        let c2 = attachDataFile id attachment c
        let exp = c2.experiments |> List.find (fun e -> e.id = id)
        Assert.Equal(Some attachment, exp.dataFileOpt)

    // ============================ spec 0050 S2.2: version bindings and explicit pins ============================

    [<Fact>]
    let ``versionRefs collects the versioned bindings of the setup (samples yes, presets no)`` () =
        let bound = e1.versionRefs
        Assert.Contains(SampleVersionRef (SampleVersionId.firstOf SampleIds.glassPlate1mmId), bound)
        // E2 has no sample, so it binds no version; a dark line binds nothing.
        Assert.Empty(e2.versionRefs)
        Assert.Empty(e3.versionRefs)

    [<Fact>]
    let ``an experiment references the library versions its setup binds, each once`` () : unit =
        let sampleVersion : LibraryVersionRef =
            {
                entryId = SampleIds.glassPlate1mmId.toLibraryEntryId
                version = VersionNumber.first
            }
        let sampleReference = EntryVersionReference sampleVersion
        let sourceId =
            match PresetIds.source600.tryToLibraryEntryId with
            | Some id -> id
            | None -> failwith "The shipped source must have a canonical GUID identity."
        let sourceReference = EntryLatestReference sourceId
        let sourceBinding = BoundByEntryId PresetIds.source600
        let experiment =
            {
                e3 with
                    setup =
                        [
                            desc "sample" "Sample #1" Sample glassPlateBinding
                            desc "source-one" "Light source #2" LightSource sourceBinding
                            desc "source-two" "Light source #3" LightSource sourceBinding
                            desc "unbound" "Lens #4" Lens Unbound
                        ]
            }
        Assert.Equal<EntryReference list>([ sampleReference; sourceReference ], experiment.libraryReferences)
        Assert.Equal<EntryReference option>(Some sampleReference, glassPlateBinding.entryReferenceOpt)
        Assert.Equal<EntryReference option>(Some sourceReference, sourceBinding.entryReferenceOpt)
        Assert.Equal<EntryReference option>(None, Unbound.entryReferenceOpt)
        Assert.Empty(e3.libraryReferences)

        let nonGuidId = ElementValueId "not-a-library-guid"
        let nonGuidBinding = BoundByEntryId nonGuidId
        Assert.Equal<EntryReference option>(None, nonGuidBinding.entryReferenceOpt)

        let nextSampleVersion : SampleVersionId =
            {
                sampleId = SampleIds.glassPlate1mmId
                version = VersionNumber.first.next
            }
        let nextSampleBinding = BoundByVersion (SampleVersionRef nextSampleVersion)
        let nextSampleReference =
            EntryVersionReference
                {
                    sampleVersion with version = nextSampleVersion.version
                }
        let materialVersion : MaterialVersionId =
            {
                materialId = MaterialIds.glass152
                version = VersionNumber.first.next
            }
        let materialBinding = BoundByVersion (MaterialVersionRef materialVersion)
        let materialReference =
            EntryVersionReference
                {
                    entryId = MaterialIds.glass152.toLibraryEntryId
                    version = VersionNumber.first.next
                }
        let latestSampleId = SampleIds.glassPlate1mmId.toElementValueId
        let latestSampleBinding = BoundByEntryId latestSampleId
        let latestSampleReference = EntryLatestReference sampleVersion.entryId
        let expanded =
            {
                experiment with
                    setup =
                        experiment.setup @
                            [
                                desc "sample-again" "Sample #5" Sample glassPlateBinding
                                desc "sample-next" "Sample #6" Sample nextSampleBinding
                                desc "material" "Sample #7" Sample materialBinding
                                desc "material-again" "Sample #8" Sample materialBinding
                                desc "sample-latest" "Sample #9" Sample latestSampleBinding
                                desc "non-guid" "Light source #10" LightSource nonGuidBinding
                            ]
            }
        Assert.Equal<EntryReference list>(
            [ sampleReference; sourceReference; nextSampleReference; materialReference; latestSampleReference ],
            expanded.libraryReferences)
        let reversed =
            {
                expanded with setup = List.rev expanded.setup
            }
        Assert.Equal<EntryReference list>(
            [ latestSampleReference; materialReference; nextSampleReference; sampleReference; sourceReference ],
            reversed.libraryReferences)

    [<Fact>]
    let ``pinning an experiment's bound sample version BLOCKS removal of that sample`` () =
        let bound = BuiltinCatalogue.sampleStore ()
        for reference in e1.versionRefs do
            match reference with
            | SampleVersionRef version -> Assert.Equal<Result<unit, SampleError>>(Ok (), bound.pinSampleVersion version)
            | MaterialVersionRef _ -> ()
        match bound.removeSample SampleIds.glassPlate1mmId with
        | Error (SampleVersionPinnedErr _) -> ()
        | other -> Assert.Fail($"expected SampleVersionPinnedErr, got %A{other}")
        let free = BuiltinCatalogue.sampleStore ()
        match free.removeSample SampleIds.glassPlate1mmId with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok, got %A{other}")

    [<Fact>]
    let ``editing a PINNED sample version mints the next version instead of rewriting it`` () =
        let store = BuiltinCatalogue.sampleStore ()
        for reference in e1.versionRefs do
            match reference with
            | SampleVersionRef version -> Assert.Equal<Result<unit, SampleError>>(Ok (), store.pinSampleVersion version)
            | MaterialVersionRef _ -> ()
        // Change the bound sample's physics (1 mm plate → the 2 mm plate's structure) under the SAME id.
        let edited =
            {
                BuiltinCatalogue.sample SampleIds.glassPlate1mmId with
                    structure = (BuiltinCatalogue.sample SampleIds.glassPlate2mmId).structure
            }
        match store.saveSample edited with
        | Ok () ->
            // v1 is frozen with the original physics …
            match store.resolveVersion (SampleVersionId.firstOf SampleIds.glassPlate1mmId) with
            | Ok (Some v1) -> Assert.Equal<SampleStructure>((BuiltinCatalogue.sample SampleIds.glassPlate1mmId).structure, v1.structure)
            | other -> Assert.Fail($"expected the frozen v1, got %A{other}")
            // … and v2 carries the edited physics as the new latest.
            let v2Id = { sampleId = SampleIds.glassPlate1mmId; version = VersionNumber.first.next }
            match store.resolveVersion v2Id with
            | Ok (Some v2) -> Assert.Equal<SampleStructure>((BuiltinCatalogue.sample SampleIds.glassPlate2mmId).structure, v2.structure)
            | other -> Assert.Fail($"expected a minted v2, got %A{other}")
        | other -> Assert.Fail($"expected Ok on the versioning save, got %A{other}")

    // ============================ step 025: committing captures the live chain ============================

    [<Fact>]
    let ``committing from a scene captures the ordered chain including the detector kind`` () =
        // The bay picks the rotating polarizer to vary and commits the scene setup (E1's chain).
        let c =
            ExperimentCollection.empty
            |> chooseElement (el "lp2") (ElementLabel "Linear polarizer #4") (VariableParameter.allowedFor LinearPolarizer) CaptureTransmitted
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
        | Some d -> Assert.Equal(BoundByEntryId PresetIds.detectorIntensity, d.binding)
        | None -> Assert.Fail "the committed chain lost its detector"
        Assert.Contains(SampleVersionRef (SampleVersionId.firstOf SampleIds.glassPlate1mmId), exp.versionRefs)

    // ============================ the mock proxy seam ============================

    [<Fact>]
    let ``the in-memory experiments proxy lists no experiment`` () : unit =
        Assert.Equal<Result<Experiment list, ExperimentError>>(Ok [], proxy.listExperiments ())

    [<Fact>]
    let ``the in-memory experiments proxy resolves no experiment id`` () : unit =
        let firstId = ExperimentId 1
        let unknownId = ExperimentId 9999
        Assert.Equal<Result<Experiment option, ExperimentError>>(Ok None, proxy.tryGetExperiment firstId)
        Assert.Equal<Result<Experiment option, ExperimentError>>(Ok None, proxy.tryGetExperiment unknownId)

    [<Fact>]
    let ``a STUB proxy of the same shape drives the same listing logic`` () =
        let only : Experiment =
            {
                id = ExperimentId 7
                setup = [ desc "stub-el" "Stub" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp) ]
                varied = Some { elementId = el "stub-el"; variable = VaryR1 }
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
                dataFileOpt = None
                sourceWaveLengthOpt = None
            }
        let stub : ExperimentProxy =
            {
                listExperiments = fun () -> Ok [ only ]
                tryGetExperiment = fun i -> Ok (if i = ExperimentId 7 then Some only else None)
            }
        match stub.listExperiments () with
        | Ok [ s ] -> Assert.Equal(7, s.id.value)
        | other -> Assert.Fail($"%A{other}")
        match stub.tryGetExperiment (ExperimentId 7) with
        | Ok (Some s) -> Assert.Equal(Some (el "stub-el"), s.varied |> Option.map (fun v -> v.elementId))
        | other -> Assert.Fail($"%A{other}")
        match stub.tryGetExperiment (ExperimentId 0) with
        | Ok None -> ()
        | other -> Assert.Fail($"%A{other}")

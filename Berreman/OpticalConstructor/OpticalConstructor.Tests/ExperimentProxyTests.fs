namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments

/// Spec 0027 (028) — pure domain tests for the redesigned Experiments: the element-constrained variable
/// set (`variablesFor`, DATA per kind), the T/R/both capture (`MeasurementMode`, defaulted from emission),
/// the variable ranges, and the editable `ExperimentCollection` (choose → commit adds; re-commit updates
/// in place, never duplicates; edit loads; remove deletes). Plus the mock `ExperimentProxy` seam.
module ExperimentProxyTests =

    let private proxy = Experiments.createInMemory ()
    let private el (s : string) : ElementId = Library.elementId s

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
        let c1 = commit c0
        Assert.Equal(1, List.length c1.experiments)
        let added = List.head c1.experiments
        Assert.Equal(el "src", added.elementId)
        Assert.Equal(VaryWaveLength, added.variable)
        // The draft now edits the added experiment, so committing again UPDATES rather than appends.
        let c2 = commit c1
        Assert.Equal(1, List.length c2.experiments)

    [<Fact>]
    let ``editing the draft after add updates the stored experiment in place`` () =
        let c1 = withSource () |> commit
        let id = (List.head c1.experiments).id
        let c2 = c1 |> chooseMeasurement CaptureReflected |> setRangeMin 300.0 |> setRangeMax 500.0 |> commit
        Assert.Equal(1, List.length c2.experiments)
        let updated = c2.experiments |> List.find (fun e -> e.id = id)
        Assert.Equal(CaptureReflected, updated.measurement)
        Assert.Equal(300.0, updated.range.min)
        Assert.Equal(500.0, updated.range.max)

    [<Fact>]
    let ``newDraft after add lets a second commit APPEND a distinct experiment`` () =
        let c1 = withSource () |> commit
        let c2 =
            c1
            |> newDraft
            |> chooseElement (el "pol") "Linear polarizer #2" (variablesFor LinearPolarizer) CaptureBoth
            |> commit
        Assert.Equal(2, List.length c2.experiments)
        let ids = c2.experiments |> List.map (fun e -> e.id.value)
        Assert.Equal(List.length ids, ids |> List.distinct |> List.length)   // distinct ids

    [<Fact>]
    let ``edit loads an experiment into the draft for re-editing`` () =
        let c1 =
            withSource () |> commit
            |> newDraft
            |> chooseElement (el "pol") "Linear polarizer #2" (variablesFor LinearPolarizer) CaptureTransmitted
            |> commit
        let target = c1.experiments |> List.find (fun e -> e.elementId = el "pol")
        let c2 = edit target.id c1
        Assert.Equal(Some target.id, c2.draft.editingId)
        Assert.Equal(Some (el "pol"), c2.draft.elementId)
        Assert.Equal(Some VaryR1, c2.draft.variable)

    [<Fact>]
    let ``remove deletes the experiment and clears the draft when it was being edited`` () =
        let c1 = withSource () |> commit
        let id = (List.head c1.experiments).id
        let c2 = remove id c1
        Assert.Empty(c2.experiments)
        // It was the one being edited, so the draft resets (editingId cleared).
        Assert.Equal(None, c2.draft.editingId)
        Assert.Equal(None, c2.draft.elementId)

    [<Fact>]
    let ``an experiment description mentions the element, the variable, and the capture`` () =
        let c1 = withSource () |> chooseMeasurement CaptureBoth |> commit
        let exp = List.head c1.experiments
        Assert.Contains("Light source", exp.description)
        Assert.Contains("Wavelength", exp.description)
        Assert.Contains("Both", exp.description)

    [<Fact>]
    let ``setRangePoints keeps at least two points`` () =
        let c = withSource () |> setRangePoints 1
        Assert.True(c.draft.range.points >= 2)

    // ============================ the mock proxy seam ============================

    [<Fact>]
    let ``listExperiments returns at least one seed template`` () =
        match proxy.listExperiments () with
        | Ok experiments -> Assert.NotEmpty experiments
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``tryGetExperiment hits a known seed id and misses an unknown one`` () =
        match proxy.tryGetExperiment 1 with
        | Ok (Some e) -> Assert.Equal(1, e.id.value)
        | other -> Assert.Fail(sprintf "expected seed 1, got %A" other)
        match proxy.tryGetExperiment 9999 with
        | Ok None -> ()
        | other -> Assert.Fail(sprintf "expected Ok None, got %A" other)

    [<Fact>]
    let ``a STUB proxy of the same shape drives the same listing logic`` () =
        let only : Experiment =
            {
                id = ExperimentId 7
                elementId = el "stub-el"
                elementLabel = "Stub"
                variable = VaryR1
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
            }
        let stub : ExperimentProxy =
            {
                listExperiments = fun () -> Ok [ only ]
                tryGetExperiment = fun i -> Ok (if i = 7 then Some only else None)
            }
        match stub.listExperiments () with
        | Ok [ s ] -> Assert.Equal(7, s.id.value)
        | other -> Assert.Fail(sprintf "%A" other)
        match stub.tryGetExperiment 7 with
        | Ok (Some s) -> Assert.Equal(el "stub-el", s.elementId)
        | other -> Assert.Fail(sprintf "%A" other)
        match stub.tryGetExperiment 0 with
        | Ok None -> ()
        | other -> Assert.Fail(sprintf "%A" other)

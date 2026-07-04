namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.TableAndElementRotationView

/// Spec 0027 (028) — the redesigned Experiments bay: the multi-step, editable experiment builder (choose
/// element → element-constrained variable → T/R/both capture → range → Add) and the persistent collection
/// (edit / remove). Covers the pure control contract, the host's `experimentState` / `experimentResult`
/// branches, the MVU bindings, and a few headless render proofs.
module ExperimentControlsTests =

    let private elem (i : int) (m : Model) : TestElement = List.item i m.elements

    /// Select the element at index `i`, then bind it to a Library entry id (the `BindValueId` MVU path).
    let private bind (i : int) (entryId : string) (m : Model) : Model =
        { m with selection = ElementSelected i } |> update (BindValueId entryId)

    /// The live id of the element at index `i`.
    let private idOf (i : int) (m : Model) : string = (elem i m).id.value

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty Experiments state is disabled with no candidates and no choice`` () =
        let s = ExperimentControls.empty
        Assert.False(s.enabled)
        Assert.Empty(s.candidates)
        Assert.Equal(None, s.chosenId)
        Assert.False(s.canAdd)
        Assert.Empty(s.collection)

    [<Fact>]
    let ``the Experiments UiIds prefix ids and are stable`` () =
        Assert.Equal("ExperimentCandidate_src", ExperimentControls.UiIds.candidate "src")
        Assert.Equal("ExperimentVariable_r1", ExperimentControls.UiIds.variable "r1")
        Assert.Equal("ExperimentMeasurement_both", ExperimentControls.UiIds.measurement "both")
        Assert.Equal("ExperimentReadout", ExperimentControls.UiIds.readout)
        Assert.Equal("ExperimentAddButton", ExperimentControls.UiIds.addButton)
        Assert.Equal("ExperimentEdit_3", ExperimentControls.UiIds.editButton "3")
        Assert.Equal("ExperimentRemove_3", ExperimentControls.UiIds.removeButton "3")

    [<Fact>]
    let ``the variable / measurement codes and labels are the mirror mapping`` () =
        Assert.Equal("wavelength", ExperimentControls.variableCode ExperimentControls.VaryWaveLength)
        Assert.Equal("r1", ExperimentControls.variableCode ExperimentControls.VaryR1)
        Assert.Equal("r2", ExperimentControls.variableCode ExperimentControls.VaryR2)
        Assert.Equal("t", ExperimentControls.measurementCode ExperimentControls.CaptureT)
        Assert.Equal("r", ExperimentControls.measurementCode ExperimentControls.CaptureR)
        Assert.Equal("both", ExperimentControls.measurementCode ExperimentControls.CaptureBoth)

    [<Fact>]
    let ``the ribbon still offers seven bays including Experiments and Details`` () =
        Assert.Equal<string list>(
            [ BayNames.rotation; BayNames.move; BayNames.add; BayNames.render; BayNames.library; BayNames.experiments; BayNames.details ],
            BayNames.all)
        let m = initMain ()
        let bays = mainBays m ignore
        Assert.Equal<string list>(BayNames.all, bays |> List.map (fun b -> b.name))

    // ============================ host MVU: choose / vary / capture / range ============================

    [<Fact>]
    let ``choosing the source constrains the variable to wavelength (data-driven)`` () =
        let m = initMain () |> update (ExpChooseElement "src")
        Assert.Equal(Some (Library.elementId "src"), m.experimentCollection.draft.elementId)
        Assert.Equal(Some Experiments.VaryWaveLength, m.experimentCollection.draft.variable)

    [<Fact>]
    let ``choosing a sample allows R1 and R2 and can then switch variable`` () =
        let m = initMain () |> update (AddElement Sample)
        let sid = idOf 2 m
        let m1 = m |> update (ExpChooseElement sid)
        // The sample bay state offers exactly the sample's allowed variables (R1, R2).
        let st = experimentState m1
        Assert.Equal<ExperimentControls.VariableChoice list>([ ExperimentControls.VaryR1; ExperimentControls.VaryR2 ], st.variableChoices)
        Assert.Equal(Some Experiments.VaryR1, m1.experimentCollection.draft.variable)
        let m2 = m1 |> update (ExpChooseVariable ExperimentControls.VaryR2)
        Assert.Equal(Some Experiments.VaryR2, m2.experimentCollection.draft.variable)

    [<Fact>]
    let ``choosing a mirror defaults the capture to reflected (not hard-coded)`` () =
        let m = initMain () |> update (AddElement FlatMirror)
        let mid = idOf 2 m
        let m1 = m |> update (ExpChooseElement mid)
        Assert.Equal(Experiments.CaptureReflected, m1.experimentCollection.draft.measurement)

    [<Fact>]
    let ``the measurement and range round-trip through the model`` () =
        let m =
            initMain ()
            |> update (ExpChooseElement "src")
            |> update (ExpChooseMeasurement ExperimentControls.CaptureBoth)
            |> update (ExpSetRangeMin 250.0)
            |> update (ExpSetRangeMax 650.0)
            |> update (ExpSetRangePoints 51)
        let d = m.experimentCollection.draft
        Assert.Equal(Experiments.CaptureBoth, d.measurement)
        Assert.Equal(250.0, d.range.min)
        Assert.Equal(650.0, d.range.max)
        Assert.Equal(51, d.range.points)

    // ============================ host MVU: Add / edit / remove collection ============================

    [<Fact>]
    let ``Add appends to the collection and re-Add does not duplicate`` () =
        let m = initMain () |> update (ExpChooseElement "src") |> update ExpCommit
        Assert.Equal(1, List.length m.experimentCollection.experiments)
        let m2 = m |> update ExpCommit
        Assert.Equal(1, List.length m2.experimentCollection.experiments)

    [<Fact>]
    let ``New then Add appends a distinct second experiment`` () =
        let m =
            initMain ()
            |> update (ExpChooseElement "src") |> update ExpCommit
            |> update ExpNew
            |> update (ExpChooseElement "det")           // detector has nothing to vary → cannot commit
        // The detector exposes nothing to vary, so a second Add is inert.
        let m2 = m |> update ExpCommit
        Assert.Equal(1, List.length m2.experimentCollection.experiments)
        // Choose a varyable element instead and Add — now there are two.
        let m3 = m |> update (AddElement Sample)
        let sid = idOf 2 m3
        let m4 = m3 |> update (ExpChooseElement sid) |> update ExpCommit
        Assert.Equal(2, List.length m4.experimentCollection.experiments)

    [<Fact>]
    let ``Edit loads an experiment and Remove deletes it`` () =
        let m = initMain () |> update (ExpChooseElement "src") |> update ExpCommit
        let exp = List.head m.experimentCollection.experiments
        let idStr = string exp.id.value
        let mEdit = m |> update (ExpEdit idStr)
        Assert.Equal(Some exp.id, mEdit.experimentCollection.draft.editingId)
        let mRemove = m |> update (ExpRemove idStr)
        Assert.Empty(mRemove.experimentCollection.experiments)

    [<Fact>]
    let ``the bay state exposes the collection rows and the editing highlight`` () =
        let m = initMain () |> update (ExpChooseElement "src") |> update ExpCommit
        let st = experimentState m
        Assert.Single(st.collection) |> ignore
        let row = List.head st.collection
        Assert.Contains("Light source", row.description)
        Assert.True(row.isEditing, "the just-added experiment should be the one being edited")
        Assert.True(st.isEditing)

    // ============================ host experimentResult branches ============================

    /// initMain + a LinearPolarizer at index 2, chosen to vary (default VaryR1).
    let private withChosenPolarizer () : Model =
        let m = initMain () |> update (AddElement LinearPolarizer)
        m |> update (ExpChooseElement (idOf 2 m))

    /// initMain + a bound Sample at index 2, chosen, on the given variable, capture T.
    let private withChosenSample (v : ExperimentControls.VariableChoice) : Model =
        let m = initMain () |> update (AddElement Sample) |> bind 2 "sample-glass-film-200"
        m
        |> update (ExpChooseElement (idOf 2 m))
        |> update (ExpChooseVariable v)
        |> update (ExpChooseMeasurement ExperimentControls.CaptureT)

    [<Fact>]
    let ``with no element chosen the chart is empty`` () =
        Assert.Empty((experimentResult (initMain ())).series)

    [<Fact>]
    let ``varying a polarizer R1 with an intensity detector yields an intensity series`` () =
        let chart = experimentResult (withChosenPolarizer ())
        Assert.Single(chart.series) |> ignore
        Assert.NotEmpty((List.head chart.series).points)
        Assert.Equal("Rotation R1 (°)", chart.xLabel)
        Assert.Equal("Intensity (S₀)", chart.yLabel)

    [<Fact>]
    let ``a VaryR2 intensity experiment yields one incidence series with x running to 89`` () =
        let chart = experimentResult (withChosenSample ExperimentControls.VaryR2)
        Assert.Single(chart.series) |> ignore
        let xs = (List.head chart.series).points |> List.map fst
        Assert.NotEmpty(xs)
        Assert.Equal("Incidence angle R2 (°)", chart.xLabel)
        Assert.True(abs (89.0 - List.last xs) < 1e-6, sprintf "last x = %g, expected 89" (List.last xs))
        Assert.True((xs = List.sort xs), "incidence x-values must be sorted ascending")

    [<Fact>]
    let ``a VaryWaveLength intensity experiment spans the chosen wavelength range`` () =
        let m =
            withChosenSample ExperimentControls.VaryWaveLength
            |> update (ExpSetRangeMin 300.0)
            |> update (ExpSetRangeMax 700.0)
        let chart = experimentResult m
        Assert.Single(chart.series) |> ignore
        let xs = (List.head chart.series).points |> List.map fst
        Assert.True(abs (300.0 - List.head xs) < 1e-6)
        Assert.True(abs (700.0 - List.last xs) < 1e-6)
        Assert.Equal("Wavelength (nm)", chart.xLabel)

    [<Fact>]
    let ``capturing BOTH branches yields two series (T and R)`` () =
        // A bound sample, VaryR2, capture Both ⇒ a transmitted AND a reflected intensity series.
        let m =
            initMain () |> update (AddElement Sample) |> bind 2 "sample-glass-film-200"
            |> (fun m -> m |> update (ExpChooseElement (idOf 2 m)))
            |> update (ExpChooseVariable ExperimentControls.VaryR2)
            |> update (ExpChooseMeasurement ExperimentControls.CaptureBoth)
        let chart = experimentResult m
        Assert.Equal(2, List.length chart.series)
        Assert.Equal<string list>([ "Intensity (T)"; "Intensity (R)" ], chart.series |> List.map (fun s -> s.name))

    [<Fact>]
    let ``an ellipsometer VaryR2 experiment yields two series (Psi and Delta)`` () =
        let m =
            initMain () |> update (AddElement Sample) |> bind 2 "sample-glass-film-200" |> bind 1 "det-ellipsometer"
            |> (fun m -> m |> update (ExpChooseElement (idOf 2 m)))
            |> update (ExpChooseVariable ExperimentControls.VaryR2)
            |> update (ExpChooseMeasurement ExperimentControls.CaptureT)
        let chart = experimentResult m
        Assert.Equal<string list>([ "Ψ"; "Δ" ], chart.series |> List.map (fun s -> s.name))

    [<Fact>]
    let ``a VaryR2 experiment with no bound sample yields an empty chart`` () =
        // A polarizer varied on R2 is impossible (polarizers only vary R1); use a sample element but do NOT
        // bind it — an R2 vary then has no sample to re-solve.
        let m = initMain () |> update (AddElement Sample)
        let m1 =
            m
            |> (fun m -> m |> update (ExpChooseElement (idOf 2 m)))
            |> update (ExpChooseVariable ExperimentControls.VaryR2)
        Assert.Empty((experimentResult m1).series)

    // ============================ ExperimentChart CSV (unchanged data model) ============================

    [<Fact>]
    let ``ExperimentChart toCsv writes a header and one row per x for two series`` () =
        let chart : ExperimentChart.ExperimentChart =
            { ExperimentChart.empty with
                series =
                    [
                        { ExperimentChart.ChartSeries.name = "Ψ"; points = [ 0.0, 10.0; 1.0, 11.0 ] }
                        { ExperimentChart.ChartSeries.name = "Δ"; points = [ 0.0, 20.0; 1.0, 21.0 ] }
                    ] }
        let lines = (ExperimentChart.toCsv chart).Split('\n')
        Assert.Equal("x,Ψ,Δ", lines.[0])
        Assert.Equal(3, lines.Length)
        Assert.Equal("0,10,20", lines.[1])
        Assert.Equal("1,11,21", lines.[2])

    [<Fact>]
    let ``ExperimentChart toCsv of the empty chart is just the header`` () =
        Assert.Equal("x", ExperimentChart.toCsv ExperimentChart.empty)

    // ============================ ChartWindow ids ============================

    [<Fact>]
    let ``ChartWindow ids are stable and distinct incl the font target + size`` () =
        Assert.Equal("ChartWindowFontTarget", ChartWindowIds.fontTarget)
        Assert.Equal("ChartWindowFontSize", ChartWindowIds.fontSize)
        let ids =
            [ ChartWindowIds.plot; ChartWindowIds.fontMinus; ChartWindowIds.fontPlus; ChartWindowIds.fontTarget
              ChartWindowIds.fontSize; ChartWindowIds.majorGrid; ChartWindowIds.minorGrid; ChartWindowIds.exportPng
              ChartWindowIds.exportCsv; ChartWindowIds.description ]
        Assert.Equal(List.length ids, ids |> List.distinct |> List.length)

    /// A small sample chart with two series, axis labels, a title, and a description.
    let private sampleChart : ExperimentChart.ExperimentChart =
        {
            series =
                [
                    { ExperimentChart.ChartSeries.name = "Ψ"; points = [ 0.0, 10.0; 1.0, 12.0; 2.0, 9.0 ] }
                    { ExperimentChart.ChartSeries.name = "Δ"; points = [ 0.0, 20.0; 1.0, 18.0; 2.0, 22.0 ] }
                ]
            xLabel = "Incidence angle R2 (°)"
            yLabel = "Ψ, Δ (°)"
            title = "Ellipsometric Ψ/Δ vs incidence"
            description = "A two-series ellipsometer sweep used by the pop-out chart-window smoke test."
        }

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the ChartWindow opens and renders with the font target selector and size readout`` () =
        HeadlessSession.run (fun () ->
            let window = ChartWindow(sampleChart)
            try window.Show() with _ -> ()
            try Dispatcher.UIThread.RunJobs() with _ -> ()
            let hasNamed (name : string) : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Control as c -> c.Name = name | _ -> false)
            Assert.True(hasNamed ChartWindowIds.plot, "the ScottPlot host control was not present")
            Assert.True(hasNamed ChartWindowIds.fontTarget, "the font-target selector was not present")
            Assert.True(hasNamed ChartWindowIds.fontSize, "the font-size readout was not present")
            window.Close())

    // ============================ headless render proofs (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Experiments bay shows the intensity polyline for a varied polarizer`` () =
        HeadlessSession.run (fun () ->
            let baseModel = initMain () |> update (AddElement LinearPolarizer)
            let pid = idOf 2 baseModel
            let mutable model =
                baseModel
                |> update (ExpChooseElement pid)
                |> update (SelectBay BayNames.experiments)
            let dispatch (msg : Msg) = model <- update msg model
            let window = Window(Width = 980.0, Height = canvasHeight + 360.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let chartVisible () : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Polyline as p when p.Name = ExperimentControls.UiIds.chart && p.IsEffectivelyVisible -> true | _ -> false)
            Assert.True(chartVisible (), "the intensity polyline was not visible")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Experiments bay lists candidates and a click picks the element`` () =
        HeadlessSession.run (fun () ->
            let mutable model = initMain () |> update (SelectBay BayNames.experiments)
            let dispatch (msg : Msg) = model <- update msg model
            let window = Window(Width = 980.0, Height = canvasHeight + 300.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let readoutVisible () : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? TextBlock as t when t.Name = ExperimentControls.UiIds.readout && t.IsEffectivelyVisible -> true | _ -> false)
            Assert.True(readoutVisible (), "the Experiment readout was not visible")
            let candName = ExperimentControls.UiIds.candidate "src"
            let findCand () : Border option =
                window.GetVisualDescendants()
                |> Seq.tryPick (function :? Border as b when b.Name = candName && b.IsEffectivelyVisible -> Some b | _ -> None)
            match findCand () with
            | None -> Assert.Fail("the src candidate was not visible in the Experiments bay")
            | Some b ->
                let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                if c.HasValue then
                    window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal(Some "src", model.experimentCollection.draft.elementId |> Option.map (fun id -> id.value))
                else Assert.Fail("the candidate has no on-screen position")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``clicking Add in the Experiments bay adds the experiment to the collection`` () =
        HeadlessSession.run (fun () ->
            let baseModel = initMain () |> update (AddElement LinearPolarizer)
            let pid = idOf 2 baseModel
            let mutable model =
                baseModel
                |> update (ExpChooseElement pid)
                |> update (SelectBay BayNames.experiments)
            let dispatch (msg : Msg) = model <- update msg model
            let window = Window(Width = 980.0, Height = canvasHeight + 360.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let findAdd () : Border option =
                window.GetVisualDescendants()
                |> Seq.tryPick (function :? Border as b when b.Name = ExperimentControls.UiIds.addButton && b.IsEffectivelyVisible -> Some b | _ -> None)
            match findAdd () with
            | None -> Assert.Fail("the Add button was not visible")
            | Some b ->
                let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                if c.HasValue then
                    window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal(1, List.length model.experimentCollection.experiments)
                else Assert.Fail("the Add button has no on-screen position")
            window.Close())

namespace OpticalConstructor.Ui.Tests

open System
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.Geometry
open Berreman.Media
open Berreman.MaterialProperties
open Berreman.Dispersion
open Berreman.FieldFunctions
open OpticalProperties.Standard
open OpticalProperties.Dispersive
open Analytics.Variables
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0046 — the constructor's numbers checked against the Analytics example scripts
/// (`Berreman/Analytics/Examples/*.fsx`, the operator's trusted reference). Each test builds the script's
/// optical system TWICE: once as the script does (an Analytics `FixedInfo` swept by `Variables.calculate` —
/// the data the script plots, read through the same `OpticalFunction`s), and once by DRIVING THE HEADLESS
/// APP: the materials and the sample go into the app's stores, the Main workbench is mounted, the table is
/// built through its messages (bind the source, add a linear polarizer at the script's polarization, add
/// and bind the sample at its incidence, a detector on each branch), the experiment is set up in the
/// Experiments bay, and its Export data button exports the run through the export proxy into an in-memory
/// store. The two must agree point by point.
///
/// Excluded scripts: every `*Wedge*.fsx` and `Work_01.fsx` (operator), `References.fsx` (the loader),
/// `TestChart.fsx` (a chart demo), `TotalRefl_Glass.fsx` (its light arrives from INSIDE glass — the
/// constructor's incident medium is always vacuum), `ActiveCrystal.fsx` (it varies a material's
/// gyration constant, which is not a scene parameter) and `ActiveCrystalComparison.fsx` (it plots the
/// transmitted light's polarization azimuth and ellipticity, which no constructor detector reads — the
/// ellipsometer reads Ψ and Δ). Point counts are reduced from the scripts' 1000–2000
/// so the suite stays quick; the grids still coincide exactly (a script's N intervals are N + 1 points).
module FsxAcceptanceTests =

    // ------------------------------ the bench: the app's stores, fresh per test ------------------------------

    type private Bench =
        {
            materials : MaterialProxy
            samples : SampleProxy
            categories : CategoryProxy
            sources : SourceProxy
            library : LibraryProxy
            export : ExperimentExport.ExperimentExportStore
        }

    let private bench () : Bench =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        let sources = SourceProxy.createInMemory ()
        {
            materials = materials
            samples = samples
            categories = categories
            sources = sources
            library = liveLibrary (Library.createInMemory ()) samples sources
            export = ExperimentExport.createInMemory ()
        }

    let private ok (r : Result<unit, 'Error>) : unit =
        match r with
        | Ok () -> ()
        | Error e -> failwith $"store refused: %A{e}"

    /// Store a material with exactly the script's optical properties.
    let private material (b : Bench) (name : string) (properties : OpticalPropertiesWithDisp) : MaterialId =
        let id = newMaterialId ()
        ok (b.materials.saveMaterial { id = id; name = name; category = CategoryIds.glass; description = None; properties = properties; complexity = None })
        id

    let private layer (id : MaterialId) (thickness : Thickness) : SampleLayer =
        { materialId = MaterialVersionId.firstOf id; thickness = thickness; orientation = PrimaryAxes }

    /// Store a sample (thin films over a lower half-space, or a plate) and return its entry id.
    let private sample (b : Bench) (name : string) (geometry : SubstrateKind) (structure : SampleStructure) : string =
        let s : Sample =
            {
                id = newSampleId ()
                name = name
                structure = structure
                substrate = geometry
                description = name
                supportedEmission = defaultSupportedEmission geometry
            }
        ok (b.samples.saveSample s)
        (SampleItem s).entryId

    /// Store a user light source and return its entry id.
    let private userSource (b : Bench) (id : string) (name : string) (nm : float) (tuning : SourceTuning) : string =
        ok (b.sources.saveSource { id = id; name = name; waveLength = WaveLength.nm (nm * 1.0<nm>); tuning = tuning; protection = UserManaged })
        id

    // ------------------------------ driving the headless app ------------------------------

    /// What the table and the experiment are (the script's light, sample and sweep).
    type private TableRun =
        {
            sourceEntry : string
            polarizationDegrees : float
            sampleEntry : string
            incidenceDegrees : float
            variable : ExperimentControls.VariableChoice
            low : float
            high : float
            points : int
        }

    /// Mount the Main workbench through a re-rendering FuncUI Component (the `LibraryWindowTests` drive
    /// pattern): the window, a dispatch, and the latest model.
    let private mount (model0 : Model) : Window * (Msg -> unit) * (unit -> Model) =
        let latest : Model ref = ref model0
        let dispatchRef : (Msg -> unit) ref = ref ignore
        let comp =
            Avalonia.FuncUI.Component(fun ctx ->
                let state = ctx.useState model0
                let dispatch (msg : Msg) =
                    let m = update msg latest.Value
                    latest.Value <- m
                    state.Set m
                dispatchRef.Value <- dispatch
                mainView state.Current dispatch)
        let window = Window(Width = 1200.0, Height = 1400.0, Content = comp)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window, (fun msg -> dispatchRef.Value msg; Dispatcher.UIThread.RunJobs()), (fun () -> latest.Value)

    let private clickOn (window : Window) (autoId : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (fun v ->
                match v with
                | :? Border as border when AutomationProperties.GetAutomationId border = autoId && border.IsEffectivelyVisible -> Some border
                | _ -> None)
        match found with
        | None -> failwith $"%s{autoId} was not found"
        | Some border ->
            border.BringIntoView() |> ignore
            Dispatcher.UIThread.RunJobs()
            match Option.ofNullable (border.TranslatePoint(Point(border.Bounds.Width / 2.0, border.Bounds.Height / 2.0), window)) with
            | Some c ->
                window.MouseDown(c, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(c, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            | None -> failwith $"%s{autoId} has no on-screen position"

    /// Build the table and the experiment through the app's own messages (its MVU `update` — the same
    /// messages the bays dispatch; rendering every intermediate step would re-run the whole sweep each
    /// time), then mount the Main workbench headless on that model and click its Export data button, which
    /// exports the run through the export proxy into the in-memory store. Capture Both: a detector on the
    /// sample's transmitted beam and one on its reflected ray. Returns the exported run.
    let private runOnTable (b : Bench) (run : TableRun) : ExperimentExport.ExportedRun =
        let apply (msgs : Msg list) (m : Model) : Model = msgs |> List.fold (fun acc msg -> update msg acc) m
        let idAt (i : int) (m : Model) : Library.ElementId = (List.item i m.elements).id
        let table =
            { initMainWith b.library (Experiments.createInMemory ()) b.materials b.samples b.categories with
                sources = b.sources
                experimentExport = b.export.proxy }
            |> apply
                [ BindValueIdTo (elementId "src", run.sourceEntry)
                  BindValueIdTo (elementId "det", SeedEntryIds.detectorIntensity)
                  // The input polarizer (pre-bound to the ideal LP) at the script's polarization angle.
                  AddElement LinearPolarizer
                  RotSetAxis (RotationControls.R1, run.polarizationDegrees)
                  // The sample (index 3), tilted to the script's incidence; bound below.
                  AddElement CatalogueKind.Sample
                  RotSetAxis (RotationControls.R2, run.incidenceDegrees) ]
            |> fun m -> update (BindValueIdTo (idAt 3 m, run.sampleEntry)) m
            // A second detector (index 4): it goes to the first free ray — the sample's reflected ray.
            |> update (AddElement Detector)
            |> fun m -> update (BindValueIdTo (idAt 4 m, SeedEntryIds.detectorIntensity)) m
        let varied =
            match run.variable with
            | ExperimentControls.VaryWaveLength -> "src"
            | ExperimentControls.VaryR1 | ExperimentControls.VaryR2 -> (idAt 3 table).value
        let experiment =
            table
            |> apply
                [ ExpChooseElement varied
                  ExpChooseVariable run.variable
                  ExpSetRangeMin run.low
                  ExpSetRangeMax run.high
                  ExpSetRangePoints run.points
                  ExpChooseMeasurement ExperimentControls.CaptureBoth
                  SelectBay BayNames.experiments ]
        let mutable exported : ExperimentExport.ExportedRun list = []
        HeadlessSession.run (fun () ->
            let window, _, _ = mount experiment
            clickOn window UiIds.Experiment.exportButton
            exported <- b.export.exported ()
            window.Close()
            Dispatcher.UIThread.RunJobs())
        match exported with
        | [ one ] -> one
        | other -> failwith $"expected one exported run, got %d{List.length other}"

    // ------------------------------ the script side ------------------------------

    /// The script's data: `Variables.calculate` over the range, read through `fn` (what the script plots).
    let private scriptData (info : FixedInfo) (range : RangedVariable) (fn : OpticalFunction) : (float * float) list =
        calculate info range
        |> Array.choose (fun (x, solution) -> solution.func fn |> Option.map (fun y -> x, y))
        |> List.ofArray

    let private seriesNamed (run : ExperimentExport.ExportedRun) (name : string) : (float * float) list =
        match run.series |> List.tryFind (fun s -> s.name = name) with
        | Some s -> s.points
        | None -> failwith $"""no series '%s{name}' among %A{run.series |> List.map (fun s -> s.name)}"""

    /// The two curves agree point by point: the same x grid, and y within `tolerance` (absolute).
    let private assertSameCurve (what : string) (tolerance : float) (expected : (float * float) list) (actual : (float * float) list) : unit =
        Assert.Equal(List.length expected, List.length actual)
        List.zip expected actual
        |> List.iter (fun ((xe, ye), (xa, ya)) ->
            Assert.True(abs (xe - xa) < 1.0e-9, $"%s{what}: x differs (%g{xe} vs %g{xa})")
            Assert.True(abs (ye - ya) <= tolerance, $"%s{what} at x = %g{xe}: script %.9f{ye}, constructor %.9f{ya}"))

    /// Compare the exported T and R curves with the script's `T` and `R`.
    let private assertRT (info : FixedInfo) (range : RangedVariable) (exported : ExperimentExport.ExportedRun) : unit =
        let tolerance = 1.0e-6
        assertSameCurve "T" tolerance (scriptData info range T) (seriesNamed exported "Intensity (T)")
        assertSameCurve "R" tolerance (scriptData info range R) (seriesNamed exported "Intensity (R)")

    // ------------------------------ Glass.fsx ------------------------------

    /// Glass.fsx: `opticalProperties refractionIndex` (an isotropic constant-n material).
    let private constantN (n : float) : OpticalPropertiesWithDisp =
        {
            epsWithDisp = (Eps.fromRefractionIndex (RefractionIndex n)).dispersive
            muWithDisp = Mu.vacuum.dispersive
            rhoWithDisp = Rho.vacuum.dispersive
        }

    /// Glass.fsx's `getGlassInfo useThickPlate nh1 None light` — a 1 µm n = 1.78052 film, or the same as a plate.
    let private glassInfo (plate : bool) : FixedInfo =
        let film = { thickness = Thickness.mm 0.001<mm>; propertiesWithDisp = constantN 1.78052 }
        {
            incidentLightInfo = light600nmInclinedDegreeLPs 79.0
            opticalSystem =
                {
                    description = Some "Glass"
                    upperWithDisp = OpticalProperties.vacuum.dispersive
                    filmsWithDisp = (if plate then [] else [ film ])
                    substrateWithDisp = (if plate then Some (PlateWithDisp film) else None)
                    lowerWithDisp = OpticalProperties.vacuum.dispersive
                }
        }

    /// The script's incidence range (0…89° — `IncidenceAngle.maxValue`), 89 intervals ⇒ a point per degree.
    let private incidence0to89 : RangedVariable = Range<_>.create 89 IncidenceAngle.normal IncidenceAngle.maxValue |> IncidenceAngleRange

    let private glassOnTable (plate : bool) : unit =
        let b = bench ()
        let glass = material b "n = 1.78052" (constantN 1.78052)
        let structure : SampleStructure =
            if plate then { films = []; substrate = Some (layer glass (Thickness.mm 0.001<mm>)); lower = None }
            else { films = [ SingleLayer (layer glass (Thickness.mm 0.001<mm>)) ]; substrate = None; lower = None }
        let entry = sample b "Glass.fsx" (if plate then Plate else ThinFilm) structure
        let exported =
            runOnTable b
                {
                    sourceEntry = SeedEntryIds.source600
                    polarizationDegrees = 0.0
                    sampleEntry = entry
                    incidenceDegrees = 0.0
                    variable = ExperimentControls.VaryR2
                    low = 0.0
                    high = 89.0
                    points = 90
                }
        assertRT (glassInfo plate) incidence0to89 exported

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``Glass.fsx — a 1 µm n = 1.78 FILM: R and T against incidence match the script`` () = glassOnTable false

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``Glass.fsx — the same glass as a PLATE: R and T against incidence match the script`` () = glassOnTable true

    // ------------------------------ shared helpers for the remaining scripts ------------------------------

    /// One sweep on the table and the same sweep in the script, compared (R and T).
    let private compare (b : Bench) (info : FixedInfo) (range : RangedVariable) (run : TableRun) : unit =
        assertRT info range (runOnTable b run)

    /// A script `RangedVariable` and the table sweep it corresponds to (N intervals ⇒ N + 1 points).
    let private incidenceSweep (n : int) (topDegrees : float) : RangedVariable * (ExperimentControls.VariableChoice * float * float * int) =
        Range<_>.create n IncidenceAngle.normal (IncidenceAngle.create (Angle.degree topDegrees)) |> IncidenceAngleRange,
        (ExperimentControls.VaryR2, 0.0, topDegrees, n + 1)

    let private waveLengthSweep (n : int) (lowNm : float) (highNm : float) : RangedVariable * (ExperimentControls.VariableChoice * float * float * int) =
        Range<_>.create n (WaveLength.nm (lowNm * 1.0<nm>)) (WaveLength.nm (highNm * 1.0<nm>)) |> WaveLengthRange,
        (ExperimentControls.VaryWaveLength, lowNm, highNm, n + 1)

    let private tableRun (source : string) (sampleEntry : string) (incidence : float) (variable, low, high, points) : TableRun =
        {
            sourceEntry = source
            polarizationDegrees = 0.0      // the scripts' "LPs" light: polarization angle 0
            sampleEntry = sampleEntry
            incidenceDegrees = incidence
            variable = variable
            low = low
            high = high
            points = points
        }

    let private infoOf (light : IncidentLightInfo) (system : OpticalSystemWithDisp) : FixedInfo =
        { incidentLightInfo = light; opticalSystem = system }

    let private filmsSample (b : Bench) (name : string) (films : (MaterialId * Thickness) list) (lower : MaterialId option) : string =
        sample b name ThinFilm
            {
                films = films |> List.map (fun (id, t) -> SingleLayer (layer id t))
                substrate = None
                lower = lower |> Option.map MaterialVersionId.firstOf
            }

    /// The seeded tunable xenon lamp + monochromator (190–1000 nm) — every visible-range wavelength sweep.
    let private xenon : string = "src-xe-mono"

    // ------------------------------ Glass_Dispersive.fsx ------------------------------

    /// Glass_Dispersive.fsx's `getRefractionIndexO` (a two-term Sellmeier, λ in µm) as the script builds it.
    let private dispersiveGlass : OpticalPropertiesWithDisp =
        let getRefractionIndexO (w : WaveLength) =
            let q = w.value / (1.0<meter>) * 1.0e6
            let nSq = 1.0 + 1.43 * q * q / (q * q - 0.073 * 0.073) + 0.65 * q * q / (q * q - 0.12 * 0.12)
            sqrt nSq |> RefractionIndex
        {
            epsWithDisp = getRefractionIndexO >> Eps.fromRefractionIndex |> EpsWithDisp
            muWithDisp = Mu.vacuum.dispersive
            rhoWithDisp = Rho.vacuum.dispersive
        }

    let private dispersiveGlassOnTable (plate : bool) : unit =
        let b = bench ()
        let glass = material b "Sellmeier glass" dispersiveGlass
        let thickness = Thickness.mm 0.001<mm>
        let entry =
            if plate then sample b "Glass_Dispersive.fsx plate" Plate { films = []; substrate = Some (layer glass thickness); lower = None }
            else filmsSample b "Glass_Dispersive.fsx film" [ glass, thickness ] None
        let film = { thickness = thickness; propertiesWithDisp = dispersiveGlass }
        let system : OpticalSystemWithDisp =
            {
                description = Some "Glass"
                upperWithDisp = OpticalProperties.vacuum.dispersive
                filmsWithDisp = (if plate then [] else [ film ])
                substrateWithDisp = (if plate then Some (PlateWithDisp film) else None)
                lowerWithDisp = OpticalProperties.vacuum.dispersive
            }
        let range, sweep = waveLengthSweep 40 300.0 700.0
        compare b (infoOf (light600nmInclinedDegreeLPs 79.0) system) range (tableRun xenon entry 79.0 sweep)

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``Glass_Dispersive.fsx — a Sellmeier glass FILM at 79°: R and T against wavelength match the script`` () = dispersiveGlassOnTable false

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``Glass_Dispersive.fsx — the same glass as a PLATE at 79°: R and T against wavelength match the script`` () = dispersiveGlassOnTable true

    // ------------------------------ LangasiteOnSilicon.fsx ------------------------------

    let private langasiteOnSiliconOnTable (incidence : float) : unit =
        let b = bench ()
        let langasite = material b "Langasite" langasiteOpticalProperties
        let silicon = material b "Silicon" siliconOpticalProperties
        let thickness = Thickness.mm 0.01<mm>
        let entry = filmsSample b "LangasiteOnSilicon.fsx film" [ langasite, thickness ] (Some silicon)
        let light = if incidence = 0.0 then light600nmNormalLPs else light600nmInclinedDegreeLPs incidence
        let range, sweep = waveLengthSweep 35 250.0 600.0
        compare b (Analytics.StandardSystems.langasiteFilmOnSilicon thickness light) range (tableRun xenon entry incidence sweep)

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``LangasiteOnSilicon.fsx — a 10 µm langasite film on silicon at normal incidence: R and T against wavelength match the script`` () =
        langasiteOnSiliconOnTable 0.0

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``LangasiteOnSilicon.fsx — the same film at 79°: R and T against wavelength match the script`` () =
        langasiteOnSiliconOnTable 79.0

    // ------------------------------ MultilayerThinFilm.fsx ------------------------------

    /// MultilayerThinFilm.fsx's 41-layer quarter-wave stack: glass (n = 1.52) / vacuum, ending on glass.
    let private quarterWaveStack (b : Bench) : string * OpticalSystemWithDisp =
        let t1 = Thickness.nm ((600.0 / 1.52 / 4.0) * oneNanometer)
        let t2 = Thickness.nm ((600.0 / 1.00 / 4.0) * oneNanometer)
        let glass = material b "Glass n = 1.52" OpticalProperties.transparentGlass.dispersive
        let gap = material b "Vacuum spacer" OpticalProperties.vacuum.dispersive
        let pairs = List.replicate 20 [ (glass, t1); (gap, t2) ] |> List.concat
        let entry = filmsSample b "MultilayerThinFilm.fsx" (pairs @ [ glass, t1 ]) None
        let layers =
            (List.replicate 20 [ { properties = OpticalProperties.transparentGlass; thickness = t1 }; { properties = OpticalProperties.vacuum; thickness = t2 } ] |> List.concat)
            @ [ { properties = OpticalProperties.transparentGlass; thickness = t1 } ]
        let system : BaseOpticalSystem = { description = Some "Multilayer thin film system."; upper = OpticalProperties.vacuum; films = layers; lower = OpticalProperties.vacuum }
        entry, system.fullSystem.dispersive

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``MultilayerThinFilm.fsx — the 41-layer quarter-wave stack: R and T against incidence match the script`` () =
        let b = bench ()
        let entry, system = quarterWaveStack b
        let range, sweep = incidenceSweep 44 88.0
        compare b (infoOf light600nmNormalLPs system) range (tableRun SeedEntryIds.source600 entry 0.0 sweep)

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``MultilayerThinFilm.fsx — the 41-layer quarter-wave stack: R and T against wavelength match the script`` () =
        let b = bench ()
        let entry, system = quarterWaveStack b
        let range, sweep = waveLengthSweep 60 200.0 800.0
        compare b (infoOf light600nmNormalLPs system) range (tableRun xenon entry 0.0 sweep)

    // ------------------------------ MultilayerThinFilm_02.fsx ------------------------------

    /// MultilayerThinFilm_02.fsx: 100 pairs of n = 1.75 / n = 1.50 quarter-wave layers on n = 1.50 glass.
    let private glassPairsStack (b : Bench) : string * OpticalSystemWithDisp =
        let t1 = Thickness.nm ((600.0 / 1.75 / 4.0) * 1.0<nm>)
        let t2 = Thickness.nm ((600.0 / 1.50 / 4.0) * 1.0<nm>)
        let high = material b "Glass n = 1.75" OpticalProperties.transparentGlass175.dispersive
        let low = material b "Glass n = 1.50" OpticalProperties.transparentGlass150.dispersive
        let entry = filmsSample b "MultilayerThinFilm_02.fsx" (List.replicate 100 [ (high, t1); (low, t2) ] |> List.concat) (Some low)
        let p12 =
            [
                { properties = OpticalProperties.transparentGlass175; thickness = t1 }
                { properties = OpticalProperties.transparentGlass150; thickness = t2 }
            ]
        let system : BaseOpticalSystem =
            {
                description = Some "Multilayer thin film system."
                upper = OpticalProperties.vacuum
                films = [ for _ in 1 .. 100 -> p12 ] |> List.concat
                lower = OpticalProperties.transparentGlass150
            }
        entry, system.fullSystem.dispersive

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``MultilayerThinFilm_02.fsx — 100 glass pairs on glass: R and T against incidence match the script`` () =
        let b = bench ()
        let entry, system = glassPairsStack b
        let range, sweep = incidenceSweep 30 87.0
        compare b (infoOf light600nmNormalLPs system) range (tableRun SeedEntryIds.source600 entry 0.0 sweep)

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``MultilayerThinFilm_02.fsx — 100 glass pairs on glass: R and T against wavelength match the script`` () =
        let b = bench ()
        let entry, system = glassPairsStack b
        let range, sweep = waveLengthSweep 40 500.0 700.0
        compare b (infoOf light600nmNormalLPs system) range (tableRun xenon entry 0.0 sweep)

    // ------------------------------ MultilayerThinFilm_EUV.fsx / MultilayerThinFilm_EUV__002.fsx ------------------------------

    /// The EUV scripts: 100 pairs of 2.65 nm layers in vacuum, with the given optical properties.
    let private euvStack (b : Bench) (high : OpticalProperties) (low : OpticalProperties) : string * OpticalSystemWithDisp =
        let t = Thickness.nm (10.6 / 4.0 * 1.0<nm>)
        let highId = material b "EUV layer 1" high.dispersive
        let lowId = material b "EUV layer 2" low.dispersive
        let entry = filmsSample b "EUV multilayer" (List.replicate 100 [ (highId, t); (lowId, t) ] |> List.concat) None
        let system : BaseOpticalSystem =
            {
                description = Some "Multilayer thin film system for EUV"
                upper = OpticalProperties.vacuum
                films = [ { properties = high; thickness = t }; { properties = low; thickness = t } ] |> List.replicate 100 |> List.concat
                lower = OpticalProperties.vacuum
            }
        entry, system.fullSystem.dispersive

    /// MultilayerThinFilm_EUV__002.fsx's own materials: n = 1 − δ + iβ.
    let private euvComplex (delta : float) (beta : float) : OpticalProperties =
        (createComplex (1.0 - delta) beta) |> ComplexRefractionIndex |> Eps.fromComplexRefractionIndex |> OpticalProperties.fromEpsion

    /// The scripts' light is a fixed 10 nm line — a user source on the table.
    let private euvAgainstIncidence (high : OpticalProperties) (low : OpticalProperties) : unit =
        let b = bench ()
        let entry, system = euvStack b high low
        let source = userSource b "src-test-10nm" "10 nm line" 10.0 FixedLine
        let range, sweep = incidenceSweep 30 60.0
        compare b (infoOf light10nmNormalLPs system) range (tableRun source entry 0.0 sweep)

    /// The wavelength sweep runs on the seeded tunable EUV beamline (5–20 nm).
    let private euvAgainstWaveLength (high : OpticalProperties) (low : OpticalProperties) : unit =
        let b = bench ()
        let entry, system = euvStack b high low
        let range, sweep = waveLengthSweep 30 5.0 20.0
        compare b (infoOf light10nmNormalLPs system) range (tableRun "src-euv-beamline" entry 0.0 sweep)

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``MultilayerThinFilm_EUV.fsx — 100 Mo/Si pairs at 10 nm: R and T against incidence match the script`` () =
        euvAgainstIncidence OpticalProperties.euvMolybdenum OpticalProperties.euvSilicon

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``MultilayerThinFilm_EUV.fsx — 100 Mo/Si pairs: R and T against wavelength match the script`` () =
        euvAgainstWaveLength OpticalProperties.euvMolybdenum OpticalProperties.euvSilicon

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``MultilayerThinFilm_EUV__002.fsx — 100 pairs of its own complex-index layers at 10 nm: R and T against incidence match the script`` () =
        euvAgainstIncidence (euvComplex 0.100 0.045) (euvComplex 0.006 0.002)

    [<Fact>]
    [<Trait("Category", "fsx-acceptance")>]
    let ``MultilayerThinFilm_EUV__002.fsx — 100 pairs of its own complex-index layers: R and T against wavelength match the script`` () =
        euvAgainstWaveLength (euvComplex 0.100 0.045) (euvComplex 0.006 0.002)

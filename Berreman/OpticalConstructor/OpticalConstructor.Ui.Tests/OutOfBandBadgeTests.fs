namespace OpticalConstructor.Ui.Tests

open Avalonia
open OpticalConstructor.Controls
open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Xunit
open Berreman.MathNetNumericsMath
open Berreman.MaterialProperties
open Berreman.Dispersion
open Berreman.Media
open Berreman.Constants
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0038 Part I (step 031) — the OUT-OF-BAND dispersion warning badge on a scene element. A bound
/// sample whose (only) material carries dispersion DEFINED over 300–700 nm, run against a 200–800 nm
/// wavelength sweep, MUST raise a warning badge whose hover tooltip names the offending material and
/// both ranges; the same request at a FIXED 600 nm (inside the band) raises none. Proven both purely
/// (the host `outOfBandWarningFor` derivation — exactly one element flags, none when in-band) and
/// headless (the host `outOfBandBadges` overlay renders the badge control and its tooltip text on a
/// canvas the same way the main workbench does).
module OutOfBandBadgeTests =

    /// The first control whose AutomationId begins with `prefix` (the per-index badge id). Guards the
    /// null AutomationId every unmarked control carries.
    let private tryFindByPrefix (window : Window) (prefix : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (fun v ->
            match v with
            | :? Control as c ->
                let aid = Avalonia.Automation.AutomationProperties.GetAutomationId(c)
                if not (isNull aid) && aid.StartsWith(prefix) then Some c else None
            | _ -> None)

    /// A dispersive material complexity DEFINED over 300–700 nm (the per-axis content is irrelevant to
    /// the diagnostic — only the segment interval is read).
    let private dispersiveComplexity : MaterialComplexity =
        let axis = EpsAxisEvaluated (fun _ -> ComplexRefractionIndex (createComplex 1.5 0.0))
        {
            eps =
                EpsWithDispValue
                    (IsotropicDispersive
                        [ { wavelengthInterval = { lower = toWaveLength Nanometer 300.0; upper = toWaveLength Nanometer 700.0 }; dispersion = axis } ])
            magnetic = None
            active = None
        }

    /// A dispersive material entry under the given id.
    let private dispersiveMaterial (matId : MaterialId) : MaterialEntry =
        {
            id = matId
            name = "Dispersive glass"
            category = CategoryIds.glass
            description = None
            properties = dispersiveComplexity.toProperties
            complexity = Some dispersiveComplexity
        }

    /// A single-film sample referencing the dispersive material at its pinned version one.
    let private dispersiveSample (matId : MaterialId) : Sample =
        {
            id = newSampleId ()
            name = "Dispersive film"
            structure =
                {
                    films = [ SingleLayer { materialId = MaterialVersionId.firstOf matId; thickness = Thickness.mm 0.01<mm>; orientation = PrimaryAxes } ]
                    substrate = None
                    lower = None
                }
            substrate = Plate
            description = "A single dispersive film for the out-of-band diagnostic test."
        }

    /// A Main-scene model with a bound-sample element reaching a 300–700 nm dispersive material, whose
    /// Experiments-bay draft sweeps `variable` over 200–800 nm. Returns the model plus the flagged
    /// element's index (it is appended after the seeded source/detector).
    let private flaggedScene (variable : VariableParameter option) : Model * int =
        let matId = newMaterialId ()
        let entry = dispersiveMaterial matId
        let sample = dispersiveSample matId
        let entryId = (SampleItem sample).entryId
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        // Insert the dispersive material at version one so the sample layer's pinned reference resolves.
        match materials.saveMaterial entry with
        | Ok () -> ()
        | Error e -> failwith $"seeding the dispersive material failed: %A{e}"
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        let libStub : LibraryProxy =
            {
                entriesForKind = fun _ -> Ok []
                libraryTrees = fun () -> Ok []
                tryGetEntry = fun id -> Ok (if id = entryId then Some (SampleItem sample) else None)
            }
        let baseModel = initMainWith libStub (Experiments.createInMemory ()) materials samples categories
        let flagged : TestElement =
            {
                id = Library.elementId "sample0"
                placement = { ElementPlacement.create Sample TablePoint.origin with valueId = Some entryId }
                zoom = defaultElementZoom
            }
        let draft = { baseModel.experimentCollection.draft with variable = variable; range = { min = 200.0; max = 800.0; points = 91 } }
        let model =
            { baseModel with
                elements = baseModel.elements @ [ flagged ]
                experimentCollection = { baseModel.experimentCollection with draft = draft } }
        model, List.length baseModel.elements

    /// Whether element `e` raises an out-of-band warning under model `m`.
    let private isFlagged (m : Model) (e : TestElement) : bool =
        match outOfBandWarningFor m e with
        | Some _ -> true
        | None -> false

    // ============================ pure host derivation ============================

    [<Fact>]
    let ``a wavelength sweep past a bound sample's defined dispersion flags exactly that element; a fixed in-band request flags none`` () =
        let model, sampleIndex = flaggedScene (Some VaryWaveLength)
        let flagged = List.item sampleIndex model.elements
        // The bound-sample element flags, naming the material and both ranges.
        match outOfBandWarningFor model flagged with
        | Some text ->
            Assert.Contains("Dispersive glass", text)
            Assert.Contains("300", text)
            Assert.Contains("700", text)
            Assert.Contains("200", text)
            Assert.Contains("800", text)
        | None -> Assert.Fail("the wavelength sweep past the material's defined band must flag")
        // The seeded (unbound) source reaches no material, so it never flags.
        Assert.Equal(None, outOfBandWarningFor model (List.head model.elements))
        // Exactly one element in the whole scene is flagged — the sample.
        Assert.Equal(1, model.elements |> List.filter (isFlagged model) |> List.length)
        // A FIXED 600 nm request (inside 300–700) flags nothing at all.
        let inBand, _ = flaggedScene None
        Assert.Equal(0, inBand.elements |> List.filter (isFlagged inBand) |> List.length)

    // ============================ headless render proof ============================

    [<Fact>]
    let ``the out-of-band warning badge and its tooltip text render headless on the flagged element`` () =
        HeadlessSession.run (fun () ->
            let model, sampleIndex = flaggedScene (Some VaryWaveLength)
            let flagged = List.item sampleIndex model.elements
            let expected =
                match outOfBandWarningFor model flagged with
                | Some text -> text
                | None -> failwith "the scene must be flagged for this test"
            // The host builds the scene's warning badges (exactly one — the flagged sample); render them
            // on a canvas the same way the main workbench overlays them, and prove the control + tooltip.
            let badges = outOfBandBadges model
            Assert.NotEmpty(badges)
            let window = Window(Width = canvasWidth, Height = canvasHeight)
            window.Content <- Component(fun _ -> Canvas.create [ Canvas.children badges ] :> IView)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            match tryFindByPrefix window UiIds.TableAndElementRotation.badgePrefix with
            | Some badge ->
                Assert.Equal(UiIds.TableAndElementRotation.outOfBandBadge sampleIndex, Avalonia.Automation.AutomationProperties.GetAutomationId(badge))
                // Its hover tooltip carries the SAME warning text the Details bay shows.
                match ToolTip.GetTip badge with
                | :? string as tip -> Assert.Equal(expected, tip)
                | other -> Assert.Fail($"the badge tooltip must be the warning text, got %A{other}")
            | None -> Assert.Fail("the flagged element must render an out-of-band warning badge")
            window.Close())

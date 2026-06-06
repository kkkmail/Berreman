/// Headless 3-D system-view test (spec 0024 Part U8 §U8.4, gate `ui-tests`). Trait
/// `Category=ui-tests` (the `ui-tests` gate runs `--filter Category!=ui-smoke`).
///
/// Asserts the Part U8 3-D view (R-4 / AC-U8.4): a multi-element system renders a 2-D
/// orthographic projection of `SystemView3D.placeElements` (the element boxes) and
/// `SystemView3D.beamSegments` (the beam rays read from the already-solved per-node
/// `EmFieldSystem`s) on the slice-002 Canvas host — `Rectangle`s + `Line`s on a
/// `Canvas`, with no GL surface (so the headless platform renders it).
///
/// The probe system mirrors the frozen `SchematicGeometryTests` AC-J12 fixture (a
/// vacuum / 200 nm-glass / glass stack), whose solved branches expose non-degenerate
/// Poynting normals — so `beamSegments` yields drawable rays (a near-fully-transmitting
/// normal-incidence stack can have a degenerate Poynting normal that `beamDirection`
/// drops by design, which this test deliberately avoids).
namespace OpticalConstructor.Ui.Tests

open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.Project
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Ui

module SystemView3DTests =

    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex 1.5)

    let private probeSystem : OpticalSystem =
        {
            description = Some "probe"
            upper = OpticalProperties.vacuum
            films = [ { properties = glass; thickness = Thickness.nm 200.0<nm> } ]
            substrate = None
            lower = glass
        }

    let private probeRoot : BeamNode =
        {
            element = Sample probeSystem
            system = probeSystem
            incident = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)
            children = Map.empty
            defaultUnit = Nanometer
        }

    let private probeProject : OpticalConstructorProject =
        { beamTree = { root = probeRoot }; systems = [ probeSystem ]; sources = []; placements = []; table = OpticalConstructor.Domain.Table.defaultTable }

    let private mount (content : Avalonia.FuncUI.Types.IView) : Window =
        let window = Window()
        window.Content <- Component(fun _ctx -> content)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``the 3-D view renders placed element boxes and beam segments on a Canvas without GL`` () =
        HeadlessSession.run (fun () ->
            // Build a multi-element (multi-node) system by attaching a reflected child, then
            // re-init the page so the whole tree is solved (results carry every node's field).
            let page0 = ConstructionPage.init probeProject "." "probe"
            let leaf = { probeRoot with children = Map.empty }
            let attached = ConstructionPage.update (ConstructionPage.AttachChild ([], Reflected, leaf)) page0
            let multi = ConstructionPage.init attached.project "." "probe"

            let window = mount (SystemView3DView.systemPanel multi)
            let descendants = window.GetVisualDescendants() |> List.ofSeq

            // AC-U8.4: hosted on a Canvas (the slice-002 ChartHosts.canvasHost — no GL).
            let canvases = descendants |> List.filter (fun v -> v :? Canvas) |> List.length
            Assert.True(canvases >= 1, "the 3-D view must host its geometry on a Canvas")

            // placeElements renders one box per placed element — the two-node tree gives >= 2.
            let rects = descendants |> List.filter (fun v -> v :? Rectangle) |> List.length
            Assert.True(rects >= 2, sprintf "expected >= 2 placed-element boxes, got %d" rects)

            // beamSegments renders the reflected/transmitted rays read from the solved fields.
            let lines = descendants |> List.filter (fun v -> v :? Line) |> List.length
            Assert.True(lines >= 1, sprintf "expected >= 1 beam-segment line, got %d" lines)
            window.Close())

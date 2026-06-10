/// Optical-table / drawer / top-down geometry + standardized-control tests (Spec 0026
/// Part C / Part J, slice 004, gate `ui-tests`). Trait `Category=ui-tests` (the
/// `ui-tests` gate runs `--filter Category!=ui-smoke`). Every assertion is over PURE
/// geometry / colour / flavour values — no headless Avalonia render is needed
/// (constraint 0.3), exactly like the slice-001/002 domain proofs. Coverage:
///   * R-1 / R-2  — the plate default size and the top-down view-state defaults + reset.
///   * AC-C4 — the standard drawer returns cylinder geometry; the source shade is
///             darker, the detector near-black with a visible frame; toggling show-box
///             adds the box edges.
///   * AC-C5 — the active-element indicator is ≥ 2 px and ≥ 3:1 contrast vs the plate.
///   * AC-C6 — the show-CR-only toggle defaults to CR-only and redraws on change.
///   * C.5   — the named drawing weights make the chief ray prominent and the reflected
///             group lighter.
///   * AC-J1 — the control flavours come from one module and a new flavour overrides
///             only the distinguishing property.
namespace OpticalConstructor.Ui.Tests

open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Project
open OpticalConstructor.Ui
open OpticalConstructor.Ui.Schematic
open OpticalConstructor.Ui.UserEnvironment

module ConstructorViewTests =

    let private origin : TablePoint = { x = 0.0<meter>; y = 0.0<meter> }
    let private at (x : float) (y : float) : TablePoint = { x = x * 1.0<meter>; y = y * 1.0<meter> }
    let private sampleAt (x : float) (y : float) : ElementPlacement = ElementPlacement.create Sample (at x y)

    let private model (ps : ElementPlacement list) : ConstructorView.Model =
        let proj = { Templates.bandpassFilter () with placements = ps; table = Table.defaultTable }
        ConstructorView.init UserEnvironment.defaults proj

    // --- R-1: the plate default size ----------------------------------------

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``R1 a fresh optical table is 1.2 x 2.0 x 0.10 m`` () =
        let t = Table.defaultTable
        Assert.Equal(1.2<meter>, t.width)
        Assert.Equal(2.0<meter>, t.length)
        Assert.Equal(0.10<meter>, t.thickness)

    // --- R-2: the top-down view-state defaults + reset ----------------------

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``R2 the default view is straight top-down (0,0,0) with the default zoom, and reset returns to it`` () =
        let v = Table.defaultView
        // Straight top-down: all three screen rotations are zero, no pan, default zoom.
        Assert.Equal(0.0, v.r1.value)
        Assert.Equal(0.0, v.r2.value)
        Assert.Equal(0.0, v.r3.value)
        Assert.Equal(0.0, v.panX)
        Assert.Equal(0.0, v.panY)
        Assert.Equal(Table.defaultZoom, v.zoom)
        // Reset from a panned / zoomed / rotated state returns to the top-down default
        // (the reset target is (0,0,0) + default zoom, C.2.5).
        let dirtied = { v with r2 = Angle.degree 40.0; panX = 120.0; panY = -30.0; zoom = 3.0 }
        let reset = Table.resetView dirtied
        Assert.Equal(0.0, reset.r2.value)
        Assert.Equal(0.0, reset.panX)
        Assert.Equal(0.0, reset.panY)
        Assert.Equal(Table.defaultZoom, reset.zoom)

    // --- C.1.3: the grey plate is drawn to scale ----------------------------

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``C1 the grey plate is drawn to scale (length:width ratio preserved)`` () =
        let corners = ConstructorTable.plateCorners Table.defaultTable Table.defaultView
        let xs = corners |> List.map (fun c -> c.dx)
        let ys = corners |> List.map (fun c -> c.dy)
        let lengthPx = List.max xs - List.min xs
        let widthPx = List.max ys - List.min ys
        let physRatio = Table.defaultLength / Table.defaultWidth
        Assert.True(abs (lengthPx / widthPx - physRatio) < 1.0e-9,
                    sprintf "drawn ratio %f vs physical %f" (lengthPx / widthPx) physRatio)

    // --- AC-C4: the standard cylinder drawer --------------------------------

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C4 the standard drawer returns cylinder geometry from placement, rotations, kind and value id`` () =
        let g = Drawer.draw origin Angle.zero (Angle.degree 25.0) Angle.zero Sample None false
        // A non-empty cylinder-body silhouette ...
        Assert.False(List.isEmpty g.frame)
        // ... a positive cap radius ...
        Assert.True(g.capRadius > 0.0<meter>)
        // ... and a real cylinder length (the two cap centres differ).
        Assert.NotEqual<TablePoint>(g.axisStart, g.axisEnd)
        // valueId = None is accepted (the one standard drawer is used regardless of it).
        Assert.Equal(Drawer.shadeFor Sample, g.fill)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C4 the source shade is darker, the detector near-black with a still-visible frame`` () =
        let lumOf (k : CatalogueKind) = ConstructorTable.relativeLuminance (Drawer.shadeFor k).color
        // The source is darker than the default light-grey interior ...
        Assert.True(lumOf LightSource < lumOf Sample, "source should be darker than the default interior")
        // ... and the detector is darker still (almost black).
        Assert.True(lumOf Detector < lumOf LightSource, "detector should be darker than the source")
        let det = Drawer.draw origin Angle.zero Angle.zero Angle.zero Detector None false
        // Near-black fill ...
        Assert.True(ConstructorTable.relativeLuminance det.fill.color < 0.05,
                    "detector fill should be near-black")
        // ... but the frame is still drawn at full opacity, so the detector stays visible
        // and selectable.
        Assert.False(List.isEmpty det.frame)
        Assert.True(det.frameStroke.opacity >= 0.8, "detector frame must stay visible")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C4 show-bounding-box is off by default and toggling it on adds the box edges`` () =
        Assert.False(Drawer.showBoundingBoxDefault)
        let noBox = Drawer.draw origin Angle.zero (Angle.degree 20.0) (Angle.degree 15.0) Sample None false
        let withBox = Drawer.draw origin Angle.zero (Angle.degree 20.0) (Angle.degree 15.0) Sample None true
        // Off: no box edges. On: the twelve projected edges that make R1/R2/R3 visible.
        Assert.Empty noBox.boundingBoxEdges
        Assert.Equal(12, List.length withBox.boundingBoxEdges)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C4 the live constructor view uses the standard drawer with the default cylinder axis along the ray`` () =
        let m = model [ sampleAt 0.0 0.0 ]
        let g = ConstructorView.elementGeometry m (List.head m.project.placements)
        // At (R1,R2,R3) = (0,0,0), N1 is the central-ray direction (+X), so the
        // cylinder axis lies on the ray. This guards against the wrong "standing on
        // the table" interpretation.
        Assert.True(g.axisEnd.x > g.axisStart.x, sprintf "axis must run along +X, got %A -> %A" g.axisStart g.axisEnd)
        Assert.True(abs ((g.axisEnd.y - g.axisStart.y) / 1.0<meter>) < 1.0e-9)
        Assert.Empty(g.visibleCapCenters)
        Assert.Empty(g.boundingBoxEdges)
        let boxed = ConstructorView.elementGeometry { m with showBoundingBox = true } (List.head m.project.placements)
        Assert.Equal(12, List.length boxed.boundingBoxEdges)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C4 a side-on cylinder projects as a rectangle, while an end-on cylinder shows one circular cap`` () =
        let side = Drawer.draw origin Angle.zero Angle.zero Angle.zero Sample None false
        Assert.False(List.isEmpty side.frame)
        Assert.Empty(side.visibleCapCenters)
        let sideXs = side.frame |> List.map (fun p -> p.x)
        let sideYs = side.frame |> List.map (fun p -> p.y)
        Assert.True((List.max sideXs - List.min sideXs) > 0.0<meter>)
        Assert.True((List.max sideYs - List.min sideYs) > 0.0<meter>)

        let endOn = Drawer.draw origin Angle.zero Angle.zero (Angle.degree 90.0) Sample None false
        Assert.Empty(endOn.frame)
        Assert.Single(endOn.visibleCapCenters) |> ignore

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C3 hit-test follows the visible projected drawer rectangle`` () =
        let m = model [ ElementPlacement.create LightSource (at 0.0 0.0) ]
        // The light-source side-on rectangle is long along the CR. This point is inside
        // that visible rectangle but outside the old circular centre-radius hit-test.
        Assert.Equal(ConstructorView.ElementSelected 0, ConstructorView.hitTest (at 0.03 0.0) m)

    // --- AC-C5: the active-element indicator --------------------------------

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C5 the active-element indicator is at least 2 px and at least 3:1 contrast against the table`` () =
        Assert.True(ConstructorTable.activeIndicatorWeightPx >= 2.0,
                    sprintf "indicator weight %f must be >= 2 px" ConstructorTable.activeIndicatorWeightPx)
        let ratio = ConstructorTable.contrastRatio ConstructorTable.activeIndicatorColor ConstructorTable.tablePlateColor
        Assert.True(ratio >= 3.0, sprintf "indicator contrast ratio %f must be >= 3:1" ratio)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C5 the WCAG contrast ratio is 1:1 for equal colours and ~21:1 for black on white`` () =
        let black : SchematicColor = { red = 0uy; green = 0uy; blue = 0uy }
        let white : SchematicColor = { red = 255uy; green = 255uy; blue = 255uy }
        Assert.True(abs (ConstructorTable.contrastRatio white white - 1.0) < 1.0e-9)
        Assert.True(ConstructorTable.contrastRatio black white > 20.9)

    // --- AC-C6: show central ray only / all rays ----------------------------

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C6 the show-CR-only toggle defaults to CR-only and redraws the side rays on change`` () =
        Assert.True(ConstructorTable.showCentralRayOnlyDefault)
        // CR-only draws no side rays; show-all draws the eight side rays. The drawn-ray
        // set is a pure function of the flag, so flipping it redraws the table.
        Assert.Equal(0, ConstructorTable.drawnSideRayCount true)
        Assert.Equal(RayModel.sideRayCount, ConstructorTable.drawnSideRayCount false)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C6 the live constructor ray list flips from central-only to all incident side rays`` () =
        let m = model []
        let crOnly = ConstructorView.drawnRaySegments m
        Assert.Equal(1, List.length crOnly)
        Assert.All(crOnly, fun r -> Assert.True(r.isCentral))
        let all = ConstructorView.update (ConstructorView.SetCentralRayOnly false) m
        let rays = ConstructorView.drawnRaySegments all
        Assert.Equal(1 + RayModel.sideRayCount, List.length rays)
        Assert.Equal(RayModel.sideRayCount, rays |> List.filter (fun r -> not r.isCentral) |> List.length)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``B2 outgoing reflected and transmitted branches are drawn from an emitting element`` () =
        let rays = ConstructorView.drawnRaySegments (model [ sampleAt 0.0 0.0 ])
        Assert.Contains(rays, fun r -> r.group = RayModel.Reflected && r.isCentral)
        Assert.Contains(rays, fun r -> r.group = RayModel.Transmitted && r.isCentral)

    // --- C.5: the named drawing weights -------------------------------------

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``C5 the chief ray is the prominent line and the reflected group reads lighter`` () =
        let crCentral = ConstructorTable.rayStroke RayModel.Incident true
        let crSide = ConstructorTable.rayStroke RayModel.Incident false
        // The central (chief) ray is heavier than the side (marginal) rays.
        Assert.True(crCentral.weightPx > crSide.weightPx)
        Assert.Equal(2.0, crCentral.weightPx)
        Assert.Equal(1.0, crSide.weightPx)
        // The reflected group keeps the same hue but reads lighter than the transmitted
        // group (it carries less energy), so they read apart at a glance.
        let transmitted = ConstructorTable.rayStroke RayModel.Transmitted true
        let reflected = ConstructorTable.rayStroke RayModel.Reflected true
        Assert.Equal<SchematicColor>(transmitted.color, reflected.color)
        Assert.True(reflected.opacity < transmitted.opacity)

    // --- AC-J1: the standardized control flavours ---------------------------

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-J1 control flavours come from one module and a new flavour overrides only the distinguishing property`` () =
        // The primary and destructive flavours differ from the default in their fill ...
        Assert.NotEqual<SchematicColor option>(Controls.defaultButtonFlavor.background, Controls.primaryButtonFlavor.background)
        Assert.NotEqual<SchematicColor option>(Controls.defaultButtonFlavor.background, Controls.destructiveButtonFlavor.background)
        // ... and the destructive flavour overrides ONLY the distinguishing properties:
        // its non-distinguishing `bold` is inherited from the base default (J.1.2).
        Assert.Equal(Controls.defaultButtonFlavor.bold, Controls.destructiveButtonFlavor.bold)
        // Adding a new flavour by overriding only the distinguishing property yields a
        // value equal to the base in every other field (one-touch restyle, J.1.2).
        let newFlavour = { Controls.defaultButtonFlavor with background = Some Controls.accentColor }
        Assert.Equal(Controls.defaultButtonFlavor.foreground, newFlavour.foreground)
        Assert.Equal(Controls.defaultButtonFlavor.bold, newFlavour.bold)
        Assert.NotEqual<SchematicColor option>(Controls.defaultButtonFlavor.background, newFlavour.background)
        // The destructive-gate CTAs are distinct positive/negative colours, defined once
        // here (the single definition ConstructionView.fs:40 now draws from — J.2 / R-9).
        Assert.NotEqual<SchematicColor>(Controls.positiveCtaColor, Controls.negativeCtaColor)

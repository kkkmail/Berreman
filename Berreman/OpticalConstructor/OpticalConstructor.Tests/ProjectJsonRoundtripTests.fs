namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors
open Xunit

/// Canonical-JSON round-trip + validate-on-load over the project aggregate
/// (AC-A2 / AC-A5 / AC-D6). The beam tree round-trips through the JSON-canonical
/// envelope, the document validates against the published schema on load, the
/// per-element `defaultUnit` label survives, and the canonical SI magnitudes
/// (`Thickness`/`WaveLength.value`) are unchanged — no unit conversion on the
/// storage path. (The deeper beam-tree `$def` round-trip AC-B9 is slice 005.)
module ProjectJsonRoundtripTests =

    let private filmThickness : float<meter> = 1.0e-7<meter>

    let private vacuumSystem : OpticalSystem =
        {
            description = Some "test stack"
            upper = OpticalProperties.vacuum
            films = [ { properties = OpticalProperties.vacuum; thickness = Thickness.Thickness filmThickness } ]
            substrate = None
            lower = OpticalProperties.vacuum
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)

    let private node element children defaultUnit : BeamNode =
        {
            element = element
            system = vacuumSystem
            incident = light
            children = children
            defaultUnit = defaultUnit
        }

    /// A small but representative project: a sample with a transmitted detector
    /// child (exercises the `Map<BeamBranch, BeamNode>` and the engine stack).
    let private sampleProject (defaultUnit : UnitOfMeasure) : OpticalConstructorProject =
        let detector = node Detector Map.empty Nanometer
        let root = node (Sample vacuumSystem) (Map.ofList [ BeamBranch.Transmitted, detector ]) defaultUnit
        { beamTree = { root = root }; systems = [ vacuumSystem ]; sources = []; placements = []; table = OpticalConstructor.Domain.Table.defaultTable }

    let private okOr (r : Result<'a, _>) : 'a =
        match r with
        | Ok v -> v
        | Error e -> failwith $"unexpected storage error: {e}"

    let private rootThickness (p : OpticalConstructorProject) : float<meter> =
        match p.beamTree.root.system.films with
        | [ { thickness = Thickness.Thickness t } ] -> t
        | _ -> failwith "unexpected films shape"

    [<Fact>]
    let ``AC-A2 a valid canonical project validates on load`` () =
        let json = sampleProject Nanometer |> ProjectJson.serializeProject |> okOr
        // The canonical artefact is JSON text (never a .binz pickle).
        Assert.StartsWith("{", json.TrimStart())
        Assert.Contains("schemaVersion", json)
        // Validate-on-load admits the document.
        sampleProject Nanometer |> ProjectJson.serializeProject |> okOr
        |> ProjectJson.deserializeProject
        |> okOr
        |> ignore

    [<Fact>]
    let ``AC-D6 round-trip preserves the defaultUnit label and the canonical SI datum`` () =
        let project = sampleProject Micrometer
        let back = project |> ProjectJson.serializeProject |> okOr |> ProjectJson.deserializeProject |> okOr
        // The display-only label survives the round-trip ...
        Assert.Equal(Micrometer, back.beamTree.root.defaultUnit)
        // ... and the stored SI magnitude is byte-identical (no conversion applied).
        Assert.Equal(filmThickness, rootThickness back)

    [<Fact>]
    let ``AC-A5 the canonical SI magnitudes are stored verbatim`` () =
        let project = sampleProject Nanometer
        let back = project |> ProjectJson.serializeProject |> okOr |> ProjectJson.deserializeProject |> okOr
        Assert.Equal(filmThickness, rootThickness back)
        Assert.Equal(light.waveLength.value, back.beamTree.root.incident.waveLength.value)
        // The transmitted detector child survived the Map round-trip.
        Assert.True(back.beamTree.root.children.ContainsKey BeamBranch.Transmitted)

    // --- AC-B9 (slice 005): the beam tree and its constructed systems round-trip
    // through the JSON-canonical project, schema-validated on load, exercising the
    // field-level shapes of the filled beamNode/beamBranch/opticalSystem/layer
    // `$defs` — the mirror Reflected child, a Plate substrate, and a Wedge system.
    let private plateSystem : OpticalSystem =
        { vacuumSystem with substrate = Some (Plate { properties = OpticalProperties.vacuum; thickness = Thickness.Infinity }) }
    let private wedgeAngle = WedgeAngle (Angle.degree 2.0)
    let private wedgeSystem : OpticalSystem =
        { vacuumSystem with substrate = Some (Wedge { layer = { properties = OpticalProperties.vacuum; thickness = Thickness.mm 1.0<mm> }; angle = wedgeAngle }) }

    /// A beam tree exercising every filled `$def` branch: a Sample root over a
    /// Plate-substrate system, a mirror Reflected child, a detector Transmitted
    /// child, plus a Plate and a Wedge entry in `systems`.
    let private richProject : OpticalConstructorProject =
        let mirror = { (node FlatMirror Map.empty Nanometer) with system = plateSystem }
        let detector = node Detector Map.empty Nanometer
        let root = { (node (Sample plateSystem) (Map.ofList [ BeamBranch.Reflected, mirror; BeamBranch.Transmitted, detector ]) Micrometer) with system = plateSystem }
        { beamTree = { root = root }; systems = [ plateSystem; wedgeSystem ]; sources = []; placements = []; table = OpticalConstructor.Domain.Table.defaultTable }

    [<Fact>]
    let ``AC-B9 beam tree and systems round-trip through the schema-validated canonical project`` () =
        let json = richProject |> ProjectJson.serializeProject |> okOr
        // The canonical artefact is JSON text and is NEVER a .binz pickle (Part A constraint 4).
        Assert.StartsWith("{", json.TrimStart())
        Assert.DoesNotContain(".binz", json)
        // Validate-on-load admits the document against the published schema and binds it.
        let back = json |> ProjectJson.deserializeProject |> okOr
        // The Sample root, its Plate substrate, and both branch children survived.
        match back.beamTree.root.element with
        | Sample _ -> ()
        | other -> failwith $"unexpected root element: {other}"
        match back.beamTree.root.system.substrate with
        | Some (Plate _) -> ()
        | other -> failwith $"unexpected root substrate: {other}"
        Assert.True(back.beamTree.root.children.ContainsKey BeamBranch.Reflected)
        Assert.True(back.beamTree.root.children.ContainsKey BeamBranch.Transmitted)
        Assert.Equal(FlatMirror, back.beamTree.root.children.[BeamBranch.Reflected].element)
        // The systems list (Plate + Wedge) round-trips with the wedge angle preserved.
        Assert.Equal(2, List.length back.systems)
        match back.systems.[1].substrate with
        | Some (Wedge w) -> Assert.Equal(wedgeAngle, w.angle)
        | other -> failwith $"unexpected wedge substrate: {other}"

    [<Fact>]
    let ``AC-B9 the filled beamNode $def rejects a malformed incident shape on load`` () =
        // A real serialized document with the incident refractionIndex corrupted to a
        // non-number; the filled beamNode `$def` MUST reject it on validate-on-load.
        let bad =
            (richProject |> ProjectJson.serializeProject |> okOr)
                .Replace("\"refractionIndex\": 1", "\"refractionIndex\": \"not-a-number\"")
        match ProjectJson.deserializeProject bad with
        | Error (SchemaValidationError _) -> ()
        | other -> failwith $"expected SchemaValidationError, got: {other}"

    // --- AC-C1 (slice 004): the optical table plate round-trips through the
    // schema-validated canonical project. A fresh table is 1.2 x 2.0 x 0.10 m; an
    // edited plate size persists and reloads (C.1.1 / C.1.2 / AC-C1).
    [<Fact>]
    let ``AC-C1 a fresh project table is 1.2 x 2.0 x 0.10 m`` () =
        let fresh = OpticalConstructor.Domain.Table.defaultTable
        Assert.Equal(1.2<meter>, fresh.width)
        Assert.Equal(2.0<meter>, fresh.length)
        Assert.Equal(0.10<meter>, fresh.thickness)

    [<Fact>]
    let ``AC-C1 the edited table size round-trips through the schema-validated canonical project`` () =
        // Edit the plate size to a new (exactly-representable) width/length/thickness ...
        let edited = OpticalConstructor.Domain.Table.withSize 1.5<meter> 2.5<meter> 0.125<meter> OpticalConstructor.Domain.Table.defaultTable
        let project = { sampleProject Nanometer with table = edited }
        // ... serialize -> validate-on-load -> bind: the table validates against the
        // opticalTable $def and the edited size survives byte-identically (no conversion).
        let json = project |> ProjectJson.serializeProject |> okOr
        Assert.Contains("\"table\"", json)
        let back = json |> ProjectJson.deserializeProject |> okOr
        Assert.Equal(1.5<meter>, back.table.width)
        Assert.Equal(2.5<meter>, back.table.length)
        Assert.Equal(0.125<meter>, back.table.thickness)
        // The display unit (display metadata only) is preserved through the round-trip.
        Assert.Equal(Millimeter, back.table.displayUnit)

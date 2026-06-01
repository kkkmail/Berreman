namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage
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
        { beamTree = { root = root }; systems = [ vacuumSystem ] }

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

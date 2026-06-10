namespace OpticalConstructor.Tests

open System.IO
open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors
open Xunit

/// `.ocproj` file round-trip + load-time schema rejection (AC-I1 / AC-I2 / AC-I3).
/// `saveProject`->`openProject` reproduces the original project value over its
/// canonical SI fields; a schema-violating document returns `SchemaValidationError`
/// (binding nothing); a `schemaVersion` mismatch is rejected as a validation error
/// with no migration path.
module RoundTripTests =

    let private vacuumSystem : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = [ { properties = OpticalProperties.vacuum; thickness = Thickness.Thickness 2.5e-7<meter> } ]
            substrate = None
            lower = OpticalProperties.vacuum
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 500.0<nm>)

    let private project : OpticalConstructorProject =
        let root =
            {
                element = Sample vacuumSystem
                system = vacuumSystem
                incident = light
                children = Map.empty
                defaultUnit = Nanometer
            }
        { beamTree = { root = root }; systems = [ vacuumSystem ]; sources = []; placements = []; table = OpticalConstructor.Domain.Table.defaultTable }

    let private withTempFile (f : string -> unit) =
        let path = Path.Combine(Path.GetTempPath(), $"oc-roundtrip-{System.Guid.NewGuid():N}.ocproj")
        try f path
        finally if File.Exists path then File.Delete path

    [<Fact>]
    let ``AC-I1 saveProject then openProject reproduces the project value`` () =
        withTempFile (fun path ->
            match ProjectFile.saveProject path project with
            | Error e -> Assert.Fail($"saveProject failed: {e}")
            | Ok () ->
                match ProjectFile.openProject path with
                | Ok back -> Assert.Equal<OpticalConstructorProject>(project, back)
                | Error e -> Assert.Fail($"openProject failed: {e}"))

    [<Fact>]
    let ``AC-I2 a schema-violating document returns SchemaValidationError and binds nothing`` () =
        // `systems` is required to be an array; a string violates the schema.
        let bad = """{ "schemaVersion": "1.0", "beamTree": { "root": {} }, "systems": "not-an-array" }"""
        match ProjectJson.deserializeProject bad with
        | Error (SchemaValidationError msgs) -> Assert.NotEmpty msgs
        | other -> Assert.Fail($"expected SchemaValidationError, got {other}")

    [<Fact>]
    let ``AC-I3 a schemaVersion mismatch is rejected without migration`` () =
        let wrongVersion = """{ "schemaVersion": "2.0", "beamTree": { "root": {} }, "systems": [] }"""
        match ProjectJson.deserializeProject wrongVersion with
        | Error (SchemaValidationError _) -> ()
        | other -> Assert.Fail($"expected SchemaValidationError for a version mismatch, got {other}")

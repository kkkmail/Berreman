namespace OpticalConstructor.Tests

open System
open System.Numerics
open System.Text.Json
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage

/// The numbers an entry file holds survive `CanonicalJson.options` (spec 0050 001 §B: an entry that
/// cannot serialise is a test failure).
module CanonicalJsonTests =

    let private toJson<'Value> (value : 'Value) : string = JsonSerializer.Serialize<'Value>(value, CanonicalJson.options)

    let private ofJson<'Value> (json : string) : 'Value = JsonSerializer.Deserialize<'Value>(json, CanonicalJson.options)

    let private arrayItems (element : JsonElement) : JsonElement list =
        Assert.Equal(JsonValueKind.Array, element.ValueKind)
        [ for item in element.EnumerateArray() -> item ]

    let private numbersOf (element : JsonElement) : float list =
        let items = arrayItems element
        Assert.Equal<JsonValueKind list>(items |> List.map (fun _ -> JsonValueKind.Number), items |> List.map (fun i -> i.ValueKind))
        items |> List.map (fun i -> i.GetDouble())

    let private assertBitIdentical (place : string) (expected : float) (actual : float) : unit =
        let identical = BitConverter.DoubleToInt64Bits expected = BitConverter.DoubleToInt64Bits actual
        Assert.True(identical, $"{place}: expected %.17g{expected}, got %.17g{actual}")

    [<Fact>]
    let ``a complex number survives the shared options as its real and imaginary parts`` () =
        let value = Complex(1.5, -0.25)
        let json = toJson value
        Assert.Equal<Complex>(value, ofJson<Complex> json)
        use doc = JsonDocument.Parse(json)
        Assert.Equal<float list>([ value.Real; value.Imaginary ], numbersOf doc.RootElement)

    [<Fact>]
    let ``a real matrix survives the shared options as its rows`` () =
        let mueller = analyzerMueller IdealLinear (Angle.degree 30.0<deg>)
        let json = toJson mueller
        let readBack = ofJson<MuellerMatrix> json
        use doc = JsonDocument.Parse(json)
        let rows = arrayItems doc.RootElement
        Assert.Equal(4, List.length rows)
        for i, row in List.indexed rows do
            let cells = numbersOf row
            Assert.Equal(4, List.length cells)
            for j, cell in List.indexed cells do
                assertBitIdentical $"element ({i},{j}) read back" (mueller.element i j) (readBack.element i j)
                assertBitIdentical $"row {i} cell {j} of the JSON" (mueller.element i j) cell

    [<Fact>]
    let ``an absorbing built-in material survives the shared options`` () =
        let complexity = (builtInEntries |> List.find (fun e -> e.id = MaterialIds.euvMolybdenum)).complexity
        Assert.Equal<MaterialComplexity>(complexity, toJson complexity |> ofJson<MaterialComplexity>)

namespace OpticalConstructor.Tests

open System
open System.Globalization
open System.Numerics
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.FSharp.Reflection
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Dispersion
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.Errors

/// The numbers and int tags an entry file holds survive `CanonicalJson.options`, and the canonical text and
/// content hash do not depend on key order, whitespace or culture (spec 0050 001 §B, step 28).
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
        let complexity =
            match (BuiltinCatalogue.material MaterialIds.euvMolybdenum).physics with
            | ComplexityPhysics complexity -> complexity
            | CrystalParameterPhysics crystal -> failwith $"expected a complexity physics, got %A{crystal}"
        Assert.Equal<MaterialComplexity>(complexity, toJson complexity |> ofJson<MaterialComplexity>)

    let private okOf (result : Result<'Value, StorageError>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected Ok, got %A{error}"

    /// A tunable source of the test's own: its node nests a union with fields, a record and a wavelength.
    let private tunableSourceAt (setting : float<nm>) : SourcePreset =
        let limits : Berreman.Dispersion.WaveLengthInterval = { lower = WaveLength.nm 600.0<nm>; upper = WaveLength.nm 900.0<nm> }

        {
            id = ElementValueId "e41b7c2d-8a6f-4d39-b0e5-7c9a2f1d6b38"
            name = EntryName "Test tunable source 600–900 nm"
            waveLength = WaveLength.nm setting
            tuning = TunableBetween limits
            protection = UserManaged
        }

    /// A copy of the node with every object's properties in reverse order, at every depth; arrays keep theirs.
    let rec private keysReversed (node : JsonNode) : JsonNode =
        match node with
        | null -> null
        | :? JsonObject as o ->
            let copy = JsonObject()
            for KeyValue (name, child) in Seq.rev o do
                copy.Add(name, keysReversed child)
            copy :> JsonNode
        | :? JsonArray as a ->
            let copy = JsonArray()
            for item in a do
                copy.Add(keysReversed item)
            copy :> JsonNode
        | value -> value.DeepClone()

    /// Every object's keys as written, at every depth, the root's first.
    let rec private objectKeysOf (element : JsonElement) : string list list =
        match element.ValueKind with
        | JsonValueKind.Object ->
            let properties = [ for p in element.EnumerateObject() -> p ]
            (properties |> List.map (fun p -> p.Name)) :: (properties |> List.collect (fun p -> objectKeysOf p.Value))
        | JsonValueKind.Array -> [ for item in element.EnumerateArray() do yield! objectKeysOf item ]
        | _ -> []

    [<Fact>]
    let ``canonical text orders every object's keys and ignores the order they came in`` () =
        let node = nodeOf (tunableSourceAt 750.0<nm>) |> okOf
        let reversed = keysReversed node
        Assert.NotEqual<string>(node.ToJsonString(), reversed.ToJsonString())
        Assert.Equal<string>(node.indentedCanonicalText, reversed.indentedCanonicalText)
        Assert.Equal<string>(node.compactCanonicalText, reversed.compactCanonicalText)

        for text in [ node.indentedCanonicalText; node.compactCanonicalText ] do
            use doc = JsonDocument.Parse text
            let keyLists = objectKeysOf doc.RootElement
            Assert.Equal<string list>([ "id"; "name"; "protection"; "tuning"; "waveLength" ], List.head keyLists)
            Assert.True(List.length keyLists > 1, "the source's node nests objects")

            for keys in keyLists do
                Assert.Equal<string list>(keys |> List.sortWith (fun a b -> String.CompareOrdinal(a, b)), keys)

    let private roundTripDoubles : float list =
        [ 0.1; 1.0 / 3.0; 5e-324; 2.2250738585072014e-308; 1.7976931348623157e308; -0.0; 6.02214076e23; 1e-300 ]

    /// Both canonical renderings of the doubles, written with `culture` as the current culture.
    let private canonicalTextsUnder (culture : CultureInfo) : string list =
        CultureInfo.CurrentCulture <- culture
        let node = nodeOf roundTripDoubles |> okOf
        [ node.indentedCanonicalText; node.compactCanonicalText ]

    [<Fact>]
    let ``every double round-trips bit-exactly through canonical text whatever the culture`` () =
        let original = CultureInfo.CurrentCulture

        try
            let invariantTexts = canonicalTextsUnder CultureInfo.InvariantCulture
            let germanTexts = canonicalTextsUnder (CultureInfo "de-DE")
            Assert.Equal<string list>(invariantTexts, germanTexts)

            for text in germanTexts do
                let readBack = ofJson<float list> text
                Assert.Equal(List.length roundTripDoubles, List.length readBack)

                for i, (expected, actual) in List.indexed (List.zip roundTripDoubles readBack) do
                    assertBitIdentical $"value {i} read back" expected actual

                use doc = JsonDocument.Parse text
                let numbers = [ for item in doc.RootElement.EnumerateArray() -> item.GetRawText() ]
                Assert.Equal("0.1", List.head numbers)
                Assert.All(numbers, fun n -> Assert.Matches(@"^-?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?$", n))
        finally
            CultureInfo.CurrentCulture <- original

    [<Fact>]
    let ``the content hash is the lowercase SHA-256 of the compact canonical text`` () =
        let node = nodeOf (tunableSourceAt 750.0<nm>) |> okOf
        let expected : LibraryEntryFiles.ContentHash = Convert.ToHexStringLower(SHA256.HashData(Encoding.UTF8.GetBytes(node.compactCanonicalText))) |> LibraryEntryFiles.ContentHash
        let hash = contentHashOf (tunableSourceAt 750.0<nm>) |> okOf
        Assert.Equal<LibraryEntryFiles.ContentHash>(expected, hash)
        Assert.Matches("^[0-9a-f]{64}$", hash.value)
        Assert.Equal<LibraryEntryFiles.ContentHash>(hash, contentHashOf (tunableSourceAt 750.0<nm>) |> okOf)
        Assert.NotEqual<LibraryEntryFiles.ContentHash>(hash, contentHashOf (tunableSourceAt 750.5<nm>) |> okOf)

    /// Its text and hash below are the writer's output, captured once in step 27 and frozen, so a change to the
    /// canonical form fails here before it can change a committed catalogue file's hash (spec 0050 S1.5).
    let private frozenCategory : MaterialCategory =
        {
            id = Guid.Parse "b7e2c4a9-5d13-4f68-9a0e-3c1f7d2b8e64" |> CategoryId
            name = CategoryName "Glass — CO₂ µm"
            visibility = SelectableOnCreate
            origin = UserCategory
        }

    [<Fact>]
    let ``the canonical form of a fixed value is frozen`` () =
        let frozenText = "{\n  \"id\": \"b7e2c4a9-5d13-4f68-9a0e-3c1f7d2b8e64\",\n  \"name\": \"Glass — CO₂ µm\",\n  \"origin\": \"UserCategory\",\n  \"visibility\": \"SelectableOnCreate\"\n}"
        let frozenHash = LibraryEntryFiles.ContentHash "74b4c2774e9b111cf97a9d595611a1557aca2e09cec11ffc7fadfd19e7163e90"
        Assert.Equal<string>(frozenText, (nodeOf frozenCategory |> okOf).indentedCanonicalText)
        Assert.Equal<LibraryEntryFiles.ContentHash>(frozenHash, contentHashOf frozenCategory |> okOf)

    [<Fact>]
    let ``a non-finite number is a typed serialization error`` () =
        for value in [ nan; infinity; -infinity ] do
            match nodeOf value with
            | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason, $"%g{value}: the error carries the serializer's message")
            | other -> Assert.Fail($"%g{value}: expected MalformedJsonErr, got %A{other}")

        match contentHashOf nan with
        | Error (MalformedJsonErr _) -> ()
        | other -> Assert.Fail($"the hash of nan: expected MalformedJsonErr, got %A{other}")

    /// Every case of a fieldless union, read off the type, so a case added later is covered too.
    let private everyCaseOf<'Tag> () : 'Tag list =
        [ for case in FSharpType.GetUnionCases typeof<'Tag> -> FSharpValue.MakeUnion(case, [||]) :?> 'Tag ]

    let private assertWrittenAsTag<'Tag when 'Tag : equality> (tagOf : 'Tag -> int) : unit =
        let cases = everyCaseOf<'Tag> ()
        Assert.NotEmpty(cases)

        for case in cases do
            let json = toJson case
            use doc = JsonDocument.Parse json
            Assert.Equal(JsonValueKind.Number, doc.RootElement.ValueKind)
            Assert.Equal(tagOf case, doc.RootElement.GetInt32())
            Assert.Equal<'Tag>(case, ofJson<'Tag> json)

    [<Fact>]
    let ``every closed set with an int tag is written as its tag and read back`` () =
        assertWrittenAsTag<LibraryEntryKind> (fun kind -> kind.value)
        assertWrittenAsTag<TabulatedInterpolation> (fun interpolation -> interpolation.value)
        assertWrittenAsTag<PublishedIndexLaw> (fun law -> law.value)
        assertWrittenAsTag<PublishedGyrationLaw> (fun law -> law.value)
        assertWrittenAsTag<KitStar> (fun star -> star.value)
        assertWrittenAsTag<ExperimentRole> (fun role -> role.value)
        assertWrittenAsTag<DataDelimiter> (fun delimiter -> delimiter.value)
        assertWrittenAsTag<HeaderRowChoice> (fun header -> header.value)
        assertWrittenAsTag<ByteOrderMark> (fun mark -> mark.value)
        assertWrittenAsTag<ScanColumnUnit> (fun columnUnit -> columnUnit.value)
        assertWrittenAsTag<ScanReading> (fun reading -> reading.value)

        for star, tag in [ StarredKit, 1; UnstarredKit, 2 ] do
            let json = toJson star
            use doc = JsonDocument.Parse json
            Assert.Equal(tag, doc.RootElement.GetInt32())
            Assert.Equal<KitStar>(star, ofJson<KitStar> json)

        Assert.Equal<string>("3", toJson DarkRun)
        Assert.Equal<ExperimentRole>(DarkRun, ofJson<ExperimentRole> "3")

        Assert.Equal<string>("2", toJson SemicolonDelimiter)
        Assert.Equal<DataDelimiter>(SemicolonDelimiter, ofJson<DataDelimiter> "2")
        Assert.Equal<string>("2", toJson NoHeaderRow)
        Assert.Equal<HeaderRowChoice>(NoHeaderRow, ofJson<HeaderRowChoice> "2")
        Assert.Equal<string>("1", toJson ByteOrderMarkPresent)
        Assert.Equal<ByteOrderMark>(ByteOrderMarkPresent, ofJson<ByteOrderMark> "1")
        Assert.Equal<string>("6", toJson WavenumberColumnUnit)
        Assert.Equal<ScanColumnUnit>(WavenumberColumnUnit, ofJson<ScanColumnUnit> "6")
        Assert.Equal<string>("1", toJson RawDialReading)
        Assert.Equal<ScanReading>(RawDialReading, ofJson<ScanReading> "1")

        let schema : EntrySchema = { kind = SourceEntryKind; version = FileSchemaVersion 1 }
        let json = toJson schema
        use doc = JsonDocument.Parse json
        Assert.Equal(SourceEntryKind.value, doc.RootElement.GetProperty("kind").GetInt32())
        Assert.Equal<EntrySchema>(schema, ofJson<EntrySchema> json)

    [<Fact>]
    let ``an unknown disk tag is refused`` () =
        Assert.Throws<JsonException>(fun () -> ofJson<LibraryEntryKind> "9" |> ignore) |> ignore
        Assert.Throws<JsonException>(fun () -> ofJson<PublishedIndexLaw> "4" |> ignore) |> ignore
        Assert.Throws<JsonException>(fun () -> ofJson<LibraryEntryKind> "0" |> ignore) |> ignore
        Assert.Throws<JsonException>(fun () -> ofJson<LibraryEntryKind> "\"MaterialEntryKind\"" |> ignore) |> ignore
        Assert.Throws<JsonException>(fun () -> ofJson<KitStar> "3" |> ignore) |> ignore
        Assert.Throws<JsonException>(fun () -> ofJson<ExperimentRole> "8" |> ignore) |> ignore
        Assert.Throws<JsonException>(fun () -> ofJson<DataDelimiter> "4" |> ignore) |> ignore
        Assert.Throws<JsonException>(fun () -> ofJson<ScanColumnUnit> "7" |> ignore) |> ignore

        // No file holds a role before step 288, so the role rides an anonymous record through the readers' seam.
        let roleText = JsonText "{ \"schemaVersion\": 1, \"role\": 8 }"
        match parseVersionedText<{| schemaVersion : FileSchemaVersion; role : ExperimentRole |}> (fun _ -> Ok ()) (FileExtension ".json") (FileSchemaVersion 1) roleText with
        | Error (MalformedJsonErr reason) -> Assert.Contains(nameof ExperimentRole, reason)
        | other -> Assert.Fail($"the role tag 8: expected MalformedJsonErr, got %A{other}")

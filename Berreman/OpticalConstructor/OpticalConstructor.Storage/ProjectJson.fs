namespace OpticalConstructor.Storage

open System.Numerics
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open MathNet.Numerics.LinearAlgebra
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage.Errors

/// Canonical project (de)serialization (§I.1 / R-1) over the §A.7
/// `OpticalConstructorProject` aggregate, using System.Text.Json +
/// FSharp.SystemTextJson. No Newtonsoft, no FsPickler on this path (binding
/// rule 4). The serializer writes the domain model's canonical SI fields
/// verbatim — no unit conversion, rounding, or canonicalization of magnitudes.
module ProjectJson =

    /// The single shipped schema version (§I.2). A document whose `schemaVersion`
    /// differs surfaces as `SchemaValidationError` (no migration; AC-I3). Pinned
    /// with `const` in the schema and injected into the serialized envelope.
    [<Literal>]
    let schemaVersion = "1.0"

    /// Custom converter for the engine's Math.NET complex matrix
    /// (`Matrix<Complex>`). FSharp.SystemTextJson unwraps the single-case
    /// `Eps`/`Mu`/`Rho` -> `ComplexMatrix3x3` -> `ComplexMatrix` wrappers down to
    /// this Math.NET type, which System.Text.Json cannot round-trip on its own.
    /// It is serialized as a rectangular array of `[re, im]` pairs and rebuilt as
    /// a dense matrix on read, so Math.NET's element-wise value-equality makes a
    /// whole-`OpticalConstructorProject` round-trip compare equal.
    type private ComplexMatrixConverter() =
        inherit JsonConverter<Matrix<Complex>>()

        override _.Write(writer, value, _options) =
            writer.WriteStartArray()
            for i in 0 .. value.RowCount - 1 do
                writer.WriteStartArray()
                for j in 0 .. value.ColumnCount - 1 do
                    let c = value.[i, j]
                    writer.WriteStartArray()
                    writer.WriteNumberValue c.Real
                    writer.WriteNumberValue c.Imaginary
                    writer.WriteEndArray()
                writer.WriteEndArray()
            writer.WriteEndArray()

        override _.Read(reader, _typeToConvert, _options) =
            use doc = JsonDocument.ParseValue(&reader)
            let root = doc.RootElement
            let rows = root.GetArrayLength()
            let cols = if rows = 0 then 0 else root.[0].GetArrayLength()
            let arr = Array2D.zeroCreate rows cols
            let mutable i = 0
            for rowEl in root.EnumerateArray() do
                let mutable j = 0
                for cellEl in rowEl.EnumerateArray() do
                    let re = cellEl.[0].GetDouble()
                    let im = cellEl.[1].GetDouble()
                    arr.[i, j] <- Complex(re, im)
                    j <- j + 1
                i <- i + 1
            Matrix<Complex>.Build.DenseOfArray arr

    /// The single shared `JsonSerializerOptions` (§I.1 — registered once). F#
    /// records, `option` values, and discriminated unions round-trip idiomatically
    /// via FSharp.SystemTextJson: single-case unions unwrap (`Eps`, `WaveLength`'s
    /// magnitudes, `RefractionIndex`, ...), `option` unwraps to value-or-null, and
    /// fieldless union cases (`UnitOfMeasure`, `BeamBranch`, the nullary
    /// `ConstructorElement` cases) serialize as plain strings.
    let options : JsonSerializerOptions =
        let o = JsonSerializerOptions(WriteIndented = true)
        o.Converters.Add(ComplexMatrixConverter())
        let fsharpOptions =
            JsonFSharpOptions
                .Default()
                .WithUnionUnwrapFieldlessTags()
                .WithUnionUnwrapSingleCaseUnions()
                .WithUnwrapOption()
        o.Converters.Add(JsonFSharpConverter(fsharpOptions))
        o

    /// Serialize a project to canonical JSON text (§I.1 / R-1), injecting the
    /// envelope's `schemaVersion`. The aggregate carries no `schemaVersion` field
    /// of its own; it is an envelope concern, added here and validated on load.
    let serializeProject (project : OpticalConstructorProject) : Result<string, StorageError> =
        try
            let node = JsonSerializer.SerializeToNode(project, options)
            let envelope = node.AsObject()
            envelope.["schemaVersion"] <- JsonValue.Create(schemaVersion)
            Ok(envelope.ToJsonString(options))
        with e -> Error(JsonParseError e.Message)

    /// Parse and validate-on-load, then bind (§I.1 / §I.2 / R-1, R-2). Schema
    /// validation ALWAYS runs before the domain model is admitted; on any
    /// validation failure the function returns `SchemaValidationError` and binds
    /// nothing. A `schemaVersion` mismatch fails the schema's `const` and so
    /// surfaces as `SchemaValidationError`, never a migration (AC-I3).
    let deserializeProject (json : string) : Result<OpticalConstructorProject, StorageError> =
        let parsed =
            try Ok(JsonDocument.Parse json)
            with e -> Error(JsonParseError e.Message)

        match parsed with
        | Error e -> Error e
        | Ok doc ->
            use doc = doc
            match SchemaValidation.validate doc.RootElement with
            | Error e -> Error e
            | Ok () ->
                try Ok(doc.RootElement.Deserialize<OpticalConstructorProject>(options))
                with e -> Error(JsonParseError e.Message)

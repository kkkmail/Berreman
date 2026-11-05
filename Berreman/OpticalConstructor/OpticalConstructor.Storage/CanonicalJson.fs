namespace OpticalConstructor.Storage

open System.Numerics
open System.Text.Json
open System.Text.Json.Serialization
open MathNet.Numerics.LinearAlgebra

/// The canonical JSON stack every JSON store of the app shares (spec 0050 001 §B, `.spec-md` §5).
module CanonicalJson =

    /// Keeps `BeamBranch` on the wire as the schema's `"Reflected"` / `"Transmitted"`: the cases were
    /// renamed apart from the engine's `RT` (spec 0048 `.manual/005`), the stored format was not.
    type private BeamBranchConverter() =
        inherit JsonConverter<OpticalConstructor.Domain.BeamTree.BeamBranch>()

        override _.Write(writer, value, _options) =
            match value with
            | OpticalConstructor.Domain.BeamTree.ReflectedBeam -> writer.WriteStringValue "Reflected"
            | OpticalConstructor.Domain.BeamTree.TransmittedBeam -> writer.WriteStringValue "Transmitted"

        override _.Read(reader, _typeToConvert, _options) =
            match reader.GetString() with
            | "Reflected" -> OpticalConstructor.Domain.BeamTree.ReflectedBeam
            | "Transmitted" -> OpticalConstructor.Domain.BeamTree.TransmittedBeam
            | other -> raise (JsonException $"unknown beam branch '{other}'")

    /// Keeps a rotation lock on the wire as the schema's boolean, `true` for locked: the
    /// lock became a union (spec 0048 E013), the stored format did not.
    type private RotationLockConverter() =
        inherit JsonConverter<OpticalConstructor.Domain.Placement.RotationLock>()

        override _.Write(writer, value, _options) =
            match value with
            | OpticalConstructor.Domain.Placement.RotationLocked -> writer.WriteBooleanValue true
            | OpticalConstructor.Domain.Placement.RotationFree -> writer.WriteBooleanValue false

        override _.Read(reader, _typeToConvert, _options) =
            match reader.GetBoolean() with
            | true -> OpticalConstructor.Domain.Placement.RotationLocked
            | false -> OpticalConstructor.Domain.Placement.RotationFree

    /// Keeps a group member's presence on the wire as the groups schema's boolean `inBeam`, `true`
    /// for in the beam: it became a union (spec 0048 E014), the stored format did not.
    type private BeamPresenceConverter() =
        inherit JsonConverter<OpticalConstructor.Domain.Groups.BeamPresence>()

        override _.Write(writer, value, _options) =
            match value with
            | OpticalConstructor.Domain.Groups.InBeam -> writer.WriteBooleanValue true
            | OpticalConstructor.Domain.Groups.OutOfBeam -> writer.WriteBooleanValue false

        override _.Read(reader, _typeToConvert, _options) =
            match reader.GetBoolean() with
            | true -> OpticalConstructor.Domain.Groups.InBeam
            | false -> OpticalConstructor.Domain.Groups.OutOfBeam

    /// Keeps a toggle's state on the wire as the groups schema's boolean `enabled`, `true` for
    /// enabled: it became a union (spec 0048 E014), the stored format did not.
    type private ToggleStateConverter() =
        inherit JsonConverter<OpticalConstructor.Domain.Groups.ToggleState>()

        override _.Write(writer, value, _options) =
            match value with
            | OpticalConstructor.Domain.Groups.ToggleEnabled -> writer.WriteBooleanValue true
            | OpticalConstructor.Domain.Groups.ToggleDisabled -> writer.WriteBooleanValue false

        override _.Read(reader, _typeToConvert, _options) =
            match reader.GetBoolean() with
            | true -> OpticalConstructor.Domain.Groups.ToggleEnabled
            | false -> OpticalConstructor.Domain.Groups.ToggleDisabled

    /// Keeps a `System.Numerics.Complex` (an absorbing material's `ComplexRefractionIndex`, a complex
    /// matrix's cell) on the wire as `[re, im]`: System.Text.Json alone writes its four properties and
    /// reads back zero (spec 0050 001 §B).
    type private ComplexConverter() =
        inherit JsonConverter<Complex>()

        override _.Write(writer, value, _options) =
            writer.WriteStartArray()
            writer.WriteNumberValue value.Real
            writer.WriteNumberValue value.Imaginary
            writer.WriteEndArray()

        override _.Read(reader, _typeToConvert, options) =
            match JsonSerializer.Deserialize<float[]>(&reader, options) with
            | [| re; im |] -> Complex(re, im)
            | _ -> raise (JsonException "a complex number is the pair [re, im]")

    /// Custom converter for the engine's Math.NET complex matrix
    /// (`Matrix<Complex>`). FSharp.SystemTextJson unwraps the single-case
    /// `Eps`/`Mu`/`Rho` -> `ComplexMatrix3x3` -> `ComplexMatrix` wrappers down to
    /// this Math.NET type, which System.Text.Json cannot round-trip on its own.
    /// It is serialized as a rectangular array of cells, each through `ComplexConverter`, and rebuilt as
    /// a dense matrix on read, so Math.NET's element-wise value-equality makes a
    /// whole-`OpticalConstructorProject` round-trip compare equal.
    type private ComplexMatrixConverter() =
        inherit JsonConverter<Matrix<Complex>>()

        override _.Write(writer, value, options) =
            writer.WriteStartArray()
            for i in 0 .. value.RowCount - 1 do
                writer.WriteStartArray()
                for j in 0 .. value.ColumnCount - 1 do
                    JsonSerializer.Serialize<Complex>(writer, value.[i, j], options)
                writer.WriteEndArray()
            writer.WriteEndArray()

        override _.Read(reader, _typeToConvert, options) =
            use doc = JsonDocument.ParseValue(&reader)
            let root = doc.RootElement
            let rows = root.GetArrayLength()
            let cols = if rows = 0 then 0 else root.[0].GetArrayLength()
            let arr = Array2D.zeroCreate rows cols
            let mutable i = 0
            for rowEl in root.EnumerateArray() do
                let mutable j = 0
                for cellEl in rowEl.EnumerateArray() do
                    arr.[i, j] <- cellEl.Deserialize<Complex>(options)
                    j <- j + 1
                i <- i + 1
            Matrix<Complex>.Build.DenseOfArray arr

    /// Keeps a Math.NET real matrix (`Matrix<double>`: a `MuellerMatrix` unwrapped through
    /// `RealMatrix4x4` and `RealMatrix`) on the wire as its array of rows of numbers: it is abstract, so
    /// System.Text.Json alone cannot read it back (spec 0050 001 §B).
    type private RealMatrixConverter() =
        inherit JsonConverter<Matrix<double>>()

        override _.Write(writer, value, _options) =
            writer.WriteStartArray()
            for i in 0 .. value.RowCount - 1 do
                writer.WriteStartArray()
                for j in 0 .. value.ColumnCount - 1 do
                    writer.WriteNumberValue value.[i, j]
                writer.WriteEndArray()
            writer.WriteEndArray()

        override _.Read(reader, _typeToConvert, options) =
            let rows = JsonSerializer.Deserialize<float[][]>(&reader, options)
            match rows |> Array.forall (fun row -> not (isNull row) && row.Length = rows.[0].Length) with
            | true -> Matrix<double>.Build.DenseOfArray(array2D rows)
            | false -> raise (JsonException "a real matrix is an array of equal-length rows of numbers")

    /// The single shared `JsonSerializerOptions` (§I.1 — registered once): the one options value every
    /// JSON store of the app reads (spec 0050). F# records, `option` values, and discriminated unions
    /// round-trip idiomatically via FSharp.SystemTextJson: single-case unions unwrap (`Eps`,
    /// `WaveLength`'s magnitudes, `RefractionIndex`, ...), `option` unwraps to value-or-null, and
    /// fieldless union cases (`UnitOfMeasure`, `BeamBranch`, the nullary `ConstructorElement` cases)
    /// serialize as plain strings. A `Complex` is `[re, im]` (`ComplexConverter`) and a real matrix its
    /// rows (`RealMatrixConverter`), as an entry file holds them (001 §B).
    let options : JsonSerializerOptions =
        let o = JsonSerializerOptions(WriteIndented = true)
        o.Converters.Add(ComplexConverter())
        o.Converters.Add(ComplexMatrixConverter())
        o.Converters.Add(RealMatrixConverter())
        o.Converters.Add(BeamBranchConverter())
        o.Converters.Add(RotationLockConverter())
        o.Converters.Add(BeamPresenceConverter())
        o.Converters.Add(ToggleStateConverter())
        let fsharpOptions =
            JsonFSharpOptions
                .Default()
                .WithUnionUnwrapFieldlessTags()
                .WithUnionUnwrapSingleCaseUnions()
                .WithUnwrapOption()
        o.Converters.Add(JsonFSharpConverter(fsharpOptions))
        o

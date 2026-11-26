namespace OpticalConstructor.Storage

open System.IO
open System.Text.Json
open Json.Schema
open Softellect.Sys.Primitives
open Berreman.MathNetNumericsMath
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.SchemaValidation

/// Material metadata import/export (§I.8 / R-4; spec 0050 S4.4, manual 012).
/// JSON exports use schema/material-metadata.schema.json; CSV imports reuse MaterialImport.
/// Category membership is resolved through the caller's list.
module MaterialLibrary =

    /// Metadata-only materialEntry DTO: category display names resolve through the caller's categories
    /// on import and export (spec 0035 step 001; spec 0050 S1.7).
    [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library file's materialEntry shape: System.Text.Json binds these fields by name from the file, and dtoToEntry elevates each one at the read")>]
    type private MaterialEntryDto =
        {
            id : string
            name : string
            category : string
            description : string option
        }

    /// The physics of an entry rebuilt from a metadata-only library JSON: vacuum (n = 1 + 0i) as
    /// constant term data, editable like any constant material (spec 0050 step 11).
    let vacuumPlaceholderComplexity : MaterialComplexity =
        {
            eps = EpsWithoutDispValue (IsotropicAbsorbing (ComplexRefractionIndex (System.Numerics.Complex.create 1.0 0.0)))
            magnetic = None
            active = None
        }

    let private materialEntrySchemaFile : FileName =
        FileName (Path.Combine("schema", "material-metadata.schema.json"))

    /// Load schema/material-metadata.schema.json once; caller lists own category membership
    /// (spec 0050 S1.7 / S4.4, manual 012).
    let private materialEntrySchema : Lazy<Result<JsonSchema, StorageError>> =
        lazy (materialEntrySchemaFile.tryLoadSchema ())

    /// Validate material-entry structure through the reused `$def` (§I.8 / AC-I10; spec 0050 S1.7).
    /// The import/export boundary resolves category membership through the caller's list.
    let validateEntryElement (element : JsonElement) : Result<unit, StorageError> =
        validateWith materialEntrySchema element

    /// Parse a persisted DTO back to a domain entry. The persisted `id` is the `MaterialId`'s Guid
    /// STRING form (spec 0033 step 002); a non-Guid id is a typed error — no legacy string-id path,
    /// no fallback, no migration.
    let private dtoToEntry (categories : MaterialCategory list) (dto : MaterialEntryDto) : Result<MaterialEntry, StorageError> =
        // Invalid GUIDs and names absent from the caller's categories remain typed errors
        // (spec 0033 step 002 / 0035 step 001; spec 0050 S1.7).
        let categoryName = CategoryName dto.category
        match MaterialId.tryCreate dto.id, tryFindCategoryByName categories categoryName with
        | Some id, Some category ->
            Ok
                {
                    id = id
                    name = EntryName dto.name
                    category = category.id
                    description = dto.description |> Option.map EntryDescription
                    physics = ComplexityPhysics vacuumPlaceholderComplexity
                }
        | None, _ -> Error (MalformedJsonErr $"materialEntry id '%s{dto.id}' is not a Guid")
        | _, None -> Error (MalformedJsonErr $"materialEntry category '%s{dto.category}' is not a known category")

    type FileName with

        /// Import the material library in this file (§I.8 / AC-I10). A document that begins with `[`/`{`
        /// is the canonical library JSON `exportMaterials` wrote (deserialized through the
        /// shared `CanonicalJson.options` — no second JSON stack); anything else is treated
        /// as refractiveindex.info-style tabulated CSV and parsed by `MaterialImport`
        /// (FSharp.Data). Returns `MaterialEntry list`; file/parse failures are values,
        /// never thrown.
        member this.importMaterials (categories : MaterialCategory list) : Result<MaterialEntry list, StorageError> =
            let read =
                try Ok(File.ReadAllText this.value)
                with e -> Error(FileIoExn e)
            match read with
            | Error e -> Error e
            | Ok text ->
                let trimmed = text.TrimStart()
                if trimmed.StartsWith "[" || trimmed.StartsWith "{" then
                    try
                        let dtos = JsonSerializer.Deserialize<MaterialEntryDto list>(text, CanonicalJson.options)
                        let rec toEntries (pending : MaterialEntryDto list) (acc : MaterialEntry list) : Result<MaterialEntry list, StorageError> =
                            match pending with
                            | [] -> Ok(List.rev acc)
                            | dto :: rest ->
                                match dtoToEntry categories dto with
                                | Ok entry -> toEntries rest (entry :: acc)
                                | Error e -> Error e
                        toEntries dtos []
                    with e -> Error(MalformedJsonErr e.Message)
                else
                    match (MaterialImport.NkCsvText text).importMaterial with
                    | Ok entry -> Ok [ entry ]
                    | Error e -> Error(MalformedJsonErr(string e))

        /// Export schema-checked material metadata using the caller's category names (§I.8 / AC-I10;
        /// spec 0050 S1.7). Missing category ids or invalid structure return SchemaValidationErr
        /// before writing; absent descriptions are omitted and IO failures remain typed errors.
        member this.exportMaterials (categories : MaterialCategory list) (entries : MaterialEntry list) : Result<unit, StorageError> =
            try
                let rec toValidatedNodes
                    (pending : MaterialEntry list)
                    (acc : System.Collections.Generic.Dictionary<string, obj> list)
                    : Result<System.Collections.Generic.Dictionary<string, obj> list, StorageError> =
                    match pending with
                    | [] -> Ok (List.rev acc)
                    | entry :: rest ->
                        // Only names in the caller's list can round-trip through import (spec 0035 step 001; spec 0050 S1.7).
                        match tryFindCategory categories entry.category with
                        | None ->
                            Error (SchemaValidationErr [ $"materialEntry category '{entry.category.value}' is absent from the supplied categories" ])
                        | Some category ->
                            let dict = System.Collections.Generic.Dictionary<string, obj>()
                            dict.["id"] <- box (string entry.id.value)
                            dict.["name"] <- box entry.name.value
                            dict.["category"] <- box category.name.value
                            match entry.description with
                            | Some description -> dict.["description"] <- box description.value
                            | None -> ()
                            let json = JsonSerializer.Serialize(dict, CanonicalJson.options)
                            use doc = JsonDocument.Parse json
                            match validateEntryElement doc.RootElement with
                            | Ok () -> toValidatedNodes rest (dict :: acc)
                            | Error e -> Error e
                match toValidatedNodes entries [] with
                | Error e -> Error e
                | Ok nodes ->
                    let json = JsonSerializer.Serialize(nodes, CanonicalJson.options)
                    File.WriteAllText(this.value, json, System.Text.UTF8Encoding false)
                    Ok()
            with e -> Error(FileIoExn e)

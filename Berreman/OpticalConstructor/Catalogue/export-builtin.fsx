// The throw-away export (spec 0050 001 §B "Shipped catalogue"; .spec-md §3.5, §3.6): seals every code seed and writes
// Builtin/ through the product writer, so no second writer exists. Build the solution, then run it once from the
// repository root: dotnet fsi Berreman/OpticalConstructor/Catalogue/export-builtin.fsx

#I "../OpticalConstructor.Tests/bin/x64/Release/net10.0/"
#r "Softellect.Sys.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"
#r "FSharp.SystemTextJson.dll"
#r "Json.More.dll"
#r "JsonPointer.Net.dll"
#r "JsonSchema.Net.dll"
#r "Softellect.Berreman.Core.dll"
#r "Softellect.Berreman.OpticalProperties.dll"
#r "Softellect.Berreman.Analytics.dll"
#r "Softellect.Berreman.OpticalConstructor.Domain.dll"
#r "Softellect.Berreman.OpticalConstructor.Storage.dll"

open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.CatalogueFolderStore

let okOrExit (result : Result<'Value, 'Problem>) : 'Value =
    match result with
    | Ok value -> value
    | Error problem ->
        printfn $"export-builtin failed: %A{problem}"
        exit 1

let builtIn : EntryProvenance = BuiltInProvenance CatalogueIds.builtInCatalogue

let seedDetectors : DetectorPreset list =
    seedEntries
    |> List.choose (fun (entry : LibraryEntry) ->
        match entry with
        | DetectorItem detector -> Some detector
        | _ -> None)

let seedPolarizers : PolarizerPreset list =
    seedEntries
    |> List.choose (fun (entry : LibraryEntry) ->
        match entry with
        | PolarizerItem polarizer -> Some polarizer
        | _ -> None)

let contents : LibraryEntryContent list =
    (builtInEntries |> List.map (LibraryEntryContent.ofMaterialEntry VersionNumber.first builtIn))
    @ (standardCategories |> List.map (LibraryEntryContent.ofMaterialCategory VersionNumber.first builtIn >> okOrExit))
    @ (SeedSamples.allSeedSamples |> List.map (LibraryEntryContent.ofSample VersionNumber.first builtIn))
    @ (SeedSources.allSeedSources |> List.map (LibraryEntryContent.ofSourcePreset VersionNumber.first builtIn >> okOrExit))
    @ (seedDetectors |> List.map (LibraryEntryContent.ofDetectorPreset VersionNumber.first builtIn >> okOrExit))
    @ (seedPolarizers |> List.map (LibraryEntryContent.ofPolarizerPreset VersionNumber.first builtIn >> okOrExit))

let catalogue : CatalogueData =
    contents
    |> List.map (fun (content : LibraryEntryContent) -> content.``sealed`` |> okOrExit)
    |> CatalogueData.tryCreate CatalogueIds.builtInCatalogue (CatalogueName "Built-in") VersionNumber.first
    |> okOrExit

let proxy : CatalogueProxy = CatalogueProxy.create (FolderName (Path.Combine(__SOURCE_DIRECTORY__, "Builtin")))

proxy.writeCatalogue catalogue |> okOrExit

printfn $"export-builtin wrote {catalogue.envelopes.Length} entries"

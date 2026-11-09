// The throw-away export of the built-in item models (spec 0050 S7.1; 001 §B "Shipped catalogue"): reads Builtin/,
// seals the eleven item models and writes the catalogue back through the product writer, so no second writer exists.
// Build the solution, then run it once from the repository root: dotnet fsi Berreman/OpticalConstructor/Catalogue/export-item-models.fsx
// Step 212 deletes it.

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
#r "../../BerremanTests/bin/x64/Release/net10.0/BerremanTests.dll"

open System
open System.IO
open Berreman.Constants
open Berreman.Geometry
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.CatalogueFolderStore

let okOrExit (describe : 'Problem -> string) (result : Result<'Value, 'Problem>) : 'Value =
    match result with
    | Ok value -> value
    | Error problem ->
        printfn $"export-item-models failed: {describe problem}"
        exit 1

// Under dotnet fsi the base directory is the SDK's, but the reader validates every entry against the schemas the build
// copied beside the test assemblies, and the schema loader resolves `schema/` against AppContext.BaseDirectory.
AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY", Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../OpticalConstructor.Tests/bin/x64/Release/net10.0/")))

// The decent-lab budget, read once from the built bench model and never transcribed (step 339 checks its parity).
let budget : BerremanTests.NoiseParam = BerremanTests.NoiseParam.decentLab

let detectorNoiseAxes : ItemParameterAxis list =
    ItemParameterAxis.detectorNoiseAxes (ParameterValue budget.rotation.value) (ParameterValue budget.element.value) (ParameterValue budget.throughput.value)

let builtInItemModel (id : ItemModelId) (name : EntryName) (model : ItemModelForm) (description : EntryDescription) : ItemModelEntry =
    {
        id = id
        name = name
        description = description
        model = model
        protection = ProtectedBuiltIn
    }

let itemModels : ItemModelEntry list =
    [
        builtInItemModel
            ItemModelIds.realLinearPolarizerModelId
            (EntryName "Real linear polarizer")
            (OpticalElementModel (RealPolarizerModel ExtinctionRatioLeakage))
            (EntryDescription "A linear polarizer with an extinction ratio and an axis offset; at its defaults it is the ideal linear polarizer.")
        builtInItemModel
            ItemModelIds.retarderPlateModelId
            (EntryName "Retarder plate")
            (OpticalElementModel (RetarderPlateModel ConstantRetardance))
            (EntryDescription "A wave plate with a retardance and a fast-axis offset; at its defaults it is a quarter-wave plate.")
        builtInItemModel
            ItemModelIds.circularPolarizerModelId
            (EntryName "Circular polarizer (linear polarizer and retarder)")
            (OpticalElementModel (
                CompoundModel
                    [
                        { form = RealPolarizerModel ExtinctionRatioLeakage; offset = Angle.zero }
                        { form = RetarderPlateModel ConstantRetardance; offset = Angle.degree 45.0<deg> }
                    ]))
            (EntryDescription "A linear polarizer followed in light order by a retarder whose fast axis sits at 45° plus its own offset.")
        builtInItemModel
            ItemModelIds.blackBoxMuellerModelId
            (EntryName "Black-box Mueller matrix")
            (OpticalElementModel (BlackBoxMuellerModel SixteenElementMueller))
            (EntryDescription "A sample known only by its sixteen Mueller elements at reference orientation; at its defaults it is the identity.")
        builtInItemModel
            ItemModelIds.emptySlotModelId
            (EntryName "Empty slot (air)")
            EmptySlotModel
            (EntryDescription "Nothing in the beam: the identity, the calibration reference.")
        builtInItemModel
            ItemModelIds.intensityDetectorModelId
            (EntryName "Intensity detector (gain, dark offset and noise)")
            (IntensityDetectorModel detectorNoiseAxes)
            (EntryDescription "Records the intensity S0 through a gain and a dark offset, with the decent-lab noise budget.")
        builtInItemModel
            ItemModelIds.muellerPolarimeterModelId
            (EntryName "Mueller polarimeter")
            (MuellerPolarimeterModel detectorNoiseAxes)
            (EntryDescription "Records a full Mueller matrix, with the decent-lab noise budget.")
        builtInItemModel
            ItemModelIds.ellipsometerModelId
            (EntryName "Ellipsometer (noise budget)")
            (EllipsometerModel detectorNoiseAxes)
            (EntryDescription "Records Psi and Delta, with the decent-lab noise budget.")
        builtInItemModel
            ItemModelIds.sourcePresetItemModelId
            (EntryName "Light source (preset binding)")
            SourcePresetItemModel
            (EntryDescription "A light source bound to a library source preset (SourcePresetBinding): the preset's wavelength and tuning, with the output polarization and intensity scale as its parameters.")
        builtInItemModel
            ItemModelIds.idealPolarizerItemModelId
            (EntryName "Ideal polarizer (preset binding)")
            IdealPolarizerItemModel
            (EntryDescription "An ideal polarizer bound to a library polarizer preset (PolarizerPresetBinding): the preset's linear, circular or constant-Mueller behaviour, with no parameters of its own.")
        builtInItemModel
            ItemModelIds.stackSampleItemModelId
            (EntryName "Sample by stack")
            StackSampleItemModel
            (EntryDescription "A sample bound to a library sample (StackSampleBinding): the sample's layer stack, with each finite layer's thickness as a parameter.")
    ]

let builtIn : EntryProvenance = BuiltInProvenance CatalogueIds.builtInCatalogue

let proxy : CatalogueProxy = CatalogueProxy.create (FolderName (Path.Combine(__SOURCE_DIRECTORY__, "Builtin")))

let shipped : CatalogueData = proxy.readCatalogue () |> okOrExit (fun (error : CatalogueError) -> error.describe)

let sealedItemModels : LibraryEntryEnvelope list =
    itemModels
    |> List.map (fun (entry : ItemModelEntry) ->
        let content = LibraryEntryContent.ofItemModel VersionNumber.first builtIn entry |> okOrExit (fun (error : LibraryEntryError) -> error.describe)
        content.``sealed`` |> okOrExit (fun (error : StorageError) -> error.describe))

let catalogue : CatalogueData =
    shipped.envelopes @ sealedItemModels
    |> CatalogueData.tryCreate CatalogueIds.builtInCatalogue (CatalogueName "Built-in") VersionNumber.first
    |> okOrExit (fun (error : CatalogueError) -> error.describe)

proxy.writeCatalogue catalogue |> okOrExit (fun (error : CatalogueError) -> error.describe)

printfn $"export-item-models wrote {catalogue.envelopes.Length} entries, {sealedItemModels.Length} of them item models"

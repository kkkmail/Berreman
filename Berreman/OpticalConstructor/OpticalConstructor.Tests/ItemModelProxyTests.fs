namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels

module private ItemModelProxyTestData =

    // Lab retarder's GUID sorts before Lab polarizer's, so a listing by id would answer them the other way round.
    let labRetarderId : ItemModelId = Guid.Parse "1b6f0c2e-7d4a-4e91-a3b8-5c2d9e0f4a17" |> ItemModelId
    let labPolarizerId : ItemModelId = Guid.Parse "e4a97d35-02bc-4f68-9e1d-7b3c6a8f2d50" |> ItemModelId
    let unheldId : ItemModelId = Guid.Parse "6d2e8b14-a9f3-4c07-b5e6-1f4a3c9d7e82" |> ItemModelId

    let labRetarder : ItemModelVersion =
        {
            version = VersionNumber 2
            entry =
                {
                    id = labRetarderId
                    name = EntryName "Lab retarder"
                    description = EntryDescription "The bench's wave plate, its retardance the same at every wavelength."
                    model = OpticalElementModel (RetarderPlateModel ConstantRetardance)
                    protection = UserManaged
                }
        }

    let labPolarizer : ItemModelVersion =
        {
            version = VersionNumber.first
            entry =
                {
                    id = labPolarizerId
                    name = EntryName "Lab polarizer"
                    description = EntryDescription "The bench's linear polarizer, its leakage stated by diattenuation."
                    model = OpticalElementModel (RealPolarizerModel DiattenuationLeakage)
                    protection = UserManaged
                }
        }

open ItemModelProxyTestData

/// An item-model store saves a new item model at its version, resolves exactly that version, lists the latest version
/// of each by name and refuses an id it already holds (spec 0050 S7.1; 001 §C 'Contract tests').
[<AbstractClass>]
type ItemModelProxyContract () =

    /// Returns a fresh store holding no item model.
    abstract freshItemModels : unit -> ItemModelProxy

    [<Fact>]
    member this.``an item model is saved at its version and resolved by exactly that version`` () : unit =
        let store = this.freshItemModels ()
        Assert.Equal<Result<unit, ItemModelError>>(Ok (), store.saveItemModel labRetarder)

        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(
            Ok (Some labRetarder), store.resolveItemModelVersion labRetarder.versionId)
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(
            Ok None, store.resolveItemModelVersion { labRetarder.versionId with version = VersionNumber.first })
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(
            Ok None, store.resolveItemModelVersion { labRetarder.versionId with itemModelId = unheldId })

    [<Fact>]
    member this.``the store lists the latest version of every item model, by name`` () : unit =
        let store = this.freshItemModels ()
        Assert.Equal<Result<ItemModelVersion list, ItemModelError>>(Ok [], store.listItemModels ())
        Assert.Equal<Result<unit, ItemModelError>>(Ok (), store.saveItemModel labRetarder)
        Assert.Equal<Result<unit, ItemModelError>>(Ok (), store.saveItemModel labPolarizer)

        Assert.Equal<Result<ItemModelVersion list, ItemModelError>>(Ok [ labPolarizer; labRetarder ], store.listItemModels ())

    [<Fact>]
    member this.``saving an item model the store already holds is DuplicateItemModelErr and keeps the held version`` () : unit =
        let store = this.freshItemModels ()
        Assert.Equal<Result<unit, ItemModelError>>(Ok (), store.saveItemModel labRetarder)

        let renamed : ItemModelVersion =
            {
                version = VersionNumber 3
                entry = { labRetarder.entry with name = EntryName "Lab retarder, remeasured" }
            }

        match store.saveItemModel renamed with
        | Error (DuplicateItemModelErr reason) -> Assert.Contains(labRetarderId.value.ToString(), reason)
        | Error error -> Assert.Fail($"Expected DuplicateItemModelErr, got {error.describe}")
        | Ok () -> Assert.Fail("Saving an item model under a held id must be refused.")

        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(
            Ok (Some labRetarder), store.resolveItemModelVersion labRetarder.versionId)
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok None, store.resolveItemModelVersion renamed.versionId)

type ItemModelProxyTests () =
    inherit ItemModelProxyContract ()

    override _.freshItemModels () : ItemModelProxy = ItemModelProxy.createInMemory ()

    [<Fact>]
    member _.``every item-model error renders its condition and its payload`` () : unit =
        let duplicateReason = "'Lab retarder' (1b6f0c2e-7d4a-4e91-a3b8-5c2d9e0f4a17) at version 2"
        let invalidReason = "a protected built-in arrives only through the import seam"
        let unavailableReason = "the library database was disposed"
        let cases : (ItemModelError * string) list =
            [
                DuplicateItemModelErr duplicateReason, $"the item model is already held: {duplicateReason}"
                InvalidItemModelErr invalidReason, $"the item model cannot be stored: {invalidReason}"
                ItemModelStoreUnavailableErr unavailableReason, $"the item-model store is unavailable: {unavailableReason}"
            ]

        cases |> List.iter (fun (error, expected) -> Assert.Equal(expected, error.describe))
        Assert.Equal(cases.Length, cases |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)

namespace OpticalConstructor.Tests

open System
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.ItemModelDatabaseStore

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

/// The SQLite item-model store runs the contract over an empty database per store, and its own facts over the
/// Built-in library database (spec 0050 S7.1; 001 §C 'Contract tests').
type ItemModelProxySqliteTests () =
    inherit ItemModelProxyContract ()

    let databases = ResizeArray<LibraryDatabase>()

    override _.freshItemModels () : ItemModelProxy =
        let database =
            LibraryDatabase.openOrCreate (ConnectionString "Data Source=:memory:")
            |> Result.defaultWith (fun error -> failwith error.describe)
        databases.Add database
        ItemModelProxy.createInDatabase database

    [<Fact>]
    member _.``the database item-model store lists the Built-in item models and resolves each by its version`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let store = ItemModelProxy.createInDatabase database
        let builtIns = BuiltinCatalogue.itemModels () |> List.sortBy (fun (held : ItemModelVersion) -> held.entry.name)
        Assert.Contains(BuiltinCatalogue.itemModel ItemModelIds.emptySlotModelId, builtIns)

        Assert.Equal<Result<ItemModelVersion list, ItemModelError>>(Ok builtIns, store.listItemModels ())
        for itemModel in builtIns do
            Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(
                Ok (Some itemModel), store.resolveItemModelVersion itemModel.versionId)

    [<Fact>]
    member _.``the database item-model store holds user item models only and refuses an id it cannot hold`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let store = ItemModelProxy.createInDatabase database
        let latestOf (entryId : LibraryEntryId) : StoredEntryVersion option =
            database.run (fun context -> context.tryLatestStored entryId)
            |> Result.bind id
            |> Result.defaultWith (fun error -> failwith error.describe)
        let catalogueEnvelope (entryId : LibraryEntryId) : LibraryEntryEnvelope =
            (BuiltinCatalogue.catalogueData ()).envelopes
            |> List.find (fun envelope -> envelope.content.id = entryId)
        let underId (itemModelId : ItemModelId) (protection : EntryProtection) : ItemModelVersion =
            {
                labRetarder with
                    entry =
                        {
                            labRetarder.entry with
                                id = itemModelId
                                protection = protection
                        }
            }

        let freshId = Guid.NewGuid() |> ItemModelId
        match store.saveItemModel (underId freshId ProtectedBuiltIn) with
        | Error (InvalidItemModelErr reason) -> Assert.Contains(freshId.value.ToString(), reason)
        | Error error -> Assert.Fail($"Expected InvalidItemModelErr, got {error.describe}")
        | Ok () -> Assert.Fail("A protected built-in must arrive through the import seam, not saveItemModel.")
        Assert.Equal<StoredEntryVersion option>(None, latestOf freshId.toLibraryEntryId)

        let userModel = underId freshId UserManaged
        Assert.Equal<Result<unit, ItemModelError>>(Ok (), store.saveItemModel userModel)
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some userModel), store.resolveItemModelVersion userModel.versionId)
        match latestOf freshId.toLibraryEntryId with
        | Some stored -> Assert.Equal(UserProvenance, stored.envelope.content.provenance)
        | None -> Assert.Fail("Expected the user item model to be stored.")

        let siliconId = ItemModelId MaterialIds.silicon.value
        match store.saveItemModel (underId siliconId UserManaged) with
        | Error (InvalidItemModelErr reason) ->
            Assert.Contains(siliconId.value.ToString(), reason)
            Assert.Contains(".material-json", reason)
        | Error error -> Assert.Fail($"Expected InvalidItemModelErr, got {error.describe}")
        | Ok () -> Assert.Fail("An id held by a material must be refused.")
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(
            Ok None, store.resolveItemModelVersion { itemModelId = siliconId; version = VersionNumber.first })
        let silicon = MaterialIds.silicon.toLibraryEntryId
        Assert.Equal(Some (catalogueEnvelope silicon), latestOf silicon |> Option.map _.envelope)

        let emptySlot = BuiltinCatalogue.itemModel ItemModelIds.emptySlotModelId
        match store.saveItemModel (underId ItemModelIds.emptySlotModelId UserManaged) with
        | Error (DuplicateItemModelErr reason) ->
            Assert.Contains(ItemModelIds.emptySlotModelId.value.ToString(), reason)
            Assert.Contains($"version {emptySlot.version.value}", reason)
        | Error error -> Assert.Fail($"Expected DuplicateItemModelErr, got {error.describe}")
        | Ok () -> Assert.Fail("An id held by a built-in item model must be refused.")
        let emptySlotEntry = ItemModelIds.emptySlotModelId.toLibraryEntryId
        Assert.Equal(Some (catalogueEnvelope emptySlotEntry), latestOf emptySlotEntry |> Option.map _.envelope)
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some emptySlot), store.resolveItemModelVersion emptySlot.versionId)

    [<Fact>]
    member _.``a database failure below the item-model store is ItemModelStoreUnavailableErr, never a throw`` () : unit =
        let database = BuiltinCatalogue.freshLibraryDatabase ()
        let store = ItemModelProxy.createInDatabase database
        (database :> IDisposable).Dispose()

        let unavailable (result : Result<'Outcome, ItemModelError>) : unit =
            match result with
            | Error (ItemModelStoreUnavailableErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"Expected ItemModelStoreUnavailableErr, got {error.describe}")
            | Ok _ -> Assert.Fail("A disposed database must answer a typed failure.")

        store.listItemModels () |> unavailable
        store.resolveItemModelVersion (BuiltinCatalogue.itemModel ItemModelIds.emptySlotModelId).versionId |> unavailable
        store.saveItemModel labRetarder |> unavailable

    interface IDisposable with
        member _.Dispose () : unit =
            for database in databases do
                (database :> IDisposable).Dispose()

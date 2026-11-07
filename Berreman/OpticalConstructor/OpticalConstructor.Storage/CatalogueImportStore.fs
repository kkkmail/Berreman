namespace OpticalConstructor.Storage

open System
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.EntryFileJson

module CatalogueImportStore =

    type CatalogueImportContext with

        /// Supplies canonical sealing and fresh entry identities at the IO boundary (spec 0050 S3.1).
        static member create (proxy : CatalogueImportProxy) : CatalogueImportContext =
            {
                proxy = proxy
                sealEntry = fun content ->
                    content.``sealed``
                    |> Result.mapError (fun error -> EntryHashingErr $"entry '{content.name.value}' ({content.id.value}): {error.describe}")
                newEntryId = fun () -> LibraryEntryId (Guid.NewGuid())
            }

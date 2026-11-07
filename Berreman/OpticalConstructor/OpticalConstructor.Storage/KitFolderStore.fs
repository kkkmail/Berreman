namespace OpticalConstructor.Storage

open System.IO
open System.Text
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.CatalogueFolderStore

module KitFolderStore =

    type KitFile with

        /// Canonical kit metadata with LF line ends and one final newline (spec 0050 S4.1).
        member this.fileText : Result<KitFileText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitFileText)

    type KitFileText with

        member private this.toJsonText : JsonText = JsonText this.value

        /// Validates the kit schema and rejects unsupported versions before binding (spec 0050 S4.1).
        member this.parseKitFile : Result<KitFile, StorageError> =
            parseVersionedText SchemaValidation.validateKitFile KitFile.extension KitFile.currentSchemaVersion this.toJsonText

    let private tryKitFolderIo (folder : KitFolder) (io : unit -> Result<'Value, KitError>) : Result<'Value, KitError> =
        try io ()
        with error -> Error (KitIoErr $"kit folder '{folder.value.value}': {error.Message}")

    let private kitFileRejectedErr (folder : KitFolder) (error : StorageError) : KitError =
        KitFileRejectedErr $"kit folder '{folder.value.value}': the file '{KitFile.fileName.value}' is rejected: {error.describe}"

    type KitProxy with

        /// Opens a folder without IO; only initialisation creates the kit and its content folders (spec 0050 S4.1).
        static member create (folder : KitFolder) : KitProxy =
            let writeMetadata (file : FileName) (kitFile : KitFile) : Result<unit, KitError> =
                kitFile.fileText
                |> Result.mapError (kitFileRejectedErr folder)
                |> Result.bind (fun (text : KitFileText) ->
                    let bytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
                    file.writeBytesAtomically bytes
                    |> Result.mapError (fun (error : StorageError) ->
                        KitIoErr $"kit folder '{folder.value.value}': the file '{file.value}' is not written: {error.describe}"))

            {
                initialiseKit =
                    fun (kitFile : KitFile) ->
                        tryKitFolderIo folder (fun () ->
                            let file = KitFile.fileName.combine folder.value
                            if File.Exists file.value then
                                Error (KitAlreadyExistsErr $"kit folder '{folder.value.value}'")
                            else
                                Directory.CreateDirectory folder.value.value |> ignore
                                for name in [ FolderName "data"; FolderName "library" ] do
                                    let contentFolder = folder.value.combine name
                                    Directory.CreateDirectory contentFolder.value |> ignore
                                // The metadata makes this a kit only after its content folders exist (spec 0050 001 §E; S4.1).
                                writeMetadata file kitFile)
                readKitFile =
                    fun () ->
                        tryKitFolderIo folder (fun () ->
                            let file = KitFile.fileName.combine folder.value
                            if File.Exists file.value then
                                let bytes = File.ReadAllBytes file.value
                                let text = KitFileText (Encoding.UTF8.GetString bytes)
                                let withoutBom =
                                    if text.value.StartsWith('\uFEFF') then KitFileText (text.value.Substring 1)
                                    else text
                                withoutBom.parseKitFile |> Result.mapError (kitFileRejectedErr folder)
                            else
                                Error (NotAKitFolderErr $"kit folder '{folder.value.value}'"))
                writeKitFile =
                    fun (kitFile : KitFile) ->
                        tryKitFolderIo folder (fun () ->
                            let file = KitFile.fileName.combine folder.value
                            if File.Exists file.value then writeMetadata file kitFile
                            else Error (NotAKitFolderErr $"kit folder '{folder.value.value}'"))
            }

namespace OpticalConstructor.Storage

open System.IO
open System.Text
open Softellect.Sys.Logging
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.CatalogueFolderStore

/// The kit history file: canonical JSON validated by its own schema on read, another version a typed error
/// (spec 0050 S5.1).
module KitHistoryStore =

    /// The text of the kit history file (spec 0050 S5.1).
    type KitHistoryText =
        | KitHistoryText of string

        member this.value : string = let (KitHistoryText text) = this in text

    type KitHistory with

        /// The history file's name, the one place it is written (spec 0050 S5.1).
        static member fileName : FileName = FileName "kit-history.json"

        /// Canonical history JSON: sorted keys, LF line ends and one final newline (spec 0050 S5.1).
        member this.fileText : Result<KitHistoryText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitHistoryText)

    type KitHistoryText with

        member private this.toJsonText : JsonText = JsonText this.value

        /// Validates the history schema and refuses another schema version before binding; there are no
        /// up-converters and no fallback readers (spec 0050 S5.1).
        member this.parseKitHistory : Result<KitHistory, StorageError> =
            parseVersionedText SchemaValidation.validateKitHistory (FileExtension ".json") KitHistory.currentSchemaVersion this.toJsonText

    type KitFileText with

        /// The history file's bytes as `FileByteContent.kitFileText` decodes them, so it has no second decoder (spec 0050 S5.1).
        member private this.toKitHistoryText : KitHistoryText = KitHistoryText this.value

    type KitHistoryProxy with

        /// The store over the per-user preferences folder: `kit-history.json` there, written atomically. Building it
        /// touches nothing, and every .NET IO exception is caught here (spec 0050 S5.1; 001 §F, §G; .spec-md §4).
        static member create (preferences : FolderName) : KitHistoryProxy =
            let file = KitHistory.fileName.combine preferences

            let fileErr (error : StorageError) : KitHistoryError =
                KitHistoryWriteErr $"the file '{file.value}': {error.describe}"

            let tryReadText () : Result<KitHistoryText, StorageError> =
                try
                    let bytes = File.ReadAllBytes file.value |> FileByteContent
                    Ok bytes.kitFileText.toKitHistoryText
                with error -> Error (FileIoExn error)

            let createPreferences () : Result<unit, KitHistoryError> =
                try
                    Directory.CreateDirectory preferences.value |> ignore
                    Ok ()
                with error -> Error (KitHistoryWriteErr $"the preferences folder '{preferences.value}': {error.Message}")

            let writeText (text : KitHistoryText) : Result<unit, KitHistoryError> =
                let bytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
                file.writeBytesAtomically bytes |> Result.mapError fileErr

            {
                readHistory =
                    fun () ->
                        match File.Exists file.value with
                        | false ->
                            Logger.logWarn $"kit history '{file.value}': the file does not exist; the history starts empty."
                            KitHistory.empty
                        | true ->
                            match tryReadText () |> Result.bind (fun (text : KitHistoryText) -> text.parseKitHistory) with
                            | Ok history -> history
                            | Error error ->
                                Logger.logWarn $"kit history '{file.value}': {error.describe}; the history starts empty."
                                KitHistory.empty
                writeHistory =
                    fun (history : KitHistory) ->
                        // Rendered before the folder is created, so a render failure leaves the disk as it was.
                        history.fileText
                        |> Result.mapError fileErr
                        |> Result.bind (fun (text : KitHistoryText) -> createPreferences () |> Result.bind (fun () -> writeText text))
                probeFolder =
                    fun (folder : KitFolder) ->
                        match Directory.Exists folder.value.value with
                        | true -> KitFolderPresent
                        | false -> KitFolderMissing
            }

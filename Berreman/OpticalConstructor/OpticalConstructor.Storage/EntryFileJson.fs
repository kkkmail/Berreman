namespace OpticalConstructor.Storage

open System.Text.Json
open System.Text.Json.Nodes
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.EntryTextDiff
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson

/// One library entry per file, and one manifest per catalogue folder: written as indented canonical JSON, read
/// back through the schema and the schema version; an entry carries its content hash beside the content's keys
/// and is read back through it too (spec 0050 001 §A, §B, .spec-md §5).
module EntryFileJson =

    /// A throwing JSON read as a value: its failure is `MalformedJsonErr` carrying the message.
    let private tryRead (read : unit -> 'Value) : Result<'Value, StorageError> =
        try Ok (read ())
        with e -> Error (MalformedJsonErr e.Message)

    /// There are no up-converters and no fallback readers: only the current schema version is read (spec 0050
    /// 001 §B).
    let private checkSchemaVersion (extension : FileExtension) (expected : FileSchemaVersion) (found : FileSchemaVersion) : Result<unit, StorageError> =
        if found = expected then Ok ()
        else Error (UnsupportedSchemaVersionErr { fileExtension = extension; found = found; expected = expected })

    /// A library file's text, UTF-8 without a BOM (spec 0050 001 §B). S2's database payload and S4's kit
    /// snapshot hold it byte for byte.
    type LibraryFileText =
        | LibraryFileText of string

        member this.value : string = let (LibraryFileText t) = this in t

        member private this.toJsonText : JsonText = JsonText this.value

        /// `read` over the root of the document this text parses to (`MalformedJsonErr` when it does not).
        /// The document ends with the call, so `read` keeps nothing of the root.
        member private this.readRoot<'Value> (read : JsonElement -> Result<'Value, StorageError>) : Result<'Value, StorageError> =
            match tryRead (fun () -> JsonDocument.Parse this.value) with
            | Error e -> Error e
            | Ok doc ->
                use doc = doc
                read doc.RootElement

        /// The entry this file holds, checked in order (spec 0050 001 §B): the text parses (`MalformedJsonErr`);
        /// it is a `kind` file of its schema (`SchemaValidationErr`); its schema version is the kind's current
        /// one, judged before anything binds (`UnsupportedSchemaVersionErr`); the content binds without its hash
        /// (`MalformedJsonErr`); and the recorded hash is the content's (`ContentHashMismatchErr`).
        member this.parseEntry (kind : LibraryEntryKind) : Result<LibraryEntryEnvelope, StorageError> =
            this.readRoot (fun (root : JsonElement) ->
                let hashKey = nameof Unchecked.defaultof<LibraryEntryEnvelope>.contentHash

                let bindContent () : LibraryEntryContent =
                    let body = JsonObject.Create root
                    body.Remove hashKey |> ignore
                    body.Deserialize<LibraryEntryContent> options

                let checkHash (recorded : ContentHash) (content : LibraryEntryContent) : Result<LibraryEntryEnvelope, StorageError> =
                    contentHashOf content
                    |> Result.bind (fun computed ->
                        if computed = recorded then Ok { content = content; contentHash = recorded }
                        else Error (ContentHashMismatchErr { entryId = content.id; recorded = recorded; computed = computed }))

                SchemaValidation.validateEntry kind root
                |> Result.bind (fun () -> tryRead (fun () -> root.GetProperty("schema").GetProperty("version").Deserialize<FileSchemaVersion> options))
                |> Result.bind (checkSchemaVersion kind.extension kind.currentSchemaVersion)
                |> Result.bind (fun () -> tryRead (fun () -> root.GetProperty(hashKey).Deserialize<ContentHash> options))
                |> Result.bind (fun recorded -> tryRead bindContent |> Result.bind (checkHash recorded)))

        /// The catalogue manifest this file holds, checked in order (spec 0050 001 §A, §B): the text parses
        /// (`MalformedJsonErr`); it is a manifest of its schema (`SchemaValidationErr`); its schema version is the
        /// current one, judged before anything binds (`UnsupportedSchemaVersionErr`); and it binds
        /// (`MalformedJsonErr`).
        member this.parseCatalogueManifest : Result<CatalogueManifest, StorageError> =
            parseVersionedText SchemaValidation.validateCatalogueManifest CatalogueManifest.extension CatalogueManifest.currentSchemaVersion this.toJsonText

    type LibraryEntryContent with

        /// The content with the hash over exactly it (spec 0050 001 §B). `sealed` is reserved by F#, hence the
        /// backticks.
        member this.``sealed`` : Result<LibraryEntryEnvelope, StorageError> =
            contentHashOf this |> Result.map (fun hash -> { content = this; contentHash = hash })

    type LibraryEntryEnvelope with

        /// The entry's file: the content's canonical node with `contentHash` beside its keys, indented and
        /// ending with one `\n` (spec 0050 001 §B / S4.4). The
        /// file is flat, so the hash's key is the envelope's field name.
        member this.fileText : Result<LibraryFileText, StorageError> =
            match nodeOf this.content, nodeOf this.contentHash with
            | Error e, _
            | _, Error e -> Error e
            | Ok contentNode, Ok hashNode ->
                match contentNode with
                | :? JsonObject as entry ->
                    entry[nameof this.contentHash] <- hashNode
                    entry.canonicalFileText |> LibraryFileText |> Ok
                | _ -> Error (MalformedJsonErr "an entry's content is not a JSON object")

        /// Compares the held file with this incoming file, preserving rendering failures (spec 0050 S3.2).
        member this.diffFrom (held : LibraryEntryEnvelope) : Result<EntryDiffLine list, StorageError> =
            let linesOf (file : LibraryFileText) : EntryTextLine list =
                let reversed = file.value.Split '\n' |> Array.toList |> List.rev
                let withoutFinalSegment =
                    match reversed with
                    | "" :: rest -> rest
                    | rest -> rest
                withoutFinalSegment |> List.rev |> List.map EntryTextLine

            held.fileText
            |> Result.bind (fun heldText ->
                this.fileText
                |> Result.map (fun incomingText -> EntryDiffLine.diffLines (linesOf heldText) (linesOf incomingText)))

    type CatalogueManifest with

        /// The catalogue folder's `.catalogue-json`: the manifest's canonical node, indented and ending with one
        /// `\n` (spec 0050 001 §A, §B).
        member this.fileText : Result<LibraryFileText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> LibraryFileText)

        /// The hash S3's start-up import compares with the one it recorded (spec 0050 001 §B "Import on every
        /// start"). It is computed, never stored in the manifest.
        member this.manifestHash : Result<ContentHash, StorageError> = contentHashOf this

namespace OpticalConstructor.Storage

open System.Text.Json
open System.Text.Json.Nodes
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson

/// One library entry per file: written as indented canonical JSON with its content hash beside the content's
/// keys, read back through the kind's schema, the schema version and the hash (spec 0050 001 §B, .spec-md §5).
module EntryFileJson =

    /// A library file's text, UTF-8 without a BOM (spec 0050 001 §B). S2's database payload and S4's kit
    /// snapshot hold it byte for byte.
    type LibraryFileText =
        | LibraryFileText of string

        member this.value : string = let (LibraryFileText t) = this in t

        /// The entry this file holds, checked in order (spec 0050 001 §B): the text parses (`MalformedJsonErr`);
        /// it is a `kind` file of its schema (`SchemaValidationErr`); its schema version is the kind's current
        /// one, judged before anything binds (`UnsupportedSchemaVersionErr`); the content binds without its hash
        /// (`MalformedJsonErr`); and the recorded hash is the content's (`ContentHashMismatchErr`). There are no
        /// up-converters and no fallback readers.
        member this.parseEntry (kind : LibraryEntryKind) : Result<LibraryEntryEnvelope, StorageError> =
            let parsed =
                try Ok (JsonDocument.Parse this.value)
                with e -> Error (MalformedJsonErr e.Message)

            match parsed with
            | Error e -> Error e
            | Ok doc ->
                use doc = doc
                let root = doc.RootElement
                let hashKey = nameof Unchecked.defaultof<LibraryEntryEnvelope>.contentHash

                let bind (read : unit -> 'Value) : Result<'Value, StorageError> =
                    try Ok (read ())
                    with e -> Error (MalformedJsonErr e.Message)

                let checkVersion (found : FileSchemaVersion) : Result<unit, StorageError> =
                    if found = kind.currentSchemaVersion then Ok ()
                    else Error (UnsupportedSchemaVersionErr { fileExtension = kind.extension; found = found; expected = kind.currentSchemaVersion })

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
                |> Result.bind (fun () -> bind (fun () -> root.GetProperty("schema").GetProperty("version").Deserialize<FileSchemaVersion> options))
                |> Result.bind checkVersion
                |> Result.bind (fun () -> bind (fun () -> root.GetProperty(hashKey).Deserialize<ContentHash> options))
                |> Result.bind (fun recorded -> bind bindContent |> Result.bind (checkHash recorded))

    type LibraryEntryContent with

        /// The content with the hash over exactly it (spec 0050 001 §B). `sealed` is reserved by F#, hence the
        /// backticks.
        member this.``sealed`` : Result<LibraryEntryEnvelope, StorageError> =
            contentHashOf this |> Result.map (fun hash -> { content = this; contentHash = hash })

    type LibraryEntryEnvelope with

        /// The entry's file: the content's canonical node with `contentHash` beside its keys, indented and
        /// ending with one `\n` (spec 0050 001 §B), as `ProjectJson.serializeProject` adds `schemaVersion`. The
        /// file is flat, so the hash's key is the envelope's field name.
        member this.fileText : Result<LibraryFileText, StorageError> =
            match nodeOf this.content, nodeOf this.contentHash with
            | Error e, _
            | _, Error e -> Error e
            | Ok contentNode, Ok hashNode ->
                match contentNode with
                | :? JsonObject as entry ->
                    entry[nameof this.contentHash] <- hashNode
                    Ok (LibraryFileText $"{entry.indentedCanonicalText}\n")
                | _ -> Error (MalformedJsonErr "an entry's content is not a JSON object")

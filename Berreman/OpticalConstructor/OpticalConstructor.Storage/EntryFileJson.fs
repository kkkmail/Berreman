namespace OpticalConstructor.Storage

open System.Text.Json
open System.Text.Json.Nodes
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.ItemModels
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

    /// The noise axes a detector item model stores, the only place an entry holds a `ParameterKind` (spec 0050 S7.1):
    /// an item-model payload's `Fields[0]` is its form, and a detector form's own `Fields[0]` is its axis list.
    let private detectorAxisNodes (entry : JsonObject) : JsonObject list =
        // The canonical stack writes a union case with fields as { "Case": <name>, "Fields": [ ... ] }.
        let firstFieldOf (caseNames : string list) (node : JsonNode) : JsonNode option =
            match node with
            | :? JsonObject as union ->
                match union["Case"], union["Fields"] with
                | (:? JsonValue as case), (:? JsonArray as fields) when fields.Count > 0 ->
                    match case.TryGetValue<string>() with
                    | true, name when List.contains name caseNames -> Some (fields[0])
                    | _ -> None
                | _ -> None
            | _ -> None

        entry[nameof Unchecked.defaultof<LibraryEntryContent>.payload]
        |> Option.ofObj
        |> Option.bind (firstFieldOf [ nameof ItemModelPayload ])
        |> Option.bind (firstFieldOf [ nameof IntensityDetectorModel; nameof MuellerPolarimeterModel; nameof EllipsometerModel ])
        |> Option.map (fun axes ->
            match axes with
            | :? JsonArray as list -> list |> Seq.choose (fun axis -> match axis with :? JsonObject as o -> Some o | _ -> None) |> List.ofSeq
            | _ -> [])
        |> Option.defaultValue []

    /// Writes each detector noise axis's `ParameterKind` as its int `value`, a closed set's wire tag (AGENTS.md; spec
    /// 0050 S7.1), in place of the case name the canonical stack writes.
    let private writeParameterKindTags (entry : JsonObject) : Result<unit, StorageError> =
        let kindKey = nameof Unchecked.defaultof<ItemParameterAxis>.kind
        tryRead (fun () ->
            for axis in detectorAxisNodes entry do
                let kind = axis[kindKey].Deserialize<ParameterKind> options
                axis[kindKey] <- JsonValue.Create kind.value)

    /// Reads each detector noise axis's int tag through `ParameterKind.tryOfWire` back to the node the canonical stack
    /// binds; any other tag is `MalformedJsonErr`, here at the file seam (spec 0050 S7.1).
    let private readParameterKindTags (entry : JsonObject) : Result<unit, StorageError> =
        let kindKey = nameof Unchecked.defaultof<ItemParameterAxis>.kind
        detectorAxisNodes entry
        |> List.fold (fun result axis ->
            result |> Result.bind (fun () ->
                let tag = axis[kindKey]
                let kindOpt =
                    match tag with
                    | :? JsonValue as value ->
                        match value.TryGetValue<int>() with
                        | true, wire -> ParameterKind.tryOfWire wire
                        | false, _ -> None
                    | _ -> None
                match kindOpt with
                | Some kind -> nodeOf kind |> Result.map (fun node -> axis[kindKey] <- node)
                | None -> Error (MalformedJsonErr $"'{tag}' is not a {nameof ParameterKind} tag"))) (Ok ())

    type LibraryEntryContent with

        /// The content as its entry file holds it, which is exactly what the hash covers: the canonical node, with
        /// each detector noise axis's `ParameterKind` as its int tag (spec 0050 001 §B; S7.1).
        member private this.fileNode : Result<JsonObject, StorageError> =
            nodeOf this
            |> Result.bind (fun node ->
                match node with
                | :? JsonObject as entry -> writeParameterKindTags entry |> Result.map (fun () -> entry)
                | _ -> Error (MalformedJsonErr "an entry's content is not a JSON object"))

        /// The hash over exactly `fileNode`, the one hash `sealed` records and `parseEntry` checks (spec 0050 001 §B).
        member private this.fileHash : Result<ContentHash, StorageError> = this.fileNode |> Result.bind contentHashOf

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
        /// one, judged before anything binds (`UnsupportedSchemaVersionErr`); the content binds without its hash,
        /// every `ParameterKind` tag included (`MalformedJsonErr`, S7.1); and the recorded hash is the content's
        /// (`ContentHashMismatchErr`).
        member this.parseEntry (kind : LibraryEntryKind) : Result<LibraryEntryEnvelope, StorageError> =
            this.readRoot (fun (root : JsonElement) ->
                let hashKey = nameof Unchecked.defaultof<LibraryEntryEnvelope>.contentHash

                let bindContent () : Result<LibraryEntryContent, StorageError> =
                    tryRead (fun () ->
                        let body = JsonObject.Create root
                        body.Remove hashKey |> ignore
                        body)
                    |> Result.bind (fun body ->
                        readParameterKindTags body
                        |> Result.bind (fun () -> tryRead (fun () -> body.Deserialize<LibraryEntryContent> options)))

                let checkHash (recorded : ContentHash) (content : LibraryEntryContent) : Result<LibraryEntryEnvelope, StorageError> =
                    content.fileHash
                    |> Result.bind (fun computed ->
                        if computed = recorded then Ok { content = content; contentHash = recorded }
                        else Error (ContentHashMismatchErr { entryId = content.id; recorded = recorded; computed = computed }))

                SchemaValidation.validateEntry kind root
                |> Result.bind (fun () -> tryRead (fun () -> root.GetProperty("schema").GetProperty("version").Deserialize<FileSchemaVersion> options))
                |> Result.bind (checkSchemaVersion kind.extension kind.currentSchemaVersion)
                |> Result.bind (fun () -> tryRead (fun () -> root.GetProperty(hashKey).Deserialize<ContentHash> options))
                |> Result.bind (fun recorded -> bindContent () |> Result.bind (checkHash recorded)))

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
            this.fileHash |> Result.map (fun hash -> { content = this; contentHash = hash })

    type LibraryEntryEnvelope with

        /// The entry's file: the content's file node with `contentHash` beside its keys, indented and
        /// ending with one `\n` (spec 0050 001 §B / S4.4). The
        /// file is flat, so the hash's key is the envelope's field name.
        member this.fileText : Result<LibraryFileText, StorageError> =
            match this.content.fileNode, nodeOf this.contentHash with
            | Error e, _
            | _, Error e -> Error e
            | Ok entry, Ok hashNode ->
                entry[nameof this.contentHash] <- hashNode
                entry.canonicalFileText |> LibraryFileText |> Ok

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

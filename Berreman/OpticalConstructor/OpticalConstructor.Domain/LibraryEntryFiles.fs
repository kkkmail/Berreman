namespace OpticalConstructor.Domain

open System
open Berreman.Fields
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

/// The identities, provenance, name rule and content of the library-entry files, with the builders
/// and decoders that join the content to the domain values (spec 0050 001 §A, §B; .spec-md §3.11, §5).
module LibraryEntryFiles =

    /// The schema version a stored entry file declares (spec 0050 001 §B: a differing version is a
    /// typed error, never up-converted).
    type FileSchemaVersion =
        | FileSchemaVersion of int

        member this.value : int = let (FileSchemaVersion v) = this in v

    /// What a library-entry file holds; its wire tag is an int and its file extension says the kind
    /// (spec 0050 001 §A). S7.1 adds the item-model kind.
    type LibraryEntryKind =
        | MaterialEntryKind
        | SampleEntryKind
        | SourceEntryKind
        | PolarizerEntryKind
        | DetectorEntryKind
        | CategoryEntryKind

        member this.value : int =
            match this with
            | MaterialEntryKind -> 1
            | SampleEntryKind -> 2
            | SourceEntryKind -> 3
            | PolarizerEntryKind -> 4
            | DetectorEntryKind -> 5
            | CategoryEntryKind -> 6

        static member tryOfWire (v : int) : LibraryEntryKind option =
            match v with
            | 1 -> Some MaterialEntryKind
            | 2 -> Some SampleEntryKind
            | 3 -> Some SourceEntryKind
            | 4 -> Some PolarizerEntryKind
            | 5 -> Some DetectorEntryKind
            | 6 -> Some CategoryEntryKind
            | _ -> None

        /// The kind's file extension (spec 0050 001 §A "Library extensions").
        member this.extension : FileExtension =
            match this with
            | MaterialEntryKind -> FileExtension ".material-json"
            | SampleEntryKind -> FileExtension ".sample-json"
            | SourceEntryKind -> FileExtension ".source-json"
            | PolarizerEntryKind -> FileExtension ".polarizer-json"
            | DetectorEntryKind -> FileExtension ".detector-json"
            | CategoryEntryKind -> FileExtension ".category-json"

        /// The only schema version of the kind this app reads and writes (spec 0050 001 §B).
        member this.currentSchemaVersion : FileSchemaVersion =
            match this with
            | MaterialEntryKind
            | SampleEntryKind
            | SourceEntryKind
            | PolarizerEntryKind
            | DetectorEntryKind
            | CategoryEntryKind -> FileSchemaVersion 1

        /// Every kind, in wire-tag order.
        static member all : LibraryEntryKind list =
            [ MaterialEntryKind; SampleEntryKind; SourceEntryKind; PolarizerEntryKind; DetectorEntryKind; CategoryEntryKind ]

    /// An entry file's `schema`: its kind and the schema version it declares (spec 0050 001 §B).
    type EntrySchema =
        {
            kind : LibraryEntryKind
            version : FileSchemaVersion
        }

    /// An entry's frozen GUID, whatever its kind (spec 0050 001 §B "Identity is GUID + version +
    /// hash"). Only the file seam builds a kind's own id from it (ELEV006).
    type LibraryEntryId =
        | LibraryEntryId of Guid

        member this.value : Guid = let (LibraryEntryId g) = this in g

    type MaterialId with

        /// The material's entry id: the same GUID (spec 0050 001 §B).
        member this.toLibraryEntryId : LibraryEntryId = LibraryEntryId this.value

    type SampleId with

        /// The sample's entry id: the same GUID (spec 0050 001 §B).
        member this.toLibraryEntryId : LibraryEntryId = LibraryEntryId this.value

    type CategoryId with

        /// The category's entry id: the same GUID (spec 0050 001 §B).
        member this.toLibraryEntryId : LibraryEntryId = LibraryEntryId this.value

    type ElementValueId with

        /// A preset's entry id, or `None` for text that is not a GUID's canonical (lower-case `D`)
        /// string. Every seeded and minted preset id is one (spec 0050 §3.3), and only that text
        /// decodes back unchanged from an entry file.
        member this.tryToLibraryEntryId : LibraryEntryId option =
            match Guid.TryParseExact(this.value, "D") with
            | true, g when string g = this.value -> Some (LibraryEntryId g)
            | _ -> None

    /// A catalogue's GUID, carried by its `.catalogue-json` (spec 0050 001 §A, §5).
    type CatalogueId =
        | CatalogueId of Guid

        member this.value : Guid = let (CatalogueId g) = this in g

    /// A catalogue's name, carried by its `.catalogue-json` (spec 0050 001 §A).
    type CatalogueName =
        | CatalogueName of string

        member this.value : string = let (CatalogueName s) = this in s

    /// The GUID a kit's `.kit-json` carries (spec 0050 001 §E); S4.1's `Kit` module reuses it.
    type KitId =
        | KitId of Guid

        member this.value : Guid = let (KitId g) = this in g

    /// The online source an entry was pulled from (spec 0050 001 §B, §D).
    type MaterialSourceName =
        | MaterialSourceName of string

        member this.value : string = let (MaterialSourceName s) = this in s

    /// The record's own id in the source it was pulled from (spec 0050 001 §B).
    type MaterialSourceRecordId =
        | MaterialSourceRecordId of string

        member this.value : string = let (MaterialSourceRecordId s) = this in s

    /// The source's version of the pulled record (spec 0050 001 §B).
    type MaterialSourceVersion =
        | MaterialSourceVersion of string

        member this.value : string = let (MaterialSourceVersion s) = this in s

    /// An entry's licence as an SPDX identifier, e.g. `CC0-1.0` or `CC-BY-4.0` (spec 0050 001 §B).
    type EntryLicence =
        | EntryLicence of string

        member this.value : string = let (EntryLicence s) = this in s

    /// The citation a pulled entry's licence asks to be shown (spec 0050 001 §B "Licence attribution").
    type EntryCitation =
        | EntryCitation of string

        member this.value : string = let (EntryCitation s) = this in s

    /// Where a pulled entry came from (spec 0050 001 §B `provenance`).
    type SourcePullInfo =
        {
            source : MaterialSourceName
            record : MaterialSourceRecordId
            sourceVersion : MaterialSourceVersion
            licence : EntryLicence
            citation : EntryCitation
        }

    /// An entry file's `provenance` (spec 0050 001 §B); a built-in's protection derives from it.
    type EntryProvenance =
        | BuiltInProvenance of CatalogueId
        | UserProvenance
        | KitImportProvenance of KitId
        | SourcePullProvenance of SourcePullInfo

        member this.protection : EntryProtection =
            match this with
            | BuiltInProvenance _ -> ProtectedBuiltIn
            | UserProvenance
            | KitImportProvenance _
            | SourcePullProvenance _ -> UserManaged

        member this.categoryOrigin : CategoryOrigin =
            match this with
            | BuiltInProvenance _ -> BuiltInCategory
            | UserProvenance
            | KitImportProvenance _
            | SourcePullProvenance _ -> UserCategory

    /// The lowercase hex SHA-256 of an entry's canonical content (spec 0050 001 §B); Storage computes it.
    type ContentHash =
        | ContentHash of string

        member this.value : string = let (ContentHash s) = this in s

    /// A file name without its kind extension (spec 0050 001 §A; .spec-md §3.11).
    type FileStem =
        | FileStem of string

        member this.value : string = let (FileStem s) = this in s

        /// The text with only the characters Windows forbids in a file name, separators included,
        /// replaced by `_` (spec 0050 .spec-md §3.11).
        static member ofText (text : string) : FileStem =
            text
            |> String.map (fun c ->
                match c with
                | ':' | '*' | '?' | '"' | '<' | '>' | '|' | '/' | '\\' -> '_'
                | other -> other)
            |> FileStem

        member this.withExtension (extension : FileExtension) : FileName = FileName $"{this.value}{extension.value}"

    type EntryName with

        /// The stem of the entry's file: its name, made filesystem-safe (spec 0050 001 §A).
        member this.fileStem : FileStem = FileStem.ofText this.value

    /// A sample entry's `payload`; its id, name and description belong to the envelope (spec 0050 001 §B).
    type SamplePayloadData =
        {
            structure : SampleStructure
            substrate : SubstrateKind
            supportedEmission : Emission
        }

    /// A light-source entry's `payload`; its protection derives from the provenance (spec 0050 001 §B).
    type SourcePayloadData =
        {
            waveLength : WaveLength
            tuning : SourceTuning
        }

    /// A polarizer entry's `payload`; its protection derives from the provenance (spec 0050 001 §B).
    type PolarizerPayloadData =
        {
            behavior : PolarizerBehavior
            category : PolarizerCategory
        }

    /// An entry file's `payload`: the domain value without what the envelope carries (spec 0050 001 §B).
    /// Facets derive from it (`LibraryFacets`) and are never stored.
    type LibraryEntryPayload =
        | MaterialPayload of MaterialComplexity
        | SamplePayload of SamplePayloadData
        | SourcePayload of SourcePayloadData
        | PolarizerPayload of PolarizerPayloadData
        | DetectorPayload of DetectorKind
        | CategoryPayload of CategoryVisibility

        member this.kind : LibraryEntryKind =
            match this with
            | MaterialPayload _ -> MaterialEntryKind
            | SamplePayload _ -> SampleEntryKind
            | SourcePayload _ -> SourceEntryKind
            | PolarizerPayload _ -> PolarizerEntryKind
            | DetectorPayload _ -> DetectorEntryKind
            | CategoryPayload _ -> CategoryEntryKind

        /// The schema an entry holding this payload is written under: its kind at the kind's only version.
        member this.currentSchema : EntrySchema = { kind = this.kind; version = this.kind.currentSchemaVersion }

    /// Why a domain value cannot be filed as entry content, or the content cannot be read back as one
    /// (spec 0050 001 §B). Each reason names the entry.
    type LibraryEntryError =
        | EntryKindMismatchErr of reason : string
        | EntryIdNotGuidErr of reason : string
        | ProvenanceMismatchErr of reason : string
        | MissingEntryFieldErr of reason : string

    type CategoryName with

        /// The name a category's entry file is filed under (spec 0050 001 §B).
        member this.toEntryName : EntryName = EntryName this.value

    /// An entry file without its `contentHash`: exactly what the hash covers (spec 0050 001 §B).
    /// `categoryOpt` holds a material's category and is `None` for every other kind; a preset's
    /// protection and a category's origin are not stored, because they derive from the provenance.
    type LibraryEntryContent =
        {
            schema : EntrySchema
            id : LibraryEntryId
            version : VersionNumber
            name : EntryName
            categoryOpt : CategoryId option
            descriptionOpt : EntryDescription option
            payload : LibraryEntryPayload
            provenance : EntryProvenance
        }

        /// The entry's file name: its name made safe plus its kind's extension (spec 0050 001 §A).
        member this.fileName : FileName = this.name.fileStem.withExtension this.schema.kind.extension

        static member private ofPayload (version : VersionNumber) (provenance : EntryProvenance) (id : LibraryEntryId) (name : EntryName) (payload : LibraryEntryPayload) : LibraryEntryContent =
            {
                schema = payload.currentSchema
                id = id
                version = version
                name = name
                categoryOpt = None
                descriptionOpt = None
                payload = payload
                provenance = provenance
            }

        /// A preset's entry id; a preset is filed only under a provenance that agrees with its protection.
        static member private tryPresetEntryId (provenance : EntryProvenance) (presetId : ElementValueId) (name : EntryName) (protection : EntryProtection) : Result<LibraryEntryId, LibraryEntryError> =
            match presetId.tryToLibraryEntryId with
            | None -> Error (EntryIdNotGuidErr $"preset '{name.value}': its id '{presetId.value}' is not a canonical GUID")
            | Some id when protection = provenance.protection -> Ok id
            | Some _ -> Error (ProvenanceMismatchErr $"preset '{name.value}' ({presetId.value}): its protection disagrees with the provenance it is filed under")

        static member ofMaterialEntry (version : VersionNumber) (provenance : EntryProvenance) (entry : MaterialEntry) : LibraryEntryContent =
            { LibraryEntryContent.ofPayload version provenance entry.id.toLibraryEntryId entry.name (MaterialPayload entry.complexity) with
                categoryOpt = Some entry.category
                descriptionOpt = entry.description }

        static member ofSample (version : VersionNumber) (provenance : EntryProvenance) (sample : Sample) : LibraryEntryContent =
            let payload : SamplePayloadData = { structure = sample.structure; substrate = sample.substrate; supportedEmission = sample.supportedEmission }
            { LibraryEntryContent.ofPayload version provenance sample.id.toLibraryEntryId sample.name (SamplePayload payload) with
                descriptionOpt = Some sample.description }

        static member ofSourcePreset (version : VersionNumber) (provenance : EntryProvenance) (preset : SourcePreset) : Result<LibraryEntryContent, LibraryEntryError> =
            let payload : SourcePayloadData = { waveLength = preset.waveLength; tuning = preset.tuning }
            LibraryEntryContent.tryPresetEntryId provenance preset.id preset.name preset.protection
            |> Result.map (fun id -> LibraryEntryContent.ofPayload version provenance id preset.name (SourcePayload payload))

        static member ofDetectorPreset (version : VersionNumber) (provenance : EntryProvenance) (preset : DetectorPreset) : Result<LibraryEntryContent, LibraryEntryError> =
            LibraryEntryContent.tryPresetEntryId provenance preset.id preset.name preset.protection
            |> Result.map (fun id -> LibraryEntryContent.ofPayload version provenance id preset.name (DetectorPayload preset.kind))

        static member ofPolarizerPreset (version : VersionNumber) (provenance : EntryProvenance) (preset : PolarizerPreset) : Result<LibraryEntryContent, LibraryEntryError> =
            let payload : PolarizerPayloadData = { behavior = preset.behavior; category = preset.category }
            LibraryEntryContent.tryPresetEntryId provenance preset.id preset.name preset.protection
            |> Result.map (fun id -> LibraryEntryContent.ofPayload version provenance id preset.name (PolarizerPayload payload))

        /// A category is filed under its name as the entry name, and only under a provenance that agrees
        /// with its origin.
        static member ofMaterialCategory (version : VersionNumber) (provenance : EntryProvenance) (category : MaterialCategory) : Result<LibraryEntryContent, LibraryEntryError> =
            if category.origin = provenance.categoryOrigin then
                Ok (LibraryEntryContent.ofPayload version provenance category.id.toLibraryEntryId category.name.toEntryName (CategoryPayload category.visibility))
            else
                Error (ProvenanceMismatchErr $"category '{category.name.value}' ({category.id.value}): its origin disagrees with the provenance it is filed under")

        member private this.kindMismatchErr (expected : LibraryEntryKind) : LibraryEntryError =
            EntryKindMismatchErr $"entry '{this.name.value}' ({this.id.value}) holds a {this.payload.kind.extension.value} payload, not a {expected.extension.value} one"

        /// A preset's id: the entry GUID's canonical string (spec 0050 §3.3).
        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member private this.presetId : ElementValueId = ElementValueId (string this.id.value)

        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member this.toMaterialEntry : Result<MaterialEntry, LibraryEntryError> =
            match this.payload, this.categoryOpt with
            | MaterialPayload complexity, Some category ->
                let entry : MaterialEntry =
                    {
                        id = MaterialId this.id.value
                        name = this.name
                        category = category
                        description = this.descriptionOpt
                        complexity = complexity
                    }
                Ok entry
            | MaterialPayload _, None -> Error (MissingEntryFieldErr $"material '{this.name.value}' ({this.id.value}) has no category")
            | _ -> Error (this.kindMismatchErr MaterialEntryKind)

        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member this.toSample : Result<Sample, LibraryEntryError> =
            match this.payload, this.descriptionOpt with
            | SamplePayload payload, Some description ->
                let sample : Sample =
                    {
                        id = SampleId this.id.value
                        name = this.name
                        structure = payload.structure
                        substrate = payload.substrate
                        description = description
                        supportedEmission = payload.supportedEmission
                    }
                Ok sample
            | SamplePayload _, None -> Error (MissingEntryFieldErr $"sample '{this.name.value}' ({this.id.value}) has no description")
            | _ -> Error (this.kindMismatchErr SampleEntryKind)

        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member this.toSourcePreset : Result<SourcePreset, LibraryEntryError> =
            match this.payload with
            | SourcePayload payload ->
                let preset : SourcePreset =
                    {
                        id = this.presetId
                        name = this.name
                        waveLength = payload.waveLength
                        tuning = payload.tuning
                        protection = this.provenance.protection
                    }
                Ok preset
            | _ -> Error (this.kindMismatchErr SourceEntryKind)

        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member this.toDetectorPreset : Result<DetectorPreset, LibraryEntryError> =
            match this.payload with
            | DetectorPayload kind ->
                let preset : DetectorPreset = { id = this.presetId; name = this.name; kind = kind; protection = this.provenance.protection }
                Ok preset
            | _ -> Error (this.kindMismatchErr DetectorEntryKind)

        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member this.toPolarizerPreset : Result<PolarizerPreset, LibraryEntryError> =
            match this.payload with
            | PolarizerPayload payload ->
                let preset : PolarizerPreset =
                    {
                        id = this.presetId
                        name = this.name
                        behavior = payload.behavior
                        category = payload.category
                        protection = this.provenance.protection
                    }
                Ok preset
            | _ -> Error (this.kindMismatchErr PolarizerEntryKind)

        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member this.toMaterialCategory : Result<MaterialCategory, LibraryEntryError> =
            match this.payload with
            | CategoryPayload visibility ->
                let category : MaterialCategory =
                    {
                        id = CategoryId this.id.value
                        name = CategoryName this.name.value
                        visibility = visibility
                        origin = this.provenance.categoryOrigin
                    }
                Ok category
            | _ -> Error (this.kindMismatchErr CategoryEntryKind)

    /// An entry file: its content and the hash over exactly that content (spec 0050 001 §B). The file
    /// itself is flat: step 30 writes the content's keys and `contentHash` side by side.
    type LibraryEntryEnvelope =
        {
            content : LibraryEntryContent
            contentHash : ContentHash
        }

namespace OpticalConstructor.Domain

open System
open System.Security.Cryptography
open Berreman.Fields
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels

/// The identities, provenance, name rule and content of the library-entry files, with the builders
/// and decoders that join the content to the domain values (spec 0050 001 §A, §B; .spec-md §3.11, §5).
module LibraryEntryFiles =

    /// The schema version a stored entry file declares (spec 0050 001 §B: a differing version is a
    /// typed error, never up-converted).
    type FileSchemaVersion =
        | FileSchemaVersion of int

        member this.value : int = let (FileSchemaVersion v) = this in v

    /// What a library-entry file holds; its wire tag is an int and its file extension says the kind
    /// (spec 0050 001 §A). S7.1 added the item-model kind, tag 7.
    type LibraryEntryKind =
        | MaterialEntryKind
        | SampleEntryKind
        | SourceEntryKind
        | PolarizerEntryKind
        | DetectorEntryKind
        | CategoryEntryKind
        | ItemModelEntryKind

        member this.value : int =
            match this with
            | MaterialEntryKind -> 1
            | SampleEntryKind -> 2
            | SourceEntryKind -> 3
            | PolarizerEntryKind -> 4
            | DetectorEntryKind -> 5
            | CategoryEntryKind -> 6
            | ItemModelEntryKind -> 7

        static member tryOfWire (v : int) : LibraryEntryKind option =
            match v with
            | 1 -> Some MaterialEntryKind
            | 2 -> Some SampleEntryKind
            | 3 -> Some SourceEntryKind
            | 4 -> Some PolarizerEntryKind
            | 5 -> Some DetectorEntryKind
            | 6 -> Some CategoryEntryKind
            | 7 -> Some ItemModelEntryKind
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
            | ItemModelEntryKind -> FileExtension ".item-model-json"

        /// The only schema version of the kind this app reads and writes (spec 0050 001 §B).
        member this.currentSchemaVersion : FileSchemaVersion =
            match this with
            | MaterialEntryKind
            | SampleEntryKind
            | SourceEntryKind
            | PolarizerEntryKind
            | DetectorEntryKind
            | CategoryEntryKind
            | ItemModelEntryKind -> FileSchemaVersion 1

        /// Every kind, in wire-tag order: seven, since S7.1 added the item-model kind (spec 0050 001 §A).
        static member all : LibraryEntryKind list =
            [ MaterialEntryKind; SampleEntryKind; SourceEntryKind; PolarizerEntryKind; DetectorEntryKind; CategoryEntryKind; ItemModelEntryKind ]

        /// The entry kind named by the file's extension, ignoring case, or none for other files (spec 0050 001 §A).
        static member tryOfFileName (file : FileName) : LibraryEntryKind option =
            match file.tryGetExtension () with
            | Some extension ->
                LibraryEntryKind.all
                |> List.tryFind (fun kind -> String.Equals(kind.extension.value, extension.value, StringComparison.OrdinalIgnoreCase))
            | None -> None

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

    type ItemModelId with

        /// The item model's entry id: the same GUID (spec 0050 S7.1).
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

    /// An entry file's `provenance` (spec 0050 001 §B); a built-in's protection derives from it.
    /// A fork preserves its ancestry but belongs to the user (spec 0050 S3.1); so does a fitted entry (S11.5).
    type EntryProvenance =
        | BuiltInProvenance of CatalogueId
        | UserProvenance
        | KitImportProvenance of KitId
        | SourcePullProvenance of SourcePullInfo
        | ForkProvenance of EntryForkInfo
        | FitProvenance of EntryFitInfo

        member this.protection : EntryProtection =
            match this with
            | BuiltInProvenance _ -> ProtectedBuiltIn
            | UserProvenance
            | KitImportProvenance _
            | SourcePullProvenance _
            | ForkProvenance _
            | FitProvenance _ -> UserManaged

        member this.categoryOrigin : CategoryOrigin =
            match this with
            | BuiltInProvenance _ -> BuiltInCategory
            | UserProvenance
            | KitImportProvenance _
            | SourcePullProvenance _
            | ForkProvenance _
            | FitProvenance _ -> UserCategory

    /// The entry version a fork came from, retaining its provenance, licence and citation (spec 0050 S3.1).
    and EntryForkInfo =
        {
            originalId : LibraryEntryId
            originalVersion : VersionNumber
            originalProvenance : EntryProvenance
        }

    /// Where a promoted entry was fitted: the kit, and the stems of the study's `.study-json` and of the
    /// fit run's `.fitrun-json` that reproduce its values (spec 0050 S11.5; 001 §I).
    and EntryFitInfo =
        {
            kit : KitId
            studyStem : FileStem
            fitRunStem : FileStem
        }

    /// The licence credit shown for an entry, including its citation when required (spec 0050 S3.3).
    type EntryAttribution =
        | LicenceAndCitation of licence : EntryLicence * citation : EntryCitation
        | LicenceNameOnly of licence : EntryLicence

        member this.text : string =
            match this with
            | LicenceAndCitation (licence, citation) -> $"Licence {licence.value} — cite: {citation.value}"
            | LicenceNameOnly licence -> $"Licence {licence.value}"

    type EntryLicence with

        /// CC-BY licences require the source citation; all others show their name alone (spec 0050 S3.3).
        member this.attributionWith (citation : EntryCitation) : EntryAttribution =
            if this.value.StartsWith("CC-BY", StringComparison.OrdinalIgnoreCase) then
                LicenceAndCitation (this, citation)
            else
                LicenceNameOnly this

    type EntryProvenance with

        /// Pulled entries retain their licence credit through every fork (spec 0050 S3.3); a fit carries no
        /// licence (S11.5).
        member this.attributionOpt : EntryAttribution option =
            match this with
            | SourcePullProvenance info -> Some (info.licence.attributionWith info.citation)
            | ForkProvenance fork -> fork.originalProvenance.attributionOpt
            | BuiltInProvenance _ | UserProvenance | KitImportProvenance _ | FitProvenance _ -> None

    /// One named entry's licence credit in a surface's attribution list (spec 0050 S3.3).
    type EntryAttributionLine =
        {
            entryName : EntryName
            attribution : EntryAttribution
        }

        member this.text : string = $"{this.entryName.value}: {this.attribution.text}"

    type LibraryEntry with

        /// Credits the entry's GUID, when present, then a sample's materials in set order (spec 0050 S3.3).
        member this.attributedEntryIds : LibraryEntryId list =
            let ownIds = this.entryId.tryToLibraryEntryId |> Option.toList
            let materialIds =
                match this with
                | SampleItem sample ->
                    sample.structure.referencedMaterials
                    |> Set.toList
                    |> List.map (fun material -> material.toLibraryEntryId)
                | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []
            ownIds @ materialIds

    /// The lowercase hex SHA-256 of an entry's canonical content or a kit file's stored bytes (spec 0050 001 §B, §E).
    type ContentHash =
        | ContentHash of string

        member this.value : string = let (ContentHash s) = this in s

        /// The app's one SHA-256 body, used for an entry's canonical content and a kit's files (spec 0050 001 §B, §E).
        static member ofBytes (bytes : FileByteContent) : ContentHash =
            Convert.ToHexStringLower(SHA256.HashData bytes.value) |> ContentHash

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
        /// A crystal-parameter material's record; a material file like `MaterialPayload` (spec 0050 S11.1).
        | CrystalMaterialPayload of CrystalParameters
        | SamplePayload of SamplePayloadData
        | SourcePayload of SourcePayloadData
        | PolarizerPayload of PolarizerPayloadData
        | DetectorPayload of DetectorKind
        | CategoryPayload of CategoryVisibility
        /// An item model's form; its id, name, description and protection belong to the envelope (spec 0050 S7.1).
        | ItemModelPayload of ItemModelForm

        member this.kind : LibraryEntryKind =
            match this with
            | MaterialPayload _
            | CrystalMaterialPayload _ -> MaterialEntryKind
            | SamplePayload _ -> SampleEntryKind
            | SourcePayload _ -> SourceEntryKind
            | PolarizerPayload _ -> PolarizerEntryKind
            | DetectorPayload _ -> DetectorEntryKind
            | CategoryPayload _ -> CategoryEntryKind
            | ItemModelPayload _ -> ItemModelEntryKind

        /// The schema an entry holding this payload is written under: its kind at the kind's only version.
        member this.currentSchema : EntrySchema = { kind = this.kind; version = this.kind.currentSchemaVersion }

    /// Why a domain value cannot be filed as entry content, the content cannot be read back as one
    /// (spec 0050 001 §B), or entries cannot share one catalogue (001 §A). Each reason names the entries.
    type LibraryEntryError =
        | EntryKindMismatchErr of reason : string
        | EntryIdNotGuidErr of reason : string
        | ProvenanceMismatchErr of reason : string
        | MissingEntryFieldErr of reason : string
        | DuplicateEntryFileNameErr of reason : string
        | DuplicateEntryVersionErr of reason : string

        member this.describe : string =
            match this with
            | EntryKindMismatchErr reason -> $"the entry kind does not match: {reason}"
            | EntryIdNotGuidErr reason -> $"the entry id is not a canonical GUID: {reason}"
            | ProvenanceMismatchErr reason -> $"the entry provenance does not match: {reason}"
            | MissingEntryFieldErr reason -> $"an entry field is missing: {reason}"
            | DuplicateEntryFileNameErr reason -> $"an entry file name is duplicated: {reason}"
            | DuplicateEntryVersionErr reason -> $"an entry version is duplicated: {reason}"

    type CategoryName with

        /// The name a category's entry file is filed under (spec 0050 001 §B).
        member this.toEntryName : EntryName = EntryName this.value

    /// One exact library entry version referenced by a kit (spec 0050 S4.2).
    type LibraryVersionRef =
        {
            entryId : LibraryEntryId
            version : VersionNumber
        }

        /// The entry GUID and exact version for diagnostics (spec 0050 S4.2).
        member this.describe : string = $"{this.entryId.value} v{this.version.value}"

    /// A kit reference to an exact version or an entry's latest version (spec 0050 S4.2).
    type EntryReference =
        | EntryVersionReference of LibraryVersionRef
        | EntryLatestReference of LibraryEntryId

        /// The referenced entry's identity, regardless of version selection (spec 0050 S4.2).
        member this.entryId : LibraryEntryId =
            match this with
            | EntryVersionReference reference -> reference.entryId
            | EntryLatestReference entryId -> entryId

        /// The entry GUID with its exact version or latest-version selection (spec 0050 S4.2).
        member this.describe : string =
            match this with
            | EntryVersionReference reference -> reference.describe
            | EntryLatestReference entryId -> $"{entryId.value} (latest)"

    type ItemModelVersionId with

        /// The exact entry version an item pins: the item model's GUID at that version (spec 0050 S7.1).
        member this.entryReference : EntryReference =
            EntryVersionReference
                {
                    entryId = this.itemModelId.toLibraryEntryId
                    version = this.version
                }

    /// An entry file without its `contentHash`: exactly what the hash covers (spec 0050 001 §B).
    /// `categoryOpt` holds a material's category and is `None` for every other kind; a preset's or an item
    /// model's protection and a category's origin are not stored, because they derive from the provenance.
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

        /// The entry as an error names it: its name, id and version.
        member this.describe : string = $"'{this.name.value}' ({this.id.value} v{this.version.value})"

        /// The material's latest category or the sample's exact material versions in set order (spec 0050 S4.2).
        /// An item model's components are inline forms, not entries, so it references none (S7.1).
        member this.references : EntryReference list =
            match this.payload with
            | MaterialPayload _
            | CrystalMaterialPayload _ ->
                this.categoryOpt
                |> Option.toList
                |> List.map (fun category -> EntryLatestReference category.toLibraryEntryId)
            | SamplePayload sample ->
                sample.structure.referencedMaterialVersions
                |> Set.toList
                |> List.map (fun materialVersion ->
                    EntryVersionReference
                        {
                            entryId = materialVersion.materialId.toLibraryEntryId
                            version = materialVersion.version
                        })
            | SourcePayload _ | PolarizerPayload _ | DetectorPayload _ | CategoryPayload _ | ItemModelPayload _ -> []

        /// The referenced identities, so an import never leaves a dangling reference (spec 0050 001 §B; S4.2).
        member this.referencedEntryIds : Set<LibraryEntryId> =
            this.references |> List.map (fun reference -> reference.entryId) |> Set.ofList

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

        /// A complexity physics is filed as its complexity, so the file keeps its bytes, and a crystal as its
        /// record (spec 0050 S11.1).
        static member ofMaterialEntry (version : VersionNumber) (provenance : EntryProvenance) (entry : MaterialEntry) : LibraryEntryContent =
            let payload =
                match entry.physics with
                | ComplexityPhysics complexity -> MaterialPayload complexity
                | CrystalParameterPhysics crystal -> CrystalMaterialPayload crystal
            { LibraryEntryContent.ofPayload version provenance entry.id.toLibraryEntryId entry.name payload with
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

        /// An item model is filed with its description, and only under a provenance that agrees with its protection
        /// (spec 0050 S7.1).
        static member ofItemModel (version : VersionNumber) (provenance : EntryProvenance) (entry : ItemModelEntry) : Result<LibraryEntryContent, LibraryEntryError> =
            if entry.protection = provenance.protection then
                Ok { LibraryEntryContent.ofPayload version provenance entry.id.toLibraryEntryId entry.name (ItemModelPayload entry.model) with
                        descriptionOpt = Some entry.description }
            else
                Error (ProvenanceMismatchErr $"item model '{entry.name.value}' ({entry.id.value}): its protection disagrees with the provenance it is filed under")

        member private this.kindMismatchErr (expected : LibraryEntryKind) : LibraryEntryError =
            EntryKindMismatchErr $"entry '{this.name.value}' ({this.id.value}) holds a {this.payload.kind.extension.value} payload, not a {expected.extension.value} one"

        /// A preset's id: the entry GUID's canonical string (spec 0050 §3.3).
        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member private this.presetId : ElementValueId = ElementValueId (string this.id.value)

        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member this.toMaterialEntry : Result<MaterialEntry, LibraryEntryError> =
            let physicsOpt =
                match this.payload with
                | MaterialPayload complexity -> Some (ComplexityPhysics complexity)
                | CrystalMaterialPayload crystal -> Some (CrystalParameterPhysics crystal)
                | SamplePayload _ | SourcePayload _ | PolarizerPayload _ | DetectorPayload _ | CategoryPayload _ | ItemModelPayload _ -> None
            match physicsOpt, this.categoryOpt with
            | Some physics, Some category ->
                let entry : MaterialEntry =
                    {
                        id = MaterialId this.id.value
                        name = this.name
                        category = category
                        description = this.descriptionOpt
                        physics = physics
                    }
                Ok entry
            | Some _, None -> Error (MissingEntryFieldErr $"material '{this.name.value}' ({this.id.value}) has no category")
            | None, _ -> Error (this.kindMismatchErr MaterialEntryKind)

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

        /// The item model this content files; its protection derives from the provenance (spec 0050 S7.1).
        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, "the library-entry file contract (001 §B): the file's kind-neutral entry GUID becomes the kind's own id")>]
        member this.toItemModel : Result<ItemModelEntry, LibraryEntryError> =
            match this.payload, this.descriptionOpt with
            | ItemModelPayload model, Some description ->
                let entry : ItemModelEntry =
                    {
                        id = ItemModelId this.id.value
                        name = this.name
                        description = description
                        model = model
                        protection = this.provenance.protection
                    }
                Ok entry
            | ItemModelPayload _, None -> Error (MissingEntryFieldErr $"item model '{this.name.value}' ({this.id.value}) has no description")
            | (MaterialPayload _ | CrystalMaterialPayload _ | SamplePayload _ | SourcePayload _ | PolarizerPayload _ | DetectorPayload _ | CategoryPayload _), _ ->
                Error (this.kindMismatchErr ItemModelEntryKind)

    /// One entry of a `.catalogue-json`: the entry's kind, identity, file and hash (spec 0050 001 §A).
    type CatalogueManifestRow =
        {
            kind : LibraryEntryKind
            id : LibraryEntryId
            version : VersionNumber
            fileName : FileName
            contentHash : ContentHash
        }

    /// An entry file: its content and the hash over exactly that content (spec 0050 001 §B). The file
    /// itself is flat: step 30 writes the content's keys and `contentHash` side by side.
    type LibraryEntryEnvelope =
        {
            content : LibraryEntryContent
            contentHash : ContentHash
        }

        /// The entry's row in its catalogue's manifest (spec 0050 001 §A).
        member this.manifestRow : CatalogueManifestRow =
            {
                kind = this.content.schema.kind
                id = this.content.id
                version = this.content.version
                fileName = this.content.fileName
                contentHash = this.contentHash
            }

    type LibraryEntryEnvelope with

        /// The envelope's exact library identity and version (spec 0050 S4.2).
        member this.libraryVersionRef : LibraryVersionRef =
            {
                entryId = this.content.id
                version = this.content.version
            }

    /// A catalogue folder's `.catalogue-json`: its identity and one row per entry file (spec 0050 001 §A).
    /// Its own hash is not stored in it: Storage computes it and S3's start-up import records it (001 §B).
    type CatalogueManifest =
        {
            schemaVersion : FileSchemaVersion
            id : CatalogueId
            name : CatalogueName
            version : VersionNumber
            entries : CatalogueManifestRow list
        }

        /// The only manifest schema version this app reads and writes (spec 0050 001 §B).
        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

        static member extension : FileExtension = FileExtension ".catalogue-json"

        /// A file that exists once per folder is named by its extension alone (spec 0050 001 §A).
        static member fileName : FileName = (FileStem.ofText "").withExtension CatalogueManifest.extension

        /// The first pair of entries that `clashOpt` refuses, earlier entries first.
        static member private tryFindClash (clashOpt : LibraryEntryEnvelope -> LibraryEntryEnvelope -> LibraryEntryError option) (envelopes : LibraryEntryEnvelope list) : LibraryEntryError option =
            match envelopes with
            | [] -> None
            | first :: rest ->
                match rest |> List.tryPick (clashOpt first) with
                | Some error -> Some error
                | None -> CatalogueManifest.tryFindClash clashOpt rest

        static member private versionClashOpt (a : LibraryEntryEnvelope) (b : LibraryEntryEnvelope) : LibraryEntryError option =
            if a.content.id = b.content.id && a.content.version = b.content.version then
                Some (DuplicateEntryVersionErr $"entries {a.content.describe} and {b.content.describe} are one entry version twice")
            else
                None

        /// File names compare as the Windows file system compares them (spec 0050 001 §A); entries of
        /// different kinds never clash, because their extensions differ.
        static member private fileNameClashOpt (a : LibraryEntryEnvelope) (b : LibraryEntryEnvelope) : LibraryEntryError option =
            if a.content.schema.kind = b.content.schema.kind
               && String.Equals(a.content.fileName.value, b.content.fileName.value, StringComparison.OrdinalIgnoreCase) then
                Some (DuplicateEntryFileNameErr $"entries {a.content.describe} and {b.content.describe} share the file name '{b.content.fileName.value}'")
            else
                None

        /// The manifest of a catalogue holding `envelopes`, which export checks before writing (spec 0050
        /// 001 §A): no entry version twice, and per kind no file name twice. Its rows are sorted by kind
        /// tag, then ordinally by file name, so its text does not depend on the envelopes' order.
        static member tryCreate (id : CatalogueId) (name : CatalogueName) (version : VersionNumber) (envelopes : LibraryEntryEnvelope list) : Result<CatalogueManifest, LibraryEntryError> =
            match CatalogueManifest.tryFindClash CatalogueManifest.versionClashOpt envelopes with
            | Some error -> Error error
            | None ->
                match CatalogueManifest.tryFindClash CatalogueManifest.fileNameClashOpt envelopes with
                | Some error -> Error error
                | None ->
                    let manifest : CatalogueManifest =
                        {
                            schemaVersion = CatalogueManifest.currentSchemaVersion
                            id = id
                            name = name
                            version = version
                            entries =
                                envelopes
                                |> List.map (fun e -> e.manifestRow)
                                |> List.sortWith (fun a b ->
                                    match compare a.kind.value b.kind.value with
                                    | 0 -> String.CompareOrdinal(a.fileName.value, b.fileName.value)
                                    | byKind -> byKind)
                        }
                    Ok manifest

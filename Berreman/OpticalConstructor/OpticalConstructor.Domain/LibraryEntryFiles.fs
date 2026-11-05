namespace OpticalConstructor.Domain

open System
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

/// The identities, provenance and name rule of the library-entry files (spec 0050 001 §A, §B;
/// .spec-md §3.11, §5).
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

        /// A preset's entry id, or `None` for text that is not a canonical GUID string: every
        /// seeded and minted preset id is one (spec 0050 §3.3).
        member this.tryToLibraryEntryId : LibraryEntryId option =
            match Guid.TryParseExact(this.value, "D") with
            | true, g -> Some (LibraryEntryId g)
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

namespace OpticalConstructor.Tests

open System
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles

/// The kinds, provenance, ids and name rule of the library-entry files (spec 0050 001 §A, §B;
/// .spec-md §3.11).
module LibraryEntryFilesTests =

    let private kindsInTagOrder : LibraryEntryKind list =
        [ MaterialEntryKind; SampleEntryKind; SourceEntryKind; PolarizerEntryKind; DetectorEntryKind; CategoryEntryKind ]

    [<Fact>]
    let ``every library entry kind round-trips its wire tag and renders its 001 extension`` () =
        Assert.Equal<int list>([ 1; 2; 3; 4; 5; 6 ], kindsInTagOrder |> List.map (fun k -> k.value))
        Assert.Equal<LibraryEntryKind option list>(kindsInTagOrder |> List.map Some, kindsInTagOrder |> List.map (fun k -> LibraryEntryKind.tryOfWire k.value))
        Assert.Equal<LibraryEntryKind option>(None, LibraryEntryKind.tryOfWire 0)
        Assert.Equal<LibraryEntryKind option>(None, LibraryEntryKind.tryOfWire 7)

        let extensions =
            [
                FileExtension ".material-json"
                FileExtension ".sample-json"
                FileExtension ".source-json"
                FileExtension ".polarizer-json"
                FileExtension ".detector-json"
                FileExtension ".category-json"
            ]

        Assert.Equal<FileExtension list>(extensions, kindsInTagOrder |> List.map (fun k -> k.extension))
        Assert.Equal<FileSchemaVersion list>(kindsInTagOrder |> List.map (fun _ -> FileSchemaVersion 1), kindsInTagOrder |> List.map (fun k -> k.currentSchemaVersion))
        Assert.Equal<LibraryEntryKind list>(kindsInTagOrder, LibraryEntryKind.all)

    [<Fact>]
    let ``a file stem keeps the name's characters and replaces only the ones Windows forbids`` () =
        Assert.Equal<FileStem>(FileStem "Ti_sapphire laser, tunable 700–1000 nm", FileStem.ofText "Ti:sapphire laser, tunable 700–1000 nm")
        Assert.Equal<FileStem>(FileStem "a_b_c_d_e_f_g_h_i_j", FileStem.ofText """a/b\c*d?e"f<g>h|i:j""")
        Assert.Equal<FileStem>(FileStem "Langasite (La3Ga5SiO14)", FileStem.ofText "Langasite (La3Ga5SiO14)")
        Assert.Equal<FileStem>(FileStem "CO₂ laser 10.6 µm", FileStem.ofText "CO₂ laser 10.6 µm")

        let siliconFile = (EntryName "Silicon").fileStem.withExtension MaterialEntryKind.extension
        Assert.Equal<FileName>(FileName "Silicon.material-json", siliconFile)

    [<Fact>]
    let ``a built-in provenance is protected and every other provenance is user-managed`` () =
        let pulled : SourcePullInfo =
            {
                source = MaterialSourceName "refractiveindex.info"
                record = MaterialSourceRecordId "main/Si/Aspnes"
                sourceVersion = MaterialSourceVersion "2024-08-14"
                licence = EntryLicence "CC0-1.0"
                citation = EntryCitation "D. E. Aspnes and A. A. Studna, Phys. Rev. B 27, 985 (1983)"
            }

        let provenances =
            [
                BuiltInProvenance (CatalogueId (Guid.Parse "3f0c2d1e-7a4b-4c5d-9e8f-0a1b2c3d4e5f"))
                UserProvenance
                KitImportProvenance (KitId (Guid.Parse "b2e4f6a8-1c3d-4e5f-8a9b-c0d1e2f3a4b5"))
                SourcePullProvenance pulled
            ]

        Assert.Equal<EntryProtection list>([ ProtectedBuiltIn; UserManaged; UserManaged; UserManaged ], provenances |> List.map (fun p -> p.protection))
        Assert.Equal<CategoryOrigin list>([ BuiltInCategory; UserCategory; UserCategory; UserCategory ], provenances |> List.map (fun p -> p.categoryOrigin))

    [<Fact>]
    let ``a library entry id keeps the GUID of the kind's own id`` () =
        Assert.Equal<Guid>(MaterialIds.silicon.value, MaterialIds.silicon.toLibraryEntryId.value)
        Assert.Equal<Guid>(SampleIds.multilayerQwId.value, SampleIds.multilayerQwId.toLibraryEntryId.value)
        Assert.Equal<Guid>(CategoryIds.glass.value, CategoryIds.glass.toLibraryEntryId.value)

        let ellipsometerGuid = Guid.Parse PresetIds.detectorEllipsometer.value
        Assert.Equal<Guid option>(Some ellipsometerGuid, PresetIds.detectorEllipsometer.tryToLibraryEntryId |> Option.map (fun id -> id.value))

        let notAGuid = ElementValueId "not-a-guid"
        Assert.Equal<LibraryEntryId option>(None, notAGuid.tryToLibraryEntryId)

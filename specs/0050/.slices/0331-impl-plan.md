# 0331 impl-plan — RETIRE_CONTRACT STORE_XDUO_0009 MuellerDataProxy

Pure deletion (spec 0050 S9.4; 001 §R, §S9, §T). After step 330 nothing outside the five seam facts reads
the archive-keyed load seam, so it goes with its identity DU, archive path, the two archive error cases and
the archive store. No replacement seam, wrapper, path-keyed variant or alias.

Files (all inside the step's authorized `effects.edits`):

1. `OpticalConstructor.Domain/MuellerReconstruction.fs` — delete `MuellerDataSet`, `MuellerArchivePath`,
   `MuellerDataProxy` (+ docs) and the `ArchiveUnreadableErr` / `DataSetMissingErr` cases; shrink the
   `MuellerDataError` doc and the seam's opening comment block to 1–3 lines each, as the brief words them.
2. `OpticalConstructor.Storage/MuellerDataStore.fs` — delete the `MuellerDataSet` extension
   (`archiveEntryName`), `committedArchive`, `tryLoadFromArchive`, `createArchiveBacked`, `open System.IO`,
   `open System.IO.Compression`; drop the trailing comment on the `MuellerReconstruction` open; module doc to
   1–3 lines. `CaptureCsvRow` and `parseMuellerCsv` unchanged.
3. `OpticalConstructor.Storage/LabFileImport.fs` — `PhoneRoiCsvProfile` arm loses the two archive arms and the
   comment about them.
4. `OpticalConstructor.Storage.fsproj` — delete the comment above `MuellerDataStore.fs`'s `<Compile>`.
5. Tests — `MuellerReconstructionTests.fs`: delete the five facts, `cannedRows`, `makeMockData`,
   `seededDataMock` and their comment block, `open System.IO.Compression`, `open System.IO`.
   `MuellerInverseTests.fs`: the comment names `ExperimentDataProxy / MuellerSolverProxy` only.

Risks: an `open System.IO` still needed elsewhere in the test file (checked: only the deleted DataSetMissing
fact uses Path/File/FileStream/ZipArchive); the selected class's count falls by exactly 5.

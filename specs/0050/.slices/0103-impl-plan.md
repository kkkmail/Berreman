# Implementation plan

Implement S32-ENTRY-FILE-READ in the four authorized source files. Add exactly the two named facts in `LibraryEntryFilesTests.fs` and `CatalogueFolderTests.fs`, reusing the existing catalogue fixtures and keeping every existing assertion. Cover all six extensions, case-insensitive matching, non-entry names, successful standalone reads, IO failure, wrong kind, changed payload hash and unsupported schema version.

Add `LibraryEntryKind.tryOfFileName` using `FileName.tryGetExtension` and ordinal case-insensitive comparison. Extract the entry-file UTF-8 read into `LibraryFileText.readEntryFile`, returning `FileIoExn` at the filesystem boundary and delegating parsing to `parseEntry`. Route catalogue rows and top-level entry discovery through the shared members while preserving manifest checks, ordering and error context.

Run the resolved Release build, elevation, constructor suite and eleven s13 invariants as local verification required by the session instructions; formal supervisor acceptance and checkpoint updates remain external. Keep persistent evidence under the supplied `.artifacts` folder. Write the required log and state-of-the-world at their supplied paths; leave versions, external repositories, analyzer policy, gate definitions and the pre-existing supervisor state edit alone.

The main risks are changing catalogue error mapping, bypassing schema/version/hash validation, and inadvertently admitting manifests or nested files into the catalogue entry listing. No persisted shape changes or migration is needed.

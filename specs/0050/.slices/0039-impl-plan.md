# Step 0039 - IMPLEMENT - plan

Add intrinsic `describe : string` members to `LibraryEntryError` in
`LibraryEntryFiles.fs` and `CatalogueError` in `CatalogueFiles.fs`. Each exhaustive
match renders a distinct condition followed by a colon and its diagnostic reason.
The catalogue's invalid-entries case delegates to the wrapped error's `describe`.
No union case, stored shape, dependency or caller changes.

First add the two facts named in the slice to `LibraryEntryFilesTests.fs` and
`CatalogueProxyTests.fs`. Cover all six cases of each union with distinct local
reasons, assert their complete descriptions and distinct results, and explicitly
assert that the nested duplicate-version description survives intact.

Review the four-file source diff against S16-ERROR-RENDERS and preserve LF endings.
The IMPLEMENT role's invariant 6 assigns build, elevation, constructor tests and
the six s06 invariant gates to the supervisor after exit. Record that pending
verification honestly in the log, state file and structured handoff. Preserve the
pre-existing supervisor manifest modification and do not change version files.

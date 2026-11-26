# Step 0058 implementation plan

Remove the Domain's built-in material/category values and sample/source/preset
values, their private builders and grouping trees, and the four self-seeding
store factories. Preserve all well-known identities and the quarter-wave
thicknesses used by `starterMultilayerStructure`. Keep the existing empty-store
signatures and bodies, renaming their public factories to `createInMemory` and
updating every caller. Catalogue files remain the source of built-in entries.

Update the affected Domain documentation, `LibraryFacets.fs`, the constructor
fixture and workbench test, and the live-library guidance in `AGENTS.md`.
`CatalogueFiles.fs` and `CatalogueImport.fs` already have current documentation
without obsolete open comments; only their actual stale references need edits.
The existing `LibraryProxyTests.fs` empty-store call also needs the mechanical
rename required by the slice's "every caller follows" obligation. Its fact and
assertions remain unchanged; this necessary caller was omitted from the edit
inventory but belongs to the declared constructor-test scope.

The pre-rename source search found no qualified calls to the self-seeding
factories, so no caller changes meaning. Review the edit for dangling helpers,
unused opens, altered identities, changed test declarations and line-ending
churn. The IMPLEMENT worker protocol reserves gate execution for the supervisor;
record pending verification without claiming passes or changing checkpoints.

Risks are removing an engine extension open still used by material composition,
deleting the editor's live starter structure along with the samples, and missing
an empty-store caller. No stored format, catalogue content, version ring, gate,
test identity or supervisor record changes.

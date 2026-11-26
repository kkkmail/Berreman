# Step 0034 — ADD_RED — impl plan

Red first for 001 §B "Shipped catalogue" ("tests load the same files") and `.spec-md` §3.5, §3.6: the catalogue the build
copies into the test output equals the code seeds. Oracle row `S15-BUILTIN-PARITY`, red `RED_0006`; step 35 closes it.

## Approach

- New `OpticalConstructor.Tests/BuiltInCatalogueParityTests.fs` (`module BuiltInCatalogueParityTests`), with exactly the
  slice's five facts, compiled directly after `CatalogueFolderTests.fs`.
- `readBuiltIn ()` runs first in every fact, so every fact fails on the cause the slice names: the build copies no
  `Catalogue/Builtin/`, and the read answers `CatalogueIoErr`.
- `expectedCatalogue ()` seals every seed at `VersionNumber.first` under `BuiltInProvenance CatalogueIds.builtInCatalogue`,
  with no use of anything the export writes. It is a function, so a failure sinks its fact and not the assembly's start-up.
- One private helper decodes a kind with its own decoder and compares it with the seeds by id. A catalogue lists entries by
  kind tag and then file name while the seeds keep display order, so a positional comparison could never go green.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/BuiltInCatalogueParityTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one `Compile` line, no comment)

No production file changes. These two files are exactly the slice's authorized edits.

## Risks

- **A green that step 35 cannot reach.** Checked by reading: `every built-in seals, writes, reads and decodes equal to its
  seed` (`EntryFileJsonTests.fs:439`) already round-trips every seed, and `CatalogueData.tryCreate` and the folder read order
  envelopes the same way, by manifest row.
- **A local check against the role's "run no checks".** See the impl-log's Gotchas for the reading chosen.

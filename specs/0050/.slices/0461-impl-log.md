# Step 0461 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 and the slice. None of the rulings
  names this step or its files. They are preserved by leaving their files untouched.
- Took step 0405 (`FitProvenance`) as the pattern for adding a provenance case: the case and its info record in the
  Domain, the store's two members, the schema def plus its `oneOf` entry, and the widened facts.
- **Domain** (`LibraryEntryFiles.fs`):
  - `EntryProvenance` gained `FileImportProvenance of EntryFileImportInfo`, declared with
    `and EntryFileImportInfo = { file : FileName; fileHash : ContentHash }`.
  - `protection` answers `UserManaged`, `categoryOrigin` `UserCategory` and `attributionOpt` `None`.
  - The docs cite S13.2.
- **Storage** (`LibraryDatabaseStore.fs`): `kindValue` answers 7 and `catalogueIdOpt` answers `None`. FS0025 also
  reports `pulledSourceRows`, which now answers `[]` for a file import, as it does for `UserProvenance`.
- **Schema** (`library-common.schema.json`):
  - A `fileImportProvenance` def is added and joins the `provenance` `oneOf`. Its shape is
    `{"Case": "FileImportProvenance", "Fields": [{"file": <non-empty string>, "fileHash": <contentHash>}]}`, exactly
    what the canonical options write, since they unwrap single-case unions.
  - The `provenance` description names the new origin. The entry schemas stay at version 1.
- **FS0025 fallout outside `effects.edits`** (see Gotchas): `CatalogueImport.fs` (`pulledRecordsOf`, → `None`) and
  `CatalogueImportStore.fs` (the pulled-source row check, → its `LibraryRowInvalidErr`). In each, the new case joins
  the alternatives `UserProvenance` already sits in.
- **Tests:**
  - `LibraryEntryFilesTests` has a private `importedFromFile` fixture: `FileName "test-glasses.agf"` and the
    `ContentHash.ofBytes` of the UTF-8 bytes of `abc`.
    - `a built-in provenance is protected and every other provenance is user-managed` now lists it and expects
      `UserManaged` / `UserCategory`.
    - `a pulled entry and its forks carry its licence's attribution, and every other provenance none` lists it among
      the unlicensed provenances. It answers `None`, and so does a fork of it.
  - `EntryFileJsonTests`' `every provenance round-trips through an entry file` gained a silica copy under
    `FileImportProvenance`. Its file round-trips, schema validation included, and `toMaterialEntry` answers the
    material.
  - No test identity was added, renamed or removed.
- The full build was green on the first try. Every gate in the roster then ran green locally (see Testing state).

## Files modified

Authorized by the step's effects:

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LibraryDatabaseStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/library-common.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs`

Authorized by the how_to's FS0025 clause ("Any other complete match on `EntryProvenance` that FS0025 reports answers
a file import as it answers `UserProvenance`"), and inside the step's `touches` projects:

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/CatalogueImport.fs` (one line)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CatalogueImportStore.fs` (one line)

## Testing state

All runs were local development runs. The supervisor's gate engine is the authority.

- **build:** green (`dotnet build Berreman.slnx -c Release`, 83 s, under the 600 s deadline). It produced 0 errors and
  no `FS` warnings. The only warnings are:
  - the exempt `NU1902` advisories;
  - `SYSLIB0051` from the vendored `MathNetNumerics/Numerics/Exceptions.cs`. That code is not touched here, and the
    warning already appears in `0459-dev-build.log`.
- **constructor-unit-tests:** whole suite green, 1828 succeeded of 1828. The count is unchanged from checkpoint 0460,
  as the acceptance requires. It took 104 s, under the 180 s deadline.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 227 s, under the 600 s deadline. Suppressed
  findings stay at 1757 and known findings at 0.
- **invariant-s44-\*:** all twenty-one hold.

## Artifacts

- `specs/0050/.artifacts/0461-dev-build.log`
- `specs/0050/.artifacts/0461-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0461-dev-elevation.log`
- `specs/0050/.artifacts/0461-dev-invariants.log`

## Gotchas

- Two FS0025 sites lie outside `effects.edits` but inside the touched projects: `CatalogueImport.fs` and
  `CatalogueImportStore.fs`.
  - The how_to's clause names exactly this case, and "no warnings from our code" makes the edits mandatory.
  - Each is a one-alternative addition beside `UserProvenance`, with no behaviour change for any existing case.
- `OpticalConstructor.Ui.Tests/LauncherTests.fs` matches on provenance with a wildcard, so FS0025 does not report it
  and it was left untouched.
- The reference kits under `data/` hold neither `library-common.schema.json` nor any entry of this provenance, so the
  committed-kit equality facts are unaffected.
- The `contentHash` def's description speaks of an entry's canonical JSON. Like `recordHash`, `fileHash` reuses that
  def for its pattern only, and the how_to asks for exactly that def. Its description was not rewritten, because no
  other schema change is authorized.

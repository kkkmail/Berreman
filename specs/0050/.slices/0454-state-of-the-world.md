# Where we are

Step 0454 adds the PTB OCDB optical-constants reader (spec 0050 S13.2, obligation S132-PTB-READER).

- Step 453 gave the tabulated n,k table optional per-sample one-sigma uncertainties.
- This step reads PTB's EUV / VUV records into such a table, inside `MaterialImport`, the module that reads every other
  optical-constants file.
- Step 455's PTB adapter will therefore parse nothing itself. It calls `PtbOcdbText.importMaterial` and
  `headlineOpt`.
- Every tabulated import now assembles its entry through one member, `TabulatedNkTable.isotropicEntry`.

# What's working

- Added `PtbOcdbText` with two members:
  - `importMaterial` reads a PTB record: n = 1 − δ, k = β, λ in nm through the `Units` seam, rows sorted by λ;
  - `headlineOpt` gives the record's first comment.
- PTB's 3-sigma bounds become one-sigma uncertainties, (HB − LB) / (2 × level), through
  `TabulatedNkTable.withUncertainty`.
- Moved the tabulated entry assembly into `TabulatedNkTable.isotropicEntry`. The refractiveindex.info and CSV imports
  call it with every field unchanged.
- Added `ImportError.MalformedSourceFileErr` for:
  - a missing column, or a partial set of bound columns;
  - bounds with no sigma level;
  - a bad row, named by its 1-based line;
  - any .NET exception.

  A record with no row answers `NoDataErr`.
- Added the fixture `fixtures/ptb-Co.txt`: PTB's `Co.txt` header and three of its rows, CC BY 4.0, credited by its
  own first line.
- Added two `MaterialImportTests` facts and widened the import-error render fact.

# Tests

These are development checks. The gate engine runs the roster after this session and is the authority. Every figure
is from one build, the final one.

- **build:** exits 0 in 85 s, against a 600 s deadline, with 0 errors. The warnings are the exempt NU1902 advisories
  and the vendored MathNet's SYSLIB0051, which predates this step. There is no FS or MSB3277 warning.
- **constructor-unit-tests, the whole suite as the gate selects it:** `Passed!`, 1804 of 1804, in 109 s against a
  180 s deadline.
  - That is checkpoint 453's 1802 plus the step's two facts:
    - `a PTB OCDB record reads n as 1 - delta and k as beta, its 3-sigma bounds as one-sigma uncertainties`;
    - `a PTB OCDB record without bounds has no uncertainty, and a malformed record is refused`.
  - The widened `every import error renders its condition and its payload` passes.
  - Filtered first, `MaterialImportTests` alone passed 22 of 22.
- **kit-acceptance, filtered to `OpticalConstructor.Ui.Tests.KitAcceptanceTests`:** `Passed!`, 14 succeeded and 1
  skipped, in 1414 s against a 3600 s deadline.
  - The skipped fact is AT-2's five-seed ensemble, which is xUnit v3 Explicit; the gate's skip policy is `allow`.
  - Both `… kit equals the committed kit file by file` facts passed. AT-1's and AT-2's expectations and tolerances are
    unchanged, and `data/` is untouched.
- **elevation, all 13 projects:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, in 223 s against a 600 s
  deadline.
- **The twenty-one s44 invariants:** each decoded `git grep`, run with `--untracked`, finds nothing.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1804
  kit_acceptance_tests: 14
```

# Architecture

- **One assembly for every tabulated entry.** `type TabulatedNkTable with member this.isotropicEntry name category
  description` (in `MaterialImport`) builds:
  - one `IsotropicDispersive` segment over `sampleRange`, with the axis `TabulatedNK this`;
  - no magnetic or active part;
  - a minted `MaterialId`.

  Callers:
  - `entryFromTabulated` (refractiveindex.info and CSV) calls it with its old description, so no field or number
    moves;
  - the PTB reader calls it with its own name, category (`CategoryIds.semiconductor`) and description;
  - step 455's adapter renames the entry.
- **The reader's shape.** A private `PtbOcdbLines` holds the line rules: comments, header columns, sigma level, numbered
  data lines. `PtbOcdbText.importMaterial` holds the numeric reading. The reader:
  - splits lines with `RiiPage.linesOf`;
  - collects its rows in order with `ItemEvaluation.sequenced`, so the first bad line wins;
  - sorts the rows by λ in metres with `Array.sortBy`, as `entryFromTabulated` does;
  - derives n, k, σn and σk from the sorted rows, so each uncertainty stays with its sample;
  - builds the table with `tryCreate`, then `withUncertainty` when there are bounds.
- **Refusals.** Every `TabulatedTableError` and every caught exception answers `MalformedSourceFileErr`. Only a record
  with no row answers `NoDataErr`.
- **Signatures.** The public members carry only our types (`EntryCitation option`, `Result<MaterialEntry,
  ImportError>`, `EntryName` / `CategoryId` / `EntryDescription` → `MaterialEntry`). The private `PtbOcdbLines` members
  speak only primitives. No member mixes the two, which is ELEV010.
- R0001–R0005 are untouched: this step edits none of the files they govern.

# Deferred

Nothing.

# Gotchas

- **`headlineOpt` answers `None` when the first comment's text is empty**, not `Some ""`. The adapter (step 455)
  appends `: <headline>` only when one is answered, and an empty one would leave a dangling `: `.
- **A sigma level of 0 is refused** (`MalformedSourceFileErr "the sigma level 0 is not positive"`) rather than
  dividing by zero.
- **A text without a header answers `MalformedSourceFileErr` naming `lambda/nm`**, because the column checks come
  first. `NoDataErr` is for a well-formed header with no row.
- **The PTB row reader is not `RiiPage.floatsOn`.** That function drops tokens it cannot parse and splits on commas.
  A PTB row splits on tabs and spaces only, and one unreadable token refuses the line.
- **The fixture is PTB's own text.** It was checked byte for byte against the live `Co.txt`, saved in
  `.artifacts/0454-ptb-source/`. The bounds are absolute values at 3 sigma, not offsets.

# Changelog

- 2026-11-17, step 0454:
  - added `PtbOcdbText` (`headlineOpt`, `importMaterial`), `ImportError.MalformedSourceFileErr` and
    `TabulatedNkTable.isotropicEntry`, which every tabulated import now calls;
  - added the `ptb-Co.txt` fixture and two `MaterialImportTests` facts, and widened the render fact;
  - constructor-unit-tests goes from 1802 to 1804.

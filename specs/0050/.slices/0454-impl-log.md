# Step 0454 — impl log (IMPLEMENT, S132-PTB-READER)

## Progress

- Read the task, the arc-runner base prompt, the IMPLEMENT delta, the project prompt, the slice, and the "Binding
  ruling" sections of rulings R0001–R0005.
  - The rulings confine their extra authority to steps 0427, 0428 and 0436.
  - Here they only ask that their rules be preserved. This step touches none of the files they govern: no
    StudyEngine, StudyForward, FittedProblems, ItemEvaluation, BerremanMatrix, Fields or project-file change.
- Surveyed:
  - `MaterialImport.fs`: `entryFromTabulated`, `RiiPage`, `ImportError` and the CSV import;
  - `Dispersion.fs`' `TabulatedNkTable`, `TabulatedNkUncertainty` and `withUncertainty` (step 453);
  - `MaterialImportTests` and its helpers;
  - the analyzer sources for ELEV001 and ELEV011, to learn what they actually flag;
  - `ItemEvaluation.sequenced`, the shared ordered Result collector (`.manual/016`);
  - every `ImportError` match in the tree. All of them are in `MaterialImport.fs` and its tests, so no FS0025
    outside.
- Fetched PTB's live `Co.txt` into `.artifacts/0454-ptb-source/Co.txt` to check the fixture against it.
  - It came from `raw.githubusercontent.com/PTB-SR/ocdb/master/ocdb/db/data/Co.txt`, an anonymous GET of public data.
  - Its seven header lines and its 8.0, 11.2 and 25.0 nm rows equal the slice's text byte for byte.
- Implemented the four authorized edits (below). Then built, ran the touched class, the whole constructor suite, the
  21 invariants and elevation, and finally kit-acceptance. All were green the first time, so no repair round was
  needed.
- The first wait on elevation exited early, on the run's opening `ELEV-GATE: scope` line. A second wait read the
  actual `ELEV-GATE: OK` verdict.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
  - `ImportError` gains `MalformedSourceFileErr of reason : string`, with a one-line doc. `describe` renders it as
    `the source file is malformed: <reason>`.
  - New public `PtbOcdbText of string`, with `member this.value : string`. It is declared beside the other text
    types.
  - New `type TabulatedNkTable with member this.isotropicEntry (name : EntryName) (category : CategoryId)
    (description : EntryDescription) : MaterialEntry`. This is the entry assembly moved out of `entryFromTabulated`:
    - an `EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = this.sampleRange; dispersion = TabulatedNK
      this } ])`;
    - no magnetic or active part;
    - `MaterialId.create ()`;
    - `description = Some description`.
  - `entryFromTabulated` keeps its sort, its `tryCreate` and its `NoDataErr` mapping. It then answers
    `table.isotropicEntry name category (EntryDescription "Imported tabulated n,k (refractiveindex.info).")`, the
    description it wrote before. The refractiveindex.info and CSV imports therefore keep every field and every number.
  - `RiiPage.linesOf`'s doc now names the PTB record as well, because the reader calls it rather than copying it.
  - The module doc names the PTB import and `PtbOcdbText`.
  - New private `PtbOcdbLines of string[]`, the record's lines, with four line-rule members:
    - `commentTexts`: each line whose trimmed text starts with `#`, with its `#` and surrounding white space removed;
    - `columns`: the comment whose text starts with `lambda/nm` (ordinal), split on tabs and spaces; empty without
      one;
    - `sigmaLevelOpt`: in the first comment holding `-sigma` (ordinal, ignoring case) with an integer before it, that
      integer;
    - `dataLines`: every non-empty, non-comment line, trimmed, with its 1-based number.
  - New `type PtbOcdbText with`:
    - `member this.headlineOpt : EntryCitation option`: the first comment's text, or `None` (see Decisions).
    - `member this.importMaterial : Result<MaterialEntry, ImportError>`, inside `try … with e -> Error
      (MalformedSourceFileErr e.Message)`. It answers in this order:
      - `lambda/nm`, `1-delta` or `beta` absent: `MalformedSourceFileErr "the column header has no <column>
        column"`.
      - All four bound columns absent: read without uncertainty.
      - All four present with a level ≥ 1: read with that level.
      - All four present with level 0: `MalformedSourceFileErr "the sigma level 0 is not positive"`.
      - All four present without a level: `MalformedSourceFileErr "the record has bound columns but states no sigma
        level, as 3 in 3-sigma"`.
      - Some bound columns present but not all: `MalformedSourceFileErr "the bound columns come all four or none, and
        the column header has no <the absent ones>"`.
    - Reading the rows:
      - each data line is split on tabs and spaces;
      - a line whose value count differs from the header's answers `MalformedSourceFileErr "line <n> holds <c> values
        where the header names <h> columns"`;
      - a line whose token does not parse (invariant culture, `NumberStyles.Float`) answers `MalformedSourceFileErr
        "line <n> does not read as numbers: <line>"`;
      - the rows are collected in order through `ItemEvaluation.sequenced`, so the first bad line wins;
      - no row answers `NoDataErr "the record holds no row"`.
    - Building the table:
      - λ goes through `toMeters Nanometer`;
      - the rows are sorted by that wavelength with `Array.sortBy`, as `entryFromTabulated` sorts;
      - n is the `1-delta` column and k the `beta` column;
      - `TabulatedNkTable.tryCreate`, then, with bounds, `withUncertainty { sigmaN = (n_HB − n_LB) / (2 × level);
        sigmaK = (k_HB − k_LB) / (2 × level) }`, each taken from the sorted rows, so each uncertainty stays with its
        sample;
      - each `TabulatedTableError` answers `MalformedSourceFileErr reason`.
    - The entry is `table.isotropicEntry (EntryName "Imported (PTB OCDB)") CategoryIds.semiconductor (EntryDescription
      "Imported tabulated n,k (PTB OCDB).")`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/fixtures/ptb-Co.txt` (new, LF)
  - PTB's seven `Co.txt` header lines and the rows 8.0, 11.2 and 25.0, tab-separated, as the slice lists them.
  - Checked byte for byte against the live file.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: a `Content` item for
  `fixtures\ptb-Co.txt` with `PreserveNewest`, right after `sample-nk.csv`'s, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs`
  - `every import error renders its condition and its payload` gains a `MalformedSourceFileErr` row. The step's
    `change_expectation` adds it, and the distinctness check now covers five texts.
  - New private `PtbBoundedSample` (a `TabulatedSample` plus its four bounds), `ptbCoRows` (the fixture's three rows by
    hand), `ptbCoTable` (a `TabulatedFixture` in nm built from them) and `importedTable`.
  - New fact `a PTB OCDB record reads n as 1 - delta and k as beta, its 3-sigma bounds as one-sigma uncertainties`:
    - it reuses `assertViewOnlyTabulatedSegment` and `assertTableColumns`: one isotropic segment over the sample
      range, no magnetic or active part, λ in metres within 1e-15 m, n and k exact;
    - σn and σk are within 1e-15 of (HB − LB) / 6 computed from each row's bounds;
    - name, category and description;
    - `headlineOpt = Some (EntryCitation "Optical constants for Co created by PTB")`;
    - the same rows in reverse order under CRLF read into an equal table, uncertainties included.
  - New private `RefusedPtbText`, and new fact `a PTB OCDB record without bounds has no uncertainty, and a malformed
    record is refused`:
    - a three-column text with two rows out of order reads sorted with `uncertaintyOpt = None`;
    - four texts answer `MalformedSourceFileErr`, each reason naming its fault:
      - no `beta` column: `beta`;
      - `beta_LB` alone: `beta_HB`;
      - bounds with no sigma line: `sigma level`;
      - a row of six values: `line 7`;
    - a header with no row answers `NoDataErr`.

`specs/0050/.manifest.state.json` was already modified when the round started. It is the supervisor's file, and this
round did not touch it.

## Decisions

- **`headlineOpt` answers `None` when the first comment's text is empty** (a bare `#`). The brief says "the text of
  the first comment line … `None` for a text without one".
  - An empty text is no headline. Step 455 appends `: <headline>` to the citation only "when `headlineOpt` answers
    one", and an empty one would leave a dangling `: `.
  - The refractiveindex.info `citationOpt` likewise answers `None` for an empty value.
  - PTB's own records always open with their headline.
- **A sigma level of 0 is refused** with `MalformedSourceFileErr "the sigma level 0 is not positive"`.
  - Dividing by it would otherwise give ∞ or NaN, which `withUncertainty` would refuse less clearly.
  - The answer is still the one the brief names for a bad level: `MalformedSourceFileErr` naming the level.
  - This adds no option and no second format.
- **The column checks come before the row checks.** An empty text, which has no header, therefore answers
  `MalformedSourceFileErr` naming `lambda/nm`. `NoDataErr` is the answer for a well-formed header with no row, which
  is the brief's case.
- **Shared code is called, not copied.** The reader:
  - splits lines with `RiiPage.linesOf`, the module's one line splitter, whose doc now names both sources;
  - collects its rows with `ItemEvaluation.sequenced`, the shared ordered Result collector of `.manual/016`;
  - assembles its entry through `isotropicEntry`.
- **It does not reuse `RiiPage.floatsOn`.** That function drops tokens that do not parse and also splits on commas.
  The PTB rule is that a token which does not read refuses its line, and that rows split on tabs and spaces only.
- **The numeric reading is done by local functions inside `importMaterial`.**
  - The private `PtbOcdbLines` members speak only primitives (`string[]`, `int option`, `(int * string)[]`), so no
    member mixes our types with primitives. That mix is ELEV010, step 453's lesson.
  - The public members' signatures carry only our types.
- **No test identity beyond the census.** The two new facts are the step's `create` effects, and the render fact is
  its `change_expectation`. The added assertions (the reversed CRLF text, the reason fragments, the entry's name,
  category and description) sit inside those facts.

## Testing state

These are development checks. The gate engine runs the step's roster after this session and is the authority. Every
figure below is from one build, the final one.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0 in 85 s, against a 600 s deadline, with 0
  errors.
  - The warnings are 12 NU1902 (exempt) and 4 SYSLIB0051 from the vendored MathNet, which predate this step.
  - There is no FS or MSB3277 warning.
  - Log: `.artifacts/0454-dev-build.log`.
- **`MaterialImportTests`, filtered first:** `Passed!`, 22 of 22, in 2 s. That is the class's 20 facts plus the
  step's two. Log: `.artifacts/0454-dev-material-import-class.log`.
- **constructor-unit-tests, the whole suite as the gate selects it** (`all_discovered`): `Passed!`, 1804 of 1804, in
  109 s against a 180 s deadline.
  - That is checkpoint 453's 1802 plus the step's two facts, the "count rising by 2" the acceptance asks for.
  - Elevation was running at the same time.
  - Log: `.artifacts/0454-dev-constructor-unit-tests.log`.
- **elevation, all 13 projects:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 223s`, against a 600 s
  deadline, with known findings 0. Storage reports 0 findings. Log: `.artifacts/0454-dev-elevation.log`.
- **kit-acceptance, filtered to `OpticalConstructor.Ui.Tests.KitAcceptanceTests`** with the gate's command:
  `Passed!`, 14 succeeded and 1 skipped, in 1414 s against a 3600 s deadline.
  - The skipped fact is AT-2's five-seed ensemble, which is xUnit v3 Explicit. The gate's skip policy is `allow`.
  - Both `… kit equals the committed kit file by file` facts passed, per the TRX.
  - AT-2's longest facts took 787 s (the grid generator) and 551 s (the committed kit's read-only reproduction). Both
    are within R0005's 1800 s bound.
  - `data/` is untouched.
  - Log: `.artifacts/0454-dev-kit-acceptance.log`.
- **The 21 s44 invariants:** each decoded `git grep`, run with `--untracked`, finds nothing.
- **LF check:** `git diff --numstat` equals `git diff --ignore-cr-at-eol --numstat`, and no touched file holds a CR.
  The fixture's attributes are `text: set` and `eol: lf`.
- No development run exceeded its gate's effective deadline.

## Artifacts

All of these are under `specs/0050/.artifacts/`:

- `0454-ptb-source/Co.txt`: PTB's live record, which the fixture was checked against;
- `0454-dev-build.log`;
- `0454-dev-material-import-class.log`;
- `0454-dev-constructor-unit-tests.log`;
- `0454-dev-elevation.log`;
- `0454-dev-kit-acceptance.log`.

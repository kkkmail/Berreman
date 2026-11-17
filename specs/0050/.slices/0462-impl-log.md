# Step 0462 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 and the slice.
  - None of the rulings names this step or `MaterialImport.fs`; they are preserved by leaving their files untouched.
- Read the ELEV analyzer sources (STL `Analyzers/`, read-only) for ELEV009 / ELEV010 / ELEV001. Both signature rules
  check members (see Gotchas).
- **Storage** (`MaterialImport.fs`):
  - `AgfFormulaNumber` (`.value`, `.family`) and `GlassCatalogueText` (`.value`, `ofBytes`) are declared beside the
    other source texts.
  - `micrometerInterval` and `unrangedInterval` come out of `RiiPage.rangeInterval`, which now calls them; its
    behaviour is unchanged.
  - `FormulaCoefficients` is public, with its pairing moved to the private `CoefficientPair.pairsOf`. The five
    lowering call sites are rewritten one for one.
  - `entryOfAxis` takes the name and description; the RII page passes its old strings unchanged.
  - The `AgfFormulaNumber.riiFormOf` extension follows `entryOfAxis`, and `GlassCatalogueText.importGlasses` follows
    the CXRO reader. The module doc names the new reader.
- The Storage project built clean on its own. The first try failed with FS3373: a `" "` literal inside an
  interpolation hole. It was fixed by binding the joined text to a `let`.
- **Tests:**
  - The fixture `fixtures/test-glasses.agf` is added as a `Content` item after `ptb-Co.txt`'s.
  - The three facts are appended to `MaterialImportTests`, under the census names.
  - The facts write the AGF equations out themselves, and every coefficient by hand.
- **Full solution build:** green, 49 s. The only warnings are `NU1902`.
- **Filtered class run:** `MaterialImportTests` passed, 26 of 26 (the 23 existing facts plus the 3 new ones).
- **Mutation check** (reverted): formula 9's `c 1 - 1.0` was changed to `c 1`.
  - The first fact failed exactly where it should: `TEST-F9 at 0.5876 µm: n² <3.327…>, want <2.327…>`.
  - The file was restored from the copy in `.artifacts/0462-scratch/` and rebuilt. The class passed 26 of 26
    again, with the copied Storage DLL checked to be the rebuilt one.
- **constructor-unit-tests (whole suite):** 1831 of 1831, 106 s.
- **elevation:** `ELEV-GATE: OK`, 0 new findings in 13 projects, 241 s.
- **invariants:** all 21 hold.
- **kit-acceptance:** run in the background (see Testing state).
- **Two local renames in `MaterialImport.fs`, after kit-acceptance:**
  - `head` → `prefix` in `ofBytes`, because it is the first bytes, not a list head.
  - The inner `read` → `imported` in `importGlasses`, because it shadowed the outer `read`.
  - Then: rebuilt; the constructor suite passed 1831 of 1831; elevation was OK; the invariants were re-run.

## Files modified

All four are the step's `effects.edits`:

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`, one `Content` item with no
  comment
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/fixtures/test-glasses.agf` (new, ASCII, LF)

## Testing state

All runs were local development runs. The supervisor's gate engine is the authority.

- **build** (`dotnet build Berreman.slnx -c Release`): green.
  - The first full build took 49 s and the final incremental one 12 s, against a 600 s deadline.
  - It produced 0 errors; the only warnings are 12 lines of `NU1902`, which is exempt.
- **constructor-unit-tests** (whole suite, as the gate's `all_discovered` selection): green on the final code, 1831
  of 1831, 108 s against a 180 s deadline.
  - That is checkpoint 0461's 1828 plus 3, as the acceptance requires.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` on the final code, 232 s against a 600 s
  deadline. Known findings are 0 and suppressed findings 1759 (see Gotchas, ELEV004).
- **kit-acceptance** (`--filter-trait "Category=kit-acceptance"` and the `KitAcceptanceTests` class): `Passed!`, 14
  succeeded and 1 skipped, 1436 s against a 3600 s deadline.
  - The skip is the xUnit-Explicit five-seed ensemble ("Not run (due to explicit test filtering)").
  - AT-1 and AT-2's expectations and tolerances are untouched. It ran on the code before the two local renames,
    which change no behaviour.
- **invariant-s44-\* (21):** each command decoded and run from the repo root; all 21 find nothing on the final tree.

## Artifacts

- `specs/0050/.artifacts/0462-dev-build.log` (final build)
- `specs/0050/.artifacts/0462-dev-constructor-unit-tests.log` (final run)
- `specs/0050/.artifacts/0462-dev-elevation.log` (final run)
- `specs/0050/.artifacts/0462-dev-kit-acceptance.log`
- `specs/0050/.artifacts/0462-dev-invariants.log`
- `specs/0050/.artifacts/0462-scratch/access.fsx` (the FS0491 private-member check)

## Gotchas

- **Deviation 1: `riiFormOf` takes and returns `FormulaCoefficients`.** The how_to writes
  `riiFormOf (c : float array) : Result<RiiFormulaNumber * float array, ImportError>`. That signature fails two ELEV
  rules the gate enforces, so the existing numeric carrier is used instead.
  - **ELEV009** (`NoPrimitiveOnResultHappyPath.fs`): only module-level functions and members are checked, and no
    boundary exempts them. `Tree.primitivesIn` (`ElevationAnalyzer.fs:1552`) walks into `float[]` and reports
    `float`; it stops only at a `[<NumericCarrier>]`.
  - **ELEV010** (`NoPrimitiveBesideElevated.fs`): members are checked. The parameter `float array` sits beside the
    elevated `RiiFormulaNumber` and `ImportError`.
  - The carrier is the existing `[<NumericCarrier>]` `FormulaCoefficients`, so nothing new is invented.
  - It had to become public. Its member `coefficientPairs` returned the private `CoefficientPair`, and a public type
    cannot expose a private one (FS0410); a private member is not visible to the module's lowering functions
    (FS0491, checked in a scratch script). So the pairing became `CoefficientPair.pairsOf`, a static member of the
    private pair. Each of its five call sites gets the same list as before.
- **Deviation 2: `ofBytes` takes STL's `FileByteContent`**, not `byte[]`.
  - ELEV010 reports `byte[]` beside the elevated `GlassCatalogueText`.
  - `ofBytes` is not among the analyzer's `elevationPatternMembers` (`ElevationAnalyzer.fs:656`). `ofText` is,
    which is why `FileStem.ofText (text : string)` passes.
  - `ContentHash.ofBytes` already takes `FileByteContent`. Step 463 hashes the same bytes through it, so it holds a
    `FileByteContent` anyway.
- **Reuse refactors, both inside the edited file:**
  - `entryOfAxis` takes the entry's name and description instead of the RII formula number. The how_to asks for "the
    formula import's entry assembly … named by the glass". The RII page binds its two old strings to `let`s, so its
    entries are unchanged.
  - The µm-bounds interval and the zero nominal interval moved out of `RiiPage.rangeInterval` into
    `micrometerInterval` / `unrangedInterval`. The `LD` line and the page then share one body, as the how_to
    requires ("as the page range is", "as a page without a range does").
- **Interpretation: a glass with no `CD` line is refused** ("it has no CD line"), and so is one with two `CD` or two
  `LD` lines.
  - "Absent ones zero" covers coefficients missing from a `CD` line. A glass with no dispersion data at all would
    otherwise import as n = 0.
  - An `LD` line takes its first two numbers, as `rangeInterval` does.
- **Interpretation: the formula number is parsed as a double that `Double.IsInteger` accepts**, within the `int`
  range. So `2` and `2.0` read, and `2.5` refuses the glass.
- **The first fact also covers Sellmeier 3 (formula 6)** among its test-local glasses, beside the eight the how_to
  lists. It also checks the three fixture glasses' category and description strings.
- **The second fact also checks two more encodings** beyond the three the how_to names: UTF-16 big-endian with its
  mark, and the raw UTF-8 file without one. Both are decoder branches this step adds. It also checks TEST-SCHOTT's
  0.36–1.5 µm range.
- **Refusal text.** A glass the lowering refuses reads, for example: `TEST-CONRADY (line 9): AGF formula 5 (Conrady)
  does not lower to term data: formula 5 is not supported: exponent -3.5 is not an integer — not a finite term sum`.
  The AGF number is named first, so the RII "formula 5" that follows is not mistaken for it.
- **Measured-only ELEV004:** two new `.value` unwraps, one at the decode line (`bytes.value`) and one where a glass
  name enters the refusal text. ELEV004 is excluded from the gate (R7), and the suppressed count rose from 1757 to
  1759.

# Step 0270 — impl log

## Progress

1. Read the task, both system prompts, the project prompt, the slice spec and the `.gates` snapshot. No operator
   note was in flight. Then read the code this step builds on:
   - step 269's SoW and impl-log;
   - `NamingTemplates.fs` (`NameText`, `NameReading`, `readName`) and `Kit.NamingTemplate.standard`;
   - `Setups.SlotRole` / `SlotPosition`;
   - `MuellerReconstruction.fs`, from the label type through `buildDesign`;
   - `MuellerReconstructionTests.fs` (the `parseExperiment …` fact, the `buildDesign` fact and the §7.2 fact);
   - `NamingTemplateTests.fs`.
2. `git grep` over `Berreman/` found the retired symbols' only readers: each other, `buildDesign`, and
   `MuellerReconstructionTests.fs:563` / `:946`. That matches the census. The `family` member in
   `Storage/MaterialImport.fs:47` is on another receiver, so ELEV019 does not apply.
3. Tests first:
   - `NamingTemplateTests.fs` opens `MuellerReconstruction` and gained
     `a lab label's reading names its reference family, matrix kind and object angle`, with the five labels;
   - `MuellerReconstructionTests.fs` opens `Kit` and `NamingTemplates`; its fact was renamed to the successor name
     and reads through the template; `prepareRow` takes `phi` from the reading.
   - **Red**, measured in `0270-red-check-build.log`: both test projects failed with FS0039 on
     `MuellerExperimentLabel.nameText` and on `NameReading.family` / `matrixKind` / `objectPhi` / `objectRotation`.
4. Production code in `MuellerReconstruction.fs`:
   - **Deleted:** `open System.Text.RegularExpressions`, `experimentRegex`, `objectRegex`, the
     `MuellerExperimentLabel` extension holding `parsed`, `ParsedExperiment`, `ParsedObject` and
     `MuellerLabelToken`, with their docs. "The experiment-label parser" also came out of the section header.
   - **Opened** `Kit`, `Setups` and `NamingTemplates`.
   - **`MuellerExperimentLabel`:** its doc now says it reads through `NamingTemplate.standard`. It gained
     `member this.nameText : NameText`.
   - **After `MatrixKind`:** `type NameReading with` holding `family`, `objectRotation`, `matrixKind` and `objectPhi`,
     moved from `ParsedExperiment` with the same rules.
   - **`buildDesign`** keeps a row only when `NamingTemplate.standard.readName r.experiment.nameText` reads with
     `matrixKind = kind`. A refusal is never kept, and the other filters are unchanged. Its doc says so.
5. **Green:** the solution builds with 0 warnings and 0 errors. The selected class passes 29/29 and the constructor
   suite 1440/1440.
6. **Development evidence** (`0270-lab-labels.fsx` / `.log`): every distinct label in the lab's CSVs (the 55 of
   `0269-lab-labels.txt`) was read through the template, and its `family` / `matrixKind` / `objectPhi` compared
   with the retired regex rules, restated in the script.
   - **47 read, and all 47 agree.**
   - **8 are refused:** `Darkness`, `BullshitCheck`, `BullshitCheck-2` and the five `…-(Darkness)-…` labels. Every
     one of them read as `Air` under the regex, and no design selects `Air` (the pipeline builds `Qz`, `Lr` and
     `QzLrProduct`). So every design holds the same rows in the same order.
   - The §7.2 fact confirms it numerically: it still passes at `dataMatrixTol = 1e-9` against the reference
     pipeline.
7. Ran each gate's command over the final tree (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs` (+51 / −94)
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` (+17 / −8)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/NamingTemplateTests.fs` (+16)

These are exactly the three edits the step authorizes. No project file changed.
`specs/0050/.manifest.state.json` was already modified when the round started (it is the supervisor's file); this
round did not touch it.

## Decisions

- **`MuellerReconstructionTests` also opens `OpticalConstructor.Domain.NamingTemplates`**, beside the `Kit` open the
  slice names. `readName` is an optional extension declared in that module (`type NamingTemplate with` in
  `NamingTemplates.fs`), so it is invisible without the open. Both opens sit before `MuellerReconstruction`'s, so
  that module's names keep winning.
- **One class-level helper, `standardReading`.** It reads a label through
  `NamingTemplate.standard.readName label.nameText` and fails with the refusal's `describe`. Both the renamed fact
  and the §7.2 `prepareRow` call it, as the slice's "in the same way" asks, instead of repeating the match (the
  "reuse, never copy" rule). It sits before the first member (FS0960).
- **`nameText` destructures its own union** (`let (MuellerExperimentLabel l) = this in NameText l`), the form
  `Dispersion.fs:227` uses. It never re-wraps `.value` (ELEV005).
- **`objectRotation`** keeps the Obj parts that have an angle at their position. It sorts them by `SlotPosition`,
  using the union's structural comparison, and matches every other `SlotRole` case explicitly (no wildcard).
  `family` asks `tryFindPart` for Prep 1 and Analyzer 1 through a local `isCircular` over `SlotRole`.
- **The new fact** asserts `objectRotation` as well as its two halves, since `objectRotation` is one of the four
  contract symbols created. It uses a local `reads` helper rather than a four-element tuple (AGENTS.md tuple rule).

## Testing state

No gate verdict is claimed (Invariant 6). These are development runs of each gate's own command over the final tree,
with logs in `specs/0050/.artifacts/`:

- **build** (`0270-dev-build.log`): exit 0, 0 warnings, 0 errors, 53 s (deadline 600 s).
- **elevation** (`0270-dev-elevation.log`): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0,
  204 s (deadline 600 s). The run had no comparison base, so it covered all 12 projects.
- **constructor-unit-tests** (`0270-dev-constructor-unit-tests.log`): `Passed!`, 1440 succeeded, 0 failed, 0 skipped,
  69 s (deadline 180 s).
  - The count was 1439 at step 0269, so it rises by 1.
  - `a lab label's reading names its reference family, matrix kind and object angle` is `Passed` in the TRX.
- **unit-tests**, selected class `BerremanTests.MuellerReconstructionTests` (`0270-dev-unit-tests.log`): `Passed!`,
  29 succeeded, 0 failed, 0 skipped, 4 s (deadline 7200 s). The TRX shows these three facts `Passed`:
  - the renamed fact;
  - the `buildDesign … (always-run)` fact;
  - the section 7.2 fact.
- **The fifteen s27 invariants** (`0270-dev-invariants.log`) each found nothing. So did the acceptance grep
  `git grep -n -F "experimentRegex" -- Berreman/`, and a grep for every retired symbol in the two touched files.
- Every touched file is LF (`git ls-files --eol`: `i/lf w/lf`), and `git diff --numstat` equals
  `--ignore-cr-at-eol`.

## Artifacts

- `0270-red-check-build.log`: the red build (FS0039 on the five missing members).
- `0270-dev-build.log`, `0270-dev-elevation.log`, `0270-dev-constructor-unit-tests.log`, `0270-dev-unit-tests.log`,
  `0270-dev-invariants.log`: the development runs.
- `0270-lab-labels.fsx` / `0270-lab-labels.log`: the template reading against the retired regex rules, over every
  lab label.

## Gotchas

- **Darkness labels.** `…-(Darkness)-…` labels (and `Darkness` / `BullshitCheck…`) are refused by the template. The
  regex read them as `Air`, and `buildDesign` never keeps them now. They never reached a design, because no design
  selects `Air`.
  - `reduceAirTraces` and `airRowsOf` in the tests still select AIR rows by the label text `(AIR#0)`, as before.
- **A test that reads labels must open `NamingTemplates`.** The members on `NameReading` live in
  `MuellerReconstruction`, so a reader opens that module too; `readName` lives in `NamingTemplates`.
- **Isolated test-project builds.** `dotnet build` of a single test project in this tree printed MathNet's
  SYSLIB0051 warnings (`0270-red-check-build.log`). The solution build the gate runs printed 0 warnings.

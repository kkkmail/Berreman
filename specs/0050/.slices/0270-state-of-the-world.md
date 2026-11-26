# Step 0270 — state of the world

## Where we are

This is step 0270 of spec 0050, the fourth step of section S8.3 (the naming template and the grid generator).
- Step 0267 stored the kit's naming template as text.
- Step 0268 gave it a grammar.
- Step 0269 read and wrote names through it.

This step makes the reference Mueller pipeline read its lab labels through `NamingTemplate.standard` and deletes
the label regular expressions (001 §J, §R, §T). Matching name parts to kit items and the grid generator come later
in S8.3.

## What's working

- The reference pipeline reads its labels through `NamingTemplate.standard`. `experimentRegex`, `objectRegex`,
  `MuellerExperimentLabel.parsed`, `ParsedExperiment`, `ParsedObject` and `MuellerLabelToken` are deleted.
- Add `MuellerExperimentLabel.nameText`, and move the reference reading rules onto `NameReading` as `family`,
  `objectRotation`, `matrixKind` and `objectPhi`. The rules are unchanged.
- `buildDesign` keeps a row only when its label reads with the requested matrix kind. All 47 lab labels that read
  agree with the old parse, so every design holds the same rows.
- Rename the `MuellerReconstructionTests` label fact to its naming-template successor. Its numbers are unchanged,
  and the §7.2 fact still reproduces the reference pipeline at 1e-9.
- `NamingTemplateTests` gains `a lab label's reading names its reference family, matrix kind and object angle`;
  constructor-unit-tests 1439 → 1440.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. Development runs used each gate's
own command over the final tree, with logs in `specs/0050/.artifacts/0270-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 53 s (deadline 600 s).
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 204 s (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1440 succeeded, 0 failed, 0 skipped, 69 s (deadline 180 s). The count rose
  by 1, and the new fact is `Passed` in the TRX under its exact name.
- **unit-tests** over `BerremanTests.MuellerReconstructionTests`: `Passed!`, 29 succeeded, 0 failed, 4 s (deadline
  7200 s). The TRX shows these facts `Passed`:
  - the renamed fact
    (`the standard naming template reads CPL-(QZ#90-LR#90)-CPL as the CPL-CPL family and the QZ LR product at phi 90,
    and the +90 / -90 object rotation is inert (R(2 phi) invariance)`);
  - the `buildDesign … (always-run)` fact;
  - the section 7.2 fact.
- **The fifteen s27 invariants** find nothing, and neither does `git grep -n -F "experimentRegex" -- Berreman/`.
- **Red, measured** (`0270-red-check-build.log`): with the tests in place and no production change, both test
  projects failed with FS0039 on `nameText`, `family`, `matrixKind`, `objectPhi` and `objectRotation`.
- Every touched file is LF. No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1440
  berreman_unit_tests: 29
```

## Architecture

- **The reference pipeline's view of a label lives on `NameReading`.** It is an extension in
  `MuellerReconstruction`, which owns `Family` and `MatrixKind`. The label itself only offers its text as a
  `NameText`. Reading is the template's job (`readName`), and classifying a reading is the pipeline's.
- **`family`** looks at the Prep 1 and Analyzer 1 parts: either is circular when its text contains `CPL`, and an
  absent part is linear.
- **`objectRotation`** takes the Obj parts that have an angle, in `SlotPosition` order:
  - none: AIR at 0;
  - one: its angle and its kind (`QZ` / `LR` upper-cased, else `Air`);
  - two or more: the first's angle and `QzLrProduct`.
- **A label the template refuses is never kept by `buildDesign`.** There is no fallback reader.

## Deferred

Nothing. Matching name parts to kit items (`UnknownNameItemErr`) and the grid generator are later S8.3 steps by the
spec's own plan. They are not part of this step's scope.

## Gotchas

- **Darkness labels.** The template refuses the `…-(Darkness)-…` labels, `Darkness` and `BullshitCheck…`. The old
  regex read them as `Air`. They were never in a design, since none selects `Air`. Dark and diagnostic data are
  still loaded by data set (`DarknessChecks`, `BullshitChecks`), never by reading their labels.
- **Two opens to read a label.** A file that reads labels needs:
  - `NamingTemplates`, for `readName`, an optional extension;
  - `Kit`, for `NamingTemplate.standard`;
  - `MuellerReconstruction`, for the `NameReading` members.
- **`MuellerReconstructionTests.standardReading`** is the one place that test class reads a label. A refusal fails
  the test with its `describe`.

## Changelog

- 2026-11-10 — step 0270: the reference pipeline reads its labels through `NamingTemplate.standard`. The label
  regexes and `ParsedExperiment` were retired, and `NameReading.family` / `objectRotation` / `matrixKind` /
  `objectPhi` added. Constructor-unit-tests went 1439 → 1440, and `MuellerReconstructionTests` holds 29 with its
  numbers unchanged.

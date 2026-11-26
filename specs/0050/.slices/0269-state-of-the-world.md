# Step 0269 — state of the world

## Where we are

This is step 0269 of spec 0050, the third step of section S8.3 (the naming template and the grid generator). Step
0267 stored the kit's naming template as text, and step 0268 gave it a grammar (`NamingTemplate.pieces`). This step
adds the values a name holds and both directions over the text: `writeName` writes a reading into a name, and
`readName` reads a name back into its reading. The lab's labels, today read by `MuellerExperimentLabel.parsed`, are
its first cases. Step 270 re-points `parsed`'s callers here. Matching names to kit items (`UnknownNameItemErr`) and
the grid generator come later in S8.3.

## What's working

- Add `NameText`, `SlotPart`, `ObjectAngle` and `NameReading` (with `tryFindPart` / `tryFindAngle`) to
  `NamingTemplates.fs`: the values a name holds, in its template's order.
- Add `NamingTemplate.writeName`: each value is written after the text its placeholder owns. An optional value the
  reading lacks is left out with that text, and a missing required one answers `MissingNameValueErr`. Angles are
  written as degrees to three decimals (`45`, `-22.5`, `0.333`, `0`).
- Add `NamingTemplate.readName`: it tries the ways of leaving out optional groups in binary-count order, all kept
  first. The first way that reads answers; when none does, the answer is `NameMismatchErr` naming the name and the
  template.
- The lab's labels read as `parsed` reads them, and write back unchanged. Of the 55 distinct labels in the lab's
  CSVs, all 47 that carry an object agree with `parsed` field by field.
- `NamingTemplateTests` gains the four census facts; constructor-unit-tests 1435 → 1439.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. The development runs below used each
gate's own command over the final tree, with logs in `specs/0050/.artifacts/0269-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 26 s (deadline 600 s).
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 196 s (deadline 600 s). The
  scope was all 12 projects, since a standalone run has no comparison base.
- **constructor-unit-tests:** `Passed!`, 1439 succeeded, 0 failed, 0 skipped, 74 s (deadline 180 s).
  - The count is 1435 at step 0268, so it rises by 4, as the acceptance requires.
  - Every census identity is `Passed` in `TestResults/constructor-unit-tests.trx` under its exact name:
    - `the lab's labels read into their source, objects, angles, analyzer and note`;
    - `a name that does not fit the template is refused naming it`;
    - `writing a reading back gives the name it was read from`;
    - `a reading writes each value in its place and leaves out an optional one it lacks with the text it owns`.
- **The fifteen s27 invariants** each exit 0 having found nothing (`0269-dev-invariants.log`).
- **Red, measured** (`0269-red-check-build.log`): with the tests in place and no production code, the build failed
  with FS0039 on the missing names:
  - the types `NameText` and `NameReading`;
  - the record labels `parts`, `angles` and `noteOpt`;
  - the members `readName` and `writeName`.
- Every touched file is LF; `git diff --numstat` equals `--ignore-cr-at-eol`. No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1439
```

## Architecture

- A reading is the name's values at the level of its text:
  - `SlotPart` (a `SlotRole` and its `NamePart`);
  - `ObjectAngle` (an Obj slot's `SlotPosition` and its `Angle`);
  - the note.

  Matching parts to kit items (aliases, `UnknownNameItemErr`) is not part of reading text.
- Both members derive what a placeholder owns from `pieces` (step 268), so the template's text is parsed in one
  place. The scanning is done by locals inside the two members, with no regex, cache or extra member, as the slice
  prescribes.
- Reading follows the slice's fixed order of ways, generated lazily: all groups kept first, an earlier group kept
  longest. There is no backtracking within a way, so a reading is decided by the first way that reads.
  `obj<k>` / `phi<k>` form one optional group; every other optional placeholder is a group alone.
- `writeName` and `readName` sit in a second `type NamingTemplate with` block after the new types, because the slice
  places those types after step 268's declarations.

## Deferred

Nothing. Step 270 re-points `MuellerExperimentLabel.parsed`'s callers to `readName`, and step 271 matches parts to
kit items. Those are separate S8.3 steps, not this step's scope.

## Gotchas

- `parsed` and `readName` differ on the five `…-(Darkness)-…` labels:
  - `parsed` drops the `Darkness` token and reads source, analyzer and note with no objects, which
    `objectRotation` then treats as an AIR row at φ = 0;
  - `readName` refuses those labels, because `{obj1}#{phi1}` is required. The slice lists `LP-(Darkness)-CPL2` among
    the refusals.

  Step 270 should expect these rows as `NameMismatchErr`.
- `writeName` drops each optional placeholder it lacks on its own, as the slice says. A reading with `obj2` but no
  `phi2` therefore writes a name the template does not read back.
- A way does no backtracking. With a template ending in `.csv`, the name `….csv.csv` is refused, because the value
  runs to the first `.csv`.
- .NET formats a negative zero as `-0`, so the writer folds a rounded zero to `0.0` before formatting.
- An `.fsx` probing the Domain needs `Softellect.Berreman.OpticalConstructor.Domain.dll`, plus
  `Softellect.Berreman.Core.dll`, `MathNet.Numerics.dll` and `Softellect.Sys.dll`.

## Changelog

- 2026-11-10 — step 0269: names read through the naming template and written back (`NameText`, `SlotPart`,
  `ObjectAngle`, `NameReading`, `NamingTemplate.writeName` / `readName`); constructor-unit-tests 1435 → 1439.

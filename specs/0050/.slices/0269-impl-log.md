# Step 0269 — impl log

## Progress

1. Read the task, both system prompts, the project prompt, the slice spec and the `.gates` snapshot. No operator
   note was in flight. Then read the code the step builds on:
   - step 268's SoW and impl-log, `NamingTemplates.fs` and `NamingTemplateTests.fs`;
   - `Kit.NamingTemplate.standard`, `Setups.SlotRole` / `SlotPosition` and `Geometry.Angle` (`degree`, `degrees`);
   - `MuellerReconstruction.fs:parsed`, with `experimentRegex` and `objectRegex`.
2. `git grep -w` over Berreman and STL found none of the new names (ELEV014 / 015 / 016 / 019): `NameText`,
   `SlotPart`, `ObjectAngle`, `NameReading`, `tryFindPart`, `tryFindAngle`, `writeName` and `readName`.
3. Read two of STL's analyzer sources (read-only):
   - ELEV001 (`NoCallSiteRewrap`) only flags `Target source.value` across two single-case unions;
   - ELEV012 (`NoTwoCaseDuToBool`) reads members only.

   The scanning locals are outside both rules.
4. Tests first. The four census facts were appended to `NamingTemplateTests.fs`, with `open Berreman.Constants` and
   `open Berreman.Geometry`.
   - **Red** (`0269-red-check-build.log`): the test project's build failed with FS0039 on the missing names:
     - the types `NameText` and `NameReading`;
     - the record labels `parts`, `angles` and `noteOpt`;
     - the members `NamingTemplate.readName` and `writeName`.
5. `NamingTemplates.fs` gained `open Berreman.Constants` and `open Berreman.Geometry`. Appended after step 268's
   declarations:
   - `NameText` (`.value`), `SlotPart`, `ObjectAngle`;
   - `NameReading`, with `tryFindPart` and `tryFindAngle`;
   - a second `type NamingTemplate with` block holding `writeName` and `readName`.
6. **Green**: the test project built with 0 warnings, and `NamingTemplateTests` passed 9/9 (5 from step 268, 4 new)
   on the first run.
7. Development evidence:
   - **Every distinct label in the lab's CSVs** (`Berreman/Data/MuellerMatrix/data.zip`, 55 labels) was read
     through `NamingTemplate.standard`, compared with `MuellerExperimentLabel.parsed` and written back
     (`0269-lab-labels.fsx` / `.log`).
     - 47 labels read. For each one, the source, the objects and angles, the analyzer and the note equal `parsed`'s,
       and `writeName` gives the label back exactly.
     - The 8 refused (`NameMismatchErr`) are the rows with no `NAME#angle` object: `Darkness`, `BullshitCheck`,
       `BullshitCheck-2` and the five `…-(Darkness)-…` labels.
   - **A template with a leading literal and a tail** (`exp_{source}_{obj}@{phi}_{note}.csv`) reads and writes back
     with or without the note, and with `-0.5` (`0269-tail-cases.fsx` / `.log`). It refuses:
     - `1e3`, `.5` and `5.` as angles;
     - a missing leading literal;
     - a doubled tail (`….csv.csv`), since a way does no backtracking.
8. Development runs of each gate's command over the final tree are listed below.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/NamingTemplates.fs` (+169)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/NamingTemplateTests.fs` (+121)

These are exactly the two edits the step authorizes. No project file changed, since both files are already compiled.
`specs/0050/.manifest.state.json` was already modified when the round started (the supervisor's); it was not touched.

## Decisions

- **The new types sit after step 268's last declaration**, as the slice asks. `writeName` and `readName` are
  therefore a second `type NamingTemplate with` block, because they name `NameReading` and `NameText`, which are
  declared after the first block. They are the only members with those names on the receiver (ELEV019).
- **Ownership.** A placeholder owns the literal text pending since the previous placeholder. An empty pending text
  is owned too (the first placeholder of the standard template owns `""`). The text after the last placeholder is
  the tail.
- **Groups.** The optional placeholders are grouped with `List.groupBy`, which keeps keys in first-appearance
  order:
  - `obj<k>` and `phi<k>` share the key `Choice1Of2 position`;
  - every other optional placeholder is its own key (`Choice2Of2 placeholder`);
  - a required placeholder is in no group, so it is always kept.
- **The order of ways** is a lazy recursive sequence:
  - each group's ways are every way of the later groups with this group kept, then every way with it left out;
  - this is exactly binary counting over the groups, with the last group the lowest digit and a left-out group a
    one;
  - it has no integer width to overflow, however many groups a template has.

  `Seq.tryPick` stops at the first way that reads.
- **Reading one way.**
  - The owned text must stand at the reading point (ordinal `StartsWith`).
  - The value runs to the following text, found by an ordinal `IndexOf` from one character after the value's start.
    That following text is the next kept placeholder's owned text, or the tail. When it is empty, the value runs to
    the end of the name.
  - A value that would start at the end of the name is refused (never empty).
  - After the last value, the rest of the name must equal the tail ordinally.
  - Comparisons are case-sensitive, as `parsed`'s regex is.
- **An angle reads** when the value is an optional `-`, at least one ASCII digit, then optionally a `.` and at least
  one ASCII digit. That number then goes through `Double.TryParse` (`AllowLeadingSign | AllowDecimalPoint`,
  invariant culture) into `Angle.degree (degrees * 1.0<deg>)`.
  - A shaped number so long that it parses to infinity is refused as no angle, by `Double.IsFinite`. Such a value is
    not "that number", so an infinite `Angle` is never made.
- **An angle is written** as `Math.Round(angle.degrees, 3)`. A rounded zero becomes `0.0`, because .NET formats a
  negative zero as `-0`. The result is formatted with `ToString("0.###", CultureInfo.InvariantCulture)`, which has
  no exponent and no trailing zero.
- **Writing is per placeholder**, as the slice says: an optional value the reading lacks is left out with its owned
  text, one placeholder at a time. A reading with `obj2` but no `phi2` therefore writes a name its own template does
  not read back. That is the slice's rule as written, so it is not "fixed" here.
- **Reasons.**
  - `MissingNameValueErr "'analyzer1' of '<template>'"` names the placeholder's `text`.
  - `NameMismatchErr "'<name>' against '<template>'"` names the name and the template.
  - Rendered, they read "the name has nothing for a placeholder it needs: 'analyzer1' of '…'" and "the name does not
    fit the naming template: 'Darkness' against '…'".
- **No regex, cache, extra member or backtracking** within a way. The scanning is done by locals inside the two
  members, as the slice prescribes (ELEV003P, ELEV010P).

## Testing state

No gate verdict is claimed (Invariant 6). Development runs used each gate's own command over the final tree; the
logs are in `specs/0050/.artifacts/0269-dev-*.log`.

| Gate | Outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 26 s / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 (all 12 projects, no base) | 196 s / 600 s |
| constructor-unit-tests | `Passed!`, 1439 succeeded, 0 failed, 0 skipped (1435 at step 0268, +4) | 74 s / 180 s |
| fifteen s27 invariants | each exit 0, found nothing (`0269-dev-invariants.log`) | — |

- All four census identities are `Passed` in `TestResults/constructor-unit-tests.trx` under their exact names, and
  so are step 268's five.
- Both touched files were tracked before this step, so `git grep` (the invariants) sees them.
- Both touched files are LF (`git ls-files --eol`: `i/lf w/lf`), and `git diff --numstat` equals
  `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0269-red-check-build.log` — the red build, before the production code
- `specs/0050/.artifacts/0269-green-build.log` — the test project's green build
- `specs/0050/.artifacts/0269-dev-build.log`, `0269-dev-constructor-unit-tests.log`, `0269-dev-elevation.log`,
  `0269-dev-invariants.log`
- `specs/0050/.artifacts/0269-lab-labels.txt`, `0269-lab-labels.fsx`, `0269-lab-labels.log` — every lab label,
  read, compared with `parsed` and written back
- `specs/0050/.artifacts/0269-tail-cases.fsx`, `0269-tail-cases.log` — a template with a leading literal and a tail

## Gotchas

- **`parsed` and `readName` differ on the Darkness rows.**
  - `parsed` reads `CPL-(Darkness)-LP-2` (and the other four `…-(Darkness)-…` labels) as source `CPL`, analyzer
    `LP`, note `2` and no objects, because `objectRegex` silently drops `Darkness`. With no objects,
    `objectRotation` then calls the row an AIR calibration at φ = 0.
  - `readName` refuses those labels, because `{obj1}#{phi1}` is required. `LP-(Darkness)-CPL2` is among the refusals
    the slice lists.
  - Step 270, which re-points `parsed`'s callers here, should know that these rows become `NameMismatchErr`.
- The Domain assembly is `Softellect.Berreman.OpticalConstructor.Domain.dll`. An `.fsx` that `#r`s it also needs
  `Softellect.Berreman.Core.dll`, `MathNet.Numerics.dll` and `Softellect.Sys.dll`.
- A `bash` heredoc holding a Python triple-quoted string with F# code failed to parse in the Bash tool ("unexpected
  EOF while looking for matching `''"). The production edit was made with the Edit tool instead.

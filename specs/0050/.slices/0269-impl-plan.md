# Step 0269 — impl plan

A name read through the kit's naming template and written back, at the level of its text (spec 0050 S8.3; 001 §J).
Step 268 gave the template its grammar (`NamingTemplate.pieces`). This step adds the values a name holds and the two
directions: `writeName` (reading → text) and `readName` (text → reading). The lab's labels, read today by
`MuellerExperimentLabel.parsed`, are the first test cases. Step 270 re-points `parsed`'s callers here, so
`MuellerReconstruction.fs` is not touched in this step.

## Approach

1. `OpticalConstructor.Domain/NamingTemplates.fs`, appended after step 268's declarations:
   - `NameText of string` with `.value`;
   - `SlotPart = { role; part }`, `ObjectAngle = { position; angle }`;
   - `NameReading = { parts; angles; noteOpt }`, with `tryFindPart` and `tryFindAngle`;
   - a second `type NamingTemplate with` block holding `writeName` and `readName`.

   Each new doc comment is one to three lines and cites spec 0050 S8.3.
2. `writeName` folds over `pieces`, carrying the literal a placeholder owns:
   - a value present is written after its owned text;
   - an optional value that is missing drops its owned text;
   - a missing required value answers `MissingNameValueErr`, naming `placeholder.text`;
   - the tail ends the name.

   An angle is written as degrees: `Math.Round(_, 3)`, a negative zero folded to `0`, then
   `ToString("0.###", InvariantCulture)`.
3. `readName` uses only locals:
   - Split the pieces into (owned text, placeholder) entries plus the tail.
   - Group the optional placeholders: `obj<k>` and `phi<k>` form one group, and every other optional placeholder is
     a group alone. Groups follow template order.
   - Enumerate the ways to leave groups out, all kept first and the last group the lowest binary digit. This is a
     lazy recursive sequence: the order is binary counting, with no integer width to overflow.
   - Read each way left to right, with no backtracking:
     - the owned text must stand at the reading point;
     - the value runs to the next following text found at least one character on, or to the end when that text is
       empty;
     - a value is never empty, and an angle must read as `-?digits(.digits)?`;
     - the rest of the name must equal the tail.
   - The first way that reads answers. When none does, the answer is `NameMismatchErr` naming the name and the
     template.
4. Tests in `NamingTemplateTests.fs`: the four census facts, named exactly as the slice names them, over
   `NamingTemplate.standard`.

## Risks

- Record field names shared with earlier records:
  - `part` (`NameAlias`) and `role` (`RoledSlot`);
  - `position` and `angle` may also be in scope.

  Record construction is checked by the build. The tests annotate the record types they build.
- Negative zero formats as `-0` under .NET Core 3+, so the rounded value is compared with `0.0` and replaced by
  `0.0` before formatting.
- ELEV is checked on member signatures only, and these are elevated (`NameReading`, `NameText`, `SlotRole`,
  `SlotPosition`). Scanning stays in locals. No DU is interpolated into a string (ELEV002 / ELEV007).

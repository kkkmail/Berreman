# Step 0473 — impl-log

## Progress

- Read the task, the system prompts, the project prompt, operator rulings R0001–R0005 and the slice. The rulings bind
  steps 0427, 0428 and 0436 and ask later steps to preserve that behavior. This step touches none of their files.
- Mapped the moved block in `MuellerInverseTests.fs` (before the step):
  - lines 60–105: the ground-truth banner, `waveLength` … `quartz`;
  - lines 107–121: `solverParameters` and `forward`, which stay;
  - lines 123–206: the C1-C4 banner, `thickPlate` … `fullConfigurations`;
  - lines 208 onward: `observe` and everything after it, which stay.
- Created `Berreman/BerremanTestSupport/QuartzSample.fs`:
  - `namespace BerremanTestSupport` + `module QuartzSample =`, the same declaration form as `BiaxialSample.fs`.
  - A two-line module doc: spec 0044's quartz ground truth and its C1-C4 design, shared by `MuellerInverseTests` and
    the Planner's acceptance facts (spec 0050 S14.1).
  - The opens the step names: `Berreman.Constants`, `Berreman.Fields`, `Berreman.MaterialProperties`, `Berreman.Media`,
    `OpticalConstructor.Domain.CrystalPhysics`, `OpticalConstructor.Domain.MuellerInverse` and
    `OpticalConstructor.Optimization.MuellerFitting`. No `BerremanTests` name is read.
  - The body was copied by line range (`sed -n 60,105p` and `sed -n 123,206p`), so every byte is unchanged except the
    three edits the step names:
    - "Spec R3 puts the material in the TEST" became "in test code";
    - "(asserted by the ablation fact below)" became "(asserted by `MuellerInverseTests`' ablation fact)";
    - "The ablation fact below asserts" became "`MuellerInverseTests`' ablation fact asserts".
  - Checked by diffing the removed lines against the new body. Only those three lines differ, plus the blank line
    between the two blocks (`0473-removed-lines.txt` vs `0473-moved-lines.txt`).
- `BerremanTestSupport.fsproj`: appended `<Compile Include="QuartzSample.fs" />` after `MuellerReference.fs`, with no
  comment. No reference was added: Domain and Optimization are already referenced (steps 344 and 345).
- `MuellerInverseTests.fs`:
  - deleted both banners and the 14 moved declarations (−132 lines);
  - added `open BerremanTestSupport.QuartzSample` after `open BerremanTests.InverseFitHarness`, the last `open`;
  - left `degree`, `solverParameters`, `forward`, `observe` and every later binding and fact unchanged, along with the
    class doc.
- Build: green in 81 s.
- Elevation: OK in 227 s.
- 21 invariants: nothing found.
- `MuellerInverseTests` filtered: 20/20 in 361 s.

## Files modified

- `Berreman/BerremanTestSupport/QuartzSample.fs` (new, 145 lines)
- `Berreman/BerremanTestSupport/BerremanTestSupport.fsproj` (+1 / −0)
- `Berreman/BerremanTests/MuellerInverseTests.fs` (+1 / −132)

All three are the step's authorized edits. No other source file was touched.

## Testing state

Development runs only; the arc-runner's gate engine is the authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | 0 errors. Warnings are only `NU1902` (exempt) and vendored-MathNet `SYSLIB0051` (pre-existing, `MathNetNumerics/Numerics/Exceptions.cs`). No FS warning. | 81 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no comparison base, so all 13; ELEV004 excluded; suppressed 1767, the same as step 0472; known findings 0) | 227 s | 600 s |
| unit-tests (selected `BerremanTests.MuellerInverseTests`) | `Passed!` 20/20 (failed 0, skipped 0); the 20 facts are the step's 20 identities, so the count is unchanged | 361 s | 7200 s |
| kit-acceptance (selected `KitAcceptanceTests`) | not run locally | — | 3600 s |
| 21 × invariant-s45-* | each decoded `git grep` (also with `--untracked`, so the new file is searched) finds nothing. Positive control: `QuartzSample` finds 3. | < 5 s | 300 s |

Why kit-acceptance was not run locally:

- `KitAcceptanceTests` reads none of the moved names.
- Its only contact with this step is that `OpticalConstructor.Ui.Tests` compiles against the changed
  `BerremanTestSupport`, and the build covers that.
- The run takes about 30 minutes, so it is left to the gate engine. This follows the filtered-run discipline: run the
  touched class.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0473-build.log`
- `0473-elevation.log`
- `0473-invariants.log`
- `0473-unit-tests-mueller-inverse.log`
- `0473-removed-lines.txt` and `0473-moved-lines.txt`: the move check

## Gotchas

- **Shadowing.** The moved names were class-level `let`s, which shadowed every earlier `open` within the class.
  `QuartzSample` is opened LAST, so the facts resolve every name exactly as before.
- **ELEV.** `QuartzSample.configuration` keeps its two `double` parameters (`incidenceDeg`, `azimuthDeg`), as the step
  requires ("bodies and docs as S10.1 to S12.2 leave them"). This is ELEV-clean for two reasons:
  - the assembly `BerremanTestSupport` does not match `.elevation-analyzers.json`'s `inScopePatterns`
    (`^Softellect\.Berreman\.`);
  - elevation's suppressed count is unchanged at 1767.
- **The one long line.** The new file's longest line (164 characters) is that same signature, unchanged from the class.
- **Declaration form.** The step writes the module as `module BerremanTestSupport.QuartzSample` and also says "declared
  as `BiaxialSample.fs` is". I used `BiaxialSample.fs`'s form, `namespace BerremanTestSupport` + `module QuartzSample =`.
  It yields the same fully qualified name, `BerremanTestSupport.QuartzSample`.

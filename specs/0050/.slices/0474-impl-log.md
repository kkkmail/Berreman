# Step 0474 — impl-log

## Progress

- Read the task, the system prompts, the project prompt, operator rulings R0001–R0005 and the slice.
  - The rulings bind steps 0427, 0428 and 0436 and ask later steps to preserve that behavior.
  - This step touches none of their files, so they are preserved unchanged.
- Mapped the moved block in `BiaxialInverseTests.fs` (before the step):
  - lines 122–194: the measurement-set banner, then `plate`, `configuration`, `principalCuts`, `b1`, `b2`, `b3` and
    `fullConfigurations`;
  - line 195: the blank line after them;
  - lines 116–120: `forward` and `waveLength`, which stay;
  - lines 196 onward: `observe` and everything after it, which stay.
- Created `Berreman/BerremanTestSupport/BiaxialDesign.fs`:
  - `namespace BerremanTestSupport` + `module BiaxialDesign =`, the same declaration form as `BiaxialSample.fs` and
    `QuartzSample.fs`. The fully qualified name is `BerremanTestSupport.BiaxialDesign`.
  - A two-line module doc: spec 0044's biaxial measurement design for the triclinic sample, shared by
    `BiaxialInverseTests` and the Planner's acceptance facts (spec 0050 S14.1).
  - The opens the step names, and only those:
    - `Berreman.Constants` and `Berreman.Media`;
    - `OpticalConstructor.Domain.MuellerInverse`;
    - `OpticalConstructor.Optimization.MuellerFitting`;
    - `BerremanTestSupport.BiaxialSample`.

    No `BerremanTests` name is read.
  - The body was copied by line range (`sed -n 122,194p`), so the banner, the docs and the bodies are byte-identical,
    except the two edits the step names:
    - `configuration` passes `referenceWaveLength` where it passed the class alias `waveLength`, which held that value;
    - `b1` became `principalCuts |> List.collect normalIncidenceOn`.
  - Added `normalIncidenceOn (cut : SampleCut) : MeasurementConfiguration list`, between `principalCuts` and `b1`. It is
    the ablation fact's `normalOn`, with its body unchanged and a one-line doc. The azimuth list
    `[ 0.0; 22.5; 45.0; 67.5 ]` is now written once.
  - Checked by diffing the removed lines against the new body (`0474-removed-lines.txt` vs `0474-moved-lines.txt`).
    Only those two edits and the new `normalIncidenceOn` differ, plus the dropped trailing blank line.
- `BerremanTestSupport.fsproj`: appended `<Compile Include="BiaxialDesign.fs" />` at the end of the compile list (after
  `QuartzSample.fs`, so after `BiaxialSample.fs`), with no comment. No reference was added.
- `BiaxialInverseTests.fs`:
  - deleted the banner and the seven moved declarations (lines 122–195, −74 lines);
  - added `open BerremanTestSupport.BiaxialDesign` after `open BerremanTestSupport.BiaxialSample`, the last `open`;
  - in the ablation fact `the cuts buy the gyration DIAGONAL and only the tilt buys its OFF-DIAGONAL`, deleted the local
    `normalOn` and built `zOnly`, `zAndX` and `everyCutNormal` from `normalIncidenceOn`;
  - kept `degree`, `forward`, `waveLength`, `observe`, `perturbedStart`, `box`, `blindStart`, `wideBox`,
    `recoverFromNoisy` and every fact;
  - re-pointed three doc / comment references that named the moved banner by position, so they no longer dangle (see
    Gotchas).
- Build: green in 85 s.
- Elevation: OK in 224 s.
- 21 invariants: nothing found.
- `BiaxialInverseTests` filtered: 11/11 in 792 s.

## Files modified

- `Berreman/BerremanTestSupport/BiaxialDesign.fs` (new, 86 lines)
- `Berreman/BerremanTestSupport/BerremanTestSupport.fsproj` (+1 / −0)
- `Berreman/BerremanTests/BiaxialInverseTests.fs` (+7 / −83)

All three are the step's authorized edits. No other source file was touched.

## Testing state

Development runs only; the arc-runner's gate engine is the authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | 0 errors. Warnings are only `NU1902` (exempt) and vendored-MathNet `SYSLIB0051` (pre-existing, `MathNetNumerics/Numerics/Exceptions.cs`). No FS warning. | 85 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no comparison base, so all 13; ELEV004 excluded; suppressed 1767, the same as step 0473; known findings 0) | 224 s | 600 s |
| unit-tests (selected `BerremanTests.BiaxialInverseTests`) | `Passed!` 11/11 (failed 0, skipped 0). The class declares 11 facts before and after, the step's 11 identities, so the count is unchanged. | 792 s | 7200 s |
| kit-acceptance (selected `KitAcceptanceTests`) | not run locally | — | 3600 s |
| 21 × invariant-s45-* | each decoded `git grep` (also with `--untracked`, so the new file is searched) finds nothing. Positive control: `BiaxialDesign` finds 6. | < 5 s | 300 s |

Why kit-acceptance was not run locally:

- `KitAcceptanceTests` reads none of the moved names.
- Its only contact with this step is that `OpticalConstructor.Ui.Tests` compiles against the changed
  `BerremanTestSupport`, and the build covers that.
- The run takes about 30 minutes, so it is left to the gate engine, as at step 0473.

No run exceeded its deadline.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0474-build.log`
- `0474-elevation.log`
- `0474-invariants.log`
- `0474-unit-tests-biaxial-inverse.log`
- `0474-removed-lines.txt` and `0474-moved-lines.txt`: the move check

## Gotchas

- **Shadowing.** The moved names were class-level `let`s, which shadowed every earlier `open` within the class.
  `BiaxialDesign` is opened LAST, so the facts resolve every name exactly as before.
- **Sibling designs.** `AbsorbingBiaxialSample` (also in `BerremanTestSupport`) declares its own `plate`,
  `configuration` and `fullConfigurations`, and `FeasibleBiaxialInverseTests` declares its own `configuration` and
  `fullConfigurations`. Those are different designs, which the step keeps. Neither file opens `BiaxialDesign`, and
  `BiaxialInverseTests` opens neither of them, so no name is ambiguous.
- **`waveLength` is now unread.** The class alias `waveLength` stays, as the step requires, but none of the remaining
  bindings reads it. FS1182 (unused variables) is not enabled in this repository, so the build stays warning-free.
- **Re-pointed comments.** Three comments in the class named the moved banner by position. They were re-pointed at
  `BiaxialDesign`; no other comment changed:
  - `perturbedStart`'s doc: "the thickness commentary above" became "`BiaxialDesign`'s thickness commentary";
  - `blindStart`'s doc: "the thickness commentary" became "`BiaxialDesign`'s thickness commentary";
  - the ablation fact: "the measurement set above" became "`BiaxialDesign`'s measurement set".

  The step names no wording edit, but a comment pointing "above" at nothing is stale. Step 0473 made the same kind of
  edit for its quartz move.
- **ELEV.** `BiaxialDesign.configuration` keeps its two `double` parameters (`incidenceDeg`, `azimuthDeg`), unchanged as
  a MOVE requires. This is ELEV-clean because the assembly `BerremanTestSupport` does not match
  `.elevation-analyzers.json`'s `inScopePatterns` (`^Softellect\.Berreman\.`). Elevation's suppressed count is
  unchanged at 1767.
- **Bit-identity of `b1`.** `List.collect normalIncidenceOn principalCuts` concatenates each cut's four azimuths in cut
  order: Z then X then Y, each at 0, 22.5, 45 and 67.5 deg. That is exactly the order of the old nested comprehension,
  and every element is the same `configuration cut 0.0 azimuth TransmittedMueller` call.

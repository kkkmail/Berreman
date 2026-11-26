# Where we are

Step 0438 completes AT-2's reference kit (spec 0050 S12.2; 001 §Q AT-2, §S12, §1).

- Steps 0432–0437 build the absorbing-biaxial-noisy kit through the app's messages, fit seed 1's observations, and
  add the on-demand five-seed ensemble.
- This step commits that build's output once, as `data/absorbing-biaxial-noisy/`, and adds the fact that holds it equal
  to every later build, file by file.
- The comparison AT-1 already made is now one shared helper.

# What's working

- Commit the absorbing-biaxial-noisy reference kit (34 files) exactly as AT-2's build writes it, `.oc/` excluded.
- Add the fact `the built absorbing-biaxial-noisy kit equals the committed kit file by file`.
- Extract `assertSameKitFiles`; AT-1's equality and Send facts now compare through it.
- AGENTS.md's reference-kit gotcha names the AT-2 kit and its comparison fact.

# Tests

These are development runs, evidence only. The supervisor's gates decide. Each run used the gate's own command and
selection.

- **build:** exit 0 with 0 errors, 88 s against 600 s. The only warnings are the exempt NU1902 and the vendored
  MathNet's four pre-existing SYSLIB0051.
- **elevation** (ELEV004 excluded, all 13 projects): `ELEV-GATE: OK - 0 new finding(s)`, 260 s against 600 s. The
  code diff is test-only.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 894 s against 3600 s.
  - Total 13: 12 succeeded, 1 skipped, 0 failed. The count rises from 11 to 12.
  - The skip is the explicit five-seed ensemble ("Not run (due to explicit test filtering)", TRX `NotExecuted`), as
    at 0437.
  - The new fact passed, and so did AT-1's equality and Send facts through `assertSameKitFiles`.
  - That run's AT-2 fit took about 816 s, within R0005's 1800 s bound.
- **Red run first:** the new fact alone, before the kit existed, failed as designed in 841 s. It named the built
  folder and "expected the committed kit; actual folder missing". This is not a gate run.
- **Second build:** the class run built AT-2 again, independently. Its folder is byte-identical to the committed kit.
- **Nothing written into `data/`:** a sha256 manifest of `data/absorbing-biaxial-noisy/` and `data/optics-mueller/` is
  unchanged across the class run.
- **ui-tests** (`StudyViewTests`, `DataStepTests`): `Passed!`, 18 of 18, 30 s against 360 s.
- **ui-smoke** (all discovered): `Passed!`, 264 of 264, 222 s against 600 s.
- **The twenty-one s42 invariants:** each decoded `git grep` finds nothing.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  kit_acceptance_tests: 12
  ui_smoke_tests: 264
  ui_tests: 18
```

# Architecture

- **Code:** only `OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs` changed. No production code changed, and
  R0001–R0005 are preserved.
- **The helper:** `assertSameKitFiles (built : KitFolder) (other : KitFolder) : unit` is the one file-by-file
  comparison.
  - It takes both relative-path sets with `.oc/` excluded and walks their union in order.
  - It fails at the first difference. The message names the built folder's absolute path, the relative path, and
    whether the file is missing from the other folder, extra in it, or holds different bytes.
- **Its callers:**
  - AT-1's equality fact compares the built folder with the committed one.
  - The Send fact compares the built folder with the received one.
  - The new AT-2 fact compares the shared `absorbingBuild` with
    `CommittedKit.AbsorbingBiaxialNoisyKit.tryFindFolder`'s folder. With no committed folder, it fails naming the
    built folder.
- **The committed kit's contents:**
  - the kit file;
  - eight items, five setups, five experiments;
  - the study and its one fit run;
  - five `data/noisy-observations-seed-<n>.csv`;
  - eight `library/` snapshots.
- **Byte safety:** all files are LF-only UTF-8 without a BOM. Git's `* text=auto eol=lf` leaves every blob
  byte-identical, `git check-ignore` matches none of them, and `/data/*/.oc/` still ignores the kit's lock.

# Deferred

Nothing.

# Gotchas

- **Eight snapshots, not four.** The slice's file list names four `library/` snapshots, but the build writes eight.
  - The other four are the intensity-detector preset, which the reflected detector's `AddElement Detector` seed
    snapshots, the source-preset and stack-sample item models, and the crystal category.
  - Step 0433's `assertAbsorbingLibrary` already expects all eight.
  - The acceptance ("exactly … what AT-2's build writes") and the effects (all of `library/`) govern, so the eight
    are committed.
- **AGENTS.md wording.** Each full fact name needs its own line, so the bullet could only stay at four lines by
  shortening the old trigger list. "Changes to kit-file shapes, snapshotted catalogue entries or build messages" became
  "output changes", the slice's "a change to what the build writes". The widest line is 125 characters; the file
  already has 124 and 130.
- **Regenerating the AT-2 kit.**
  1. Run the new fact. It names `…\net10.0\KitAcceptanceTests\AT-2\absorbing-biaxial-noisy`.
  2. Copy that folder, `.oc/` excluded, over `data/absorbing-biaxial-noisy/`.
  3. Re-run the fact.

  Every change to what AT-2's build writes must do this in the same step.
- **Where the time goes.** The lazy `absorbingBuild` charges the whole AT-2 build and fit (about 14 min) to whichever
  AT-2 fact runs first. The comparison itself takes 0.1 s.

# Changelog

- 2026-11-16, step 0438:
  - committed `data/absorbing-biaxial-noisy/` from AT-2's build (34 files);
  - added `the built absorbing-biaxial-noisy kit equals the committed kit file by file`;
  - extracted `assertSameKitFiles` for AT-1's equality and Send facts;
  - AGENTS.md's reference-kit gotcha now names the AT-2 kit.

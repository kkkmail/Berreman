# Step 0438 — implementation log

## Rulings read

- R0001–R0005 (`.manual/030`, `031`, `033`, `035`, `036`, section "Binding ruling" of each). None of them conflicts
  with this step, and every one is preserved:
  - the series dispatch and the circular analyzer are unchanged;
  - `ElementIdProvider` injection stays: AT-2's reflected detector still takes `reflectedDetectorSlot`;
  - the single `rhoT` read, the server collector, the concurrent residual and the 1800 s fit bound are untouched.
- This step edits no production code.

## Progress

1. **`assertSameKitFiles`.** Step 428's inline comparison moved, unchanged in substance, into the private
   `assertSameKitFiles (built : KitFolder) (other : KitFolder) : unit` (one-line doc citing spec 0050 S12.2).
   - It takes both folders' relative paths, `.oc/` left out of both (`kitContentFiles`), and walks their union in
     sorted order.
   - It fails at the first difference. The message names the built folder's absolute path and the relative path, then
     says why: missing from `<other>` (extra in the built kit), extra in `<other>` (missing from the built kit), or
     different bytes (the `assertSameBytes` failure text).
2. **The two AT-1 facts.**
   - Step 428's fact `the built optics-mueller kit equals the committed kit file by file` is now
     `try assertSameKitFiles built.folder (committedFolder ()) finally close`. The run folder is still deleted only on
     success.
   - Step 430's Send fact compares the received folder through `assertSameKitFiles built.folder target`. Its `expect`
     on the path set and its per-file `assertSameBytes` loop were removed, because the helper asserts both.
   - The Send fact keeps `expectedFiles` for its send and extraction counts, and keeps every other assertion.
3. **The new fact** `the built absorbing-biaxial-noisy kit equals the committed kit file by file`.
   - It runs over `absorbingBuild`, after the recovery fact and before the explicit ensemble.
   - `CommittedKit.AbsorbingBiaxialNoisyKit.tryFindFolder` answers the committed folder. On `None` the fact fails with
     `Built kit: <built folder>` and "expected the committed kit; actual folder missing".
   - Otherwise it calls `assertSameKitFiles built.folder committed`. It writes nothing.
4. **`AGENTS.md`.** The reference-kit gotcha now names `data/absorbing-biaxial-noisy/` as AT-2's output, five
   observation files included, and names both comparison facts.
   - It is still four lines; the widest is 125 characters, and the file already has lines of 124 and 130.
   - Nothing else in the file changed.
   - To fit four lines, the old trigger list ("kit-file shapes, snapshotted catalogue entries or build messages") became
     "output changes", which is the slice's own "a change to what the build writes". See Gotchas.
5. **Build.** `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 in 88 s with 0 errors. The only warnings
   are the exempt NU1902 (12) and the vendored MathNet SYSLIB0051 (4), the same as before this step. Log:
   `.artifacts/0438-build.log`.
6. **The s42 invariants.** Each decoded `git grep` of the 21 found nothing.
7. **Red run.** The new fact ran alone, with the gate's command and
   `--filter-method "OpticalConstructor.Ui.Tests.KitAcceptanceTests.the built absorbing-biaxial-noisy kit equals the committed kit file by file"`.
   - It failed as designed in 841 s (13 m 58 s in the fact; total 1, failed 1).
   - Failure text: `Built kit: …\OpticalConstructor.Ui.Tests\bin\x64\Release\net10.0\KitAcceptanceTests\AT-2\absorbing-biaxial-noisy`,
     then `data/absorbing-biaxial-noisy: expected the committed kit; actual folder missing`.
   - Log: `.artifacts/0438-kit-acceptance-red.log`.
8. **The committed kit.** Every file of that built folder except `.oc/` was copied into `data/absorbing-biaxial-noisy/`
   with its relative path, and `cmp` confirmed each copy byte for byte. There are 34 files:
   - the kit file `.kit-json`;
   - the eight `<label>.item-json`: Laser, Polarimeter T, Polarimeter R, OpticAxisCut, OtherOpticAxisCut, ZCut, XCut,
     YCut;
   - the five `<plate>.setup-json` and the five `<plate>.experiment-json`;
   - `AbsorbingTriclinic.study-json` and `AbsorbingTriclinic@20260601T120100Z.fitrun-json`;
   - the five `data/noisy-observations-seed-<n>.csv`;
   - eight `library/<id>@v1.<kind>-json` snapshots:
     - the material `81e1d115-…`;
     - the sample `af711361-…`;
     - the heNe633 source preset `a77786fe-…`;
     - the intensity detector preset `ffa07a4d-…`;
     - three item models (`239b0cfb-…`, `4d13ecb1-…`, `7e74324f-…`);
     - the crystal category `5d7e9f1a-…`.

   Checks before handover:
   - no file holds a CR, a BOM or a NUL, and every file ends in LF;
   - no file holds an absolute or machine path; the only instants are the injected clock's;
   - `git check-ignore` matches none of the 34;
   - `git hash-object --path` equals `--no-filters` for every file, so the `* text=auto eol=lf` normalization leaves
     every blob byte-identical;
   - `/data/*/.oc/` still ignores the kit's `.oc/lock` (`.gitignore:334`).
9. **Other gate selections.**
   - ui-tests over `StudyViewTests` and `DataStepTests`: `Passed!`, 18 of 18, in 30 s.
   - ui-smoke, all discovered: `Passed!`, 264 of 264, in 222 s.
   - Elevation, all 13 projects, ELEV004 excluded: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, in 260 s.
   - Logs: `.artifacts/0438-ui-tests.log`, `0438-ui-smoke.log`, `0438-elevation.log`.
10. **Green run.** The kit-acceptance gate's command ran over its selection, `KitAcceptanceTests`. This was the
    second, independent AT-2 build.
    - Result: `Passed!`, total 13, succeeded 12, skipped 1, failed 0, in 894 s against 3600 s.
    - The skipped fact is the explicit five-seed ensemble ("Not run (due to explicit test filtering)", TRX
      `NotExecuted`), as at 0437.
    - The new fact passed in 0.125 s over the shared build.
    - AT-1's equality and Send facts passed through `assertSameKitFiles`, in 10.9 s and 12.1 s.
    - The second build's folder is byte-identical to the committed kit (`diff -r -x .oc`).
    - That build's AT-2 fit took about 816 s, from the study's last save at 13:36:43 to the fit run's write at
      13:50:19. That is within R0005's 1800 s bound.
    - A sha256 manifest of every file and directory in `data/absorbing-biaxial-noisy/` and `data/optics-mueller/` was
      identical before and after the run: the tests write nothing into `data/`
      (`.artifacts/0438-data-before.txt`, `0438-data-after.txt`).
    - Log: `.artifacts/0438-kit-acceptance.log`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`
- `AGENTS.md`
- `data/absorbing-biaxial-noisy/**`: 34 new files, copied from the build, never hand-edited

## Testing state

These are development runs, evidence only. The supervisor's gates decide.

| Gate | Outcome | Count | Time / deadline |
|---|---|---|---|
| build | exit 0, 0 errors | — | 88 s / 600 s |
| elevation | `ELEV-GATE: OK`, 0 new | 13 projects | 260 s / 600 s |
| ui-smoke | `Passed!` | 264 | 222 s / 600 s |
| ui-tests (StudyViewTests, DataStepTests) | `Passed!` | 18 | 30 s / 360 s |
| kit-acceptance (KitAcceptanceTests) | `Passed!` | 12 (+1), 1 explicit skip | 894 s / 3600 s |
| 21 s42 invariants | nothing found | — | instant |

- No run exceeded its effective deadline.
- The red run, the new fact alone before the kit existed, failed as designed in 841 s. It is not a gate run.
- Nothing is blocked.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0438-build.log`
- `0438-elevation.log`
- `0438-kit-acceptance-red.log`
- `0438-kit-acceptance.log`
- `0438-ui-tests.log`
- `0438-ui-smoke.log`
- `0438-data-before.txt`, `0438-data-after.txt`

TRX files are under `OpticalConstructor.Ui.Tests/TestResults/` (`kit-acceptance-0438-red.trx`, `kit-acceptance.trx`,
`ui-tests.trx`, `ui-smoke.trx`).

## Gotchas

- **Eight `library/` snapshots, not four.** The slice's file list names four: the material, the sample, heNe633 and
  the Mueller polarimeter model. AT-2's build writes eight, as step 0433 already asserts in `assertAbsorbingLibrary`.
  The other four are:
  - the intensity detector preset `ffa07a4d-…`, which the reflected detector's `AddElement Detector` seed snapshots;
  - the source-preset item model `239b0cfb-…`;
  - the stack-sample item model `4d13ecb1-…`;
  - the crystal category `5d7e9f1a-…`.

  The acceptance asks for "exactly the kit files, library snapshots and five observation files AT-2's build writes",
  and the effects authorize all of `data/absorbing-biaxial-noisy/library`, so the build's eight are committed. Nothing
  was hand-edited or left out.
- **AGENTS.md wording.** Adding AT-2 inside the four-line limit was impossible with the old trigger list intact:
  each full fact name is 70–77 characters and needs its own line.
  - So "changes to kit-file shapes, snapshotted catalogue entries or build messages" became "output changes", the
    slice's "a change to what the build writes".
  - The bullet still names both folders, the five observation files, `.oc/`, both facts and the same-step re-commit.
- **Send fact.** Its own path-set `expect` and per-file byte loop were replaced by `assertSameKitFiles`, which asserts
  the same two things, so nothing is compared twice. `expectedFiles` stays for the send and extraction counts.
- **Regenerating the AT-2 kit.**
  1. Run `the built absorbing-biaxial-noisy kit equals the committed kit file by file`. It names
     `…\bin\x64\Release\net10.0\KitAcceptanceTests\AT-2\absorbing-biaxial-noisy`.
  2. Copy that folder, `.oc/` excluded, over `data/absorbing-biaxial-noisy/`.
  3. Re-run the fact.

  The build folder's name is fixed (`AT-2`), and the next run deletes it first.
- **Where the build's time lands.** The lazy `absorbingBuild` charges the whole AT-2 build and fit (about 14 min) to
  whichever AT-2 fact xUnit runs first. This time it was the grid-generator fact (13 m 45 s). The comparison fact
  itself takes 0.1 s.

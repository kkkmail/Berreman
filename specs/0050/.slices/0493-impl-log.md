# Step 0493 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 (each preserves an
  earlier repair; none authorizes or asks anything of this step beyond "preserve") and the slice.
- `TestAppScope.fs`: added the opens the moved code needs (`System` first, so
  `OpticalConstructor.Ui.AppContext` still shadows `System.AppContext`; `Xunit`;
  `BerremanTestSupport` for `CommittedKit`; `Domain.KitArchive`, `Domain.KitHistory`,
  `Storage.KitArchiveStore`, `Ui.KitArchiveJob`, `Ui.KitStartView`), then, after
  `inverseModelOver`, the moved declarations (below).
- `KitAcceptanceTests.fs`: deleted the six moved blocks (22 + 2 + 16 + 38 + 13 + 80 lines, each
  checked against its expected span) and qualified every remaining reference through
  `TestAppScope.` with a scripted, word-bounded rewrite. The script first confirmed that no match
  lies in a string, a comment or a backticked name. Counts: `expect` 292, `readKitResult` 36,
  `relativeFiles` 7, `committedFolder` 5, `assertCommittedUnchanged` 5, `assertEmptyLibrary` 4,
  `CommittedKitSession` 3, `absorbingKitId` / `absorbingKitAuthor` 3 each, `testClockAt`,
  `kitStartOver`, `refitTimeOf`, `kitId`, `kitAuthor` 2 each, and the two
  `TestAppScope.openCommittedReadOnly (FolderName "KitAcceptanceTests")` calls. `assertKitFile`'s
  own `kitId` parameter is left as it is.
- First build green; elevation green; ui-smoke 271/271.
- `TestAppScope.fs`: the four moved values, now public, carry explicit types (`kitId : KitId`,
  `kitAuthor : KitAuthor` and the absorbing pair), per AGENTS.md's explicit public surface. Rebuilt,
  then ran ui-tests (selected), kit-acceptance (selected) and ui-smoke again over the final build.
- Verified the move: a word-level diff of `KitAcceptanceTests.fs` adds only `TestAppScope.`-qualified
  tokens. A line comparison of the moved declarations against HEAD differs only in
  `openCommittedReadOnly`'s `owner` parameter and its `emptyLibraryAppContext owner`, the dropped
  `private` (and `TestAppScope.` self-qualifiers), `relativeDirectories` staying private, the four
  value annotations, and the one-line docs.

### What moved into `TestAppScope` (after `inverseModelOver`)

| Declaration | Visibility | Why it moved |
|---|---|---|
| `CommittedKitRecording` | public | the recording's type (slice) |
| `CommittedKitSession` | public | `openCommittedReadOnly`'s result; `KitAcceptanceTests` compiles later |
| `kitId`, `kitAuthor`, `absorbingKitId`, `absorbingKitAuthor` | public | read by the open's body and by the builds and `assertKitFile` facts |
| `expect`, `readKitResult` | public | called by the open's body and by both builds |
| `committedFolder` | public | called by the two and by AT-1's build |
| `testClockAt`, `kitStartOver` | public | called by the open and both builds: the start screen is now composed in one place |
| `relativeFiles` | public | called by the two and by the kit comparisons |
| `relativeDirectories` | private | called by the two alone |
| `refitTimeOf`, `assertEmptyLibrary` | public | called by the open's body and by the read-only facts |
| `assertCommittedUnchanged` | public | slice |
| `openCommittedReadOnly (owner : FolderName) (kit : CommittedKit)` | public | slice; composes over `emptyLibraryAppContext owner` |

Each moved declaration's doc is one line citing spec 0050 Close. The field docs of
`CommittedKitSession` are kept as they were.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/TestAppScope.fs` (+190)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs` (+356 / −527:
  171 lines deleted net, the rest qualified in place)

No product file, project file or other test class changed. No fact, retry or cache was added.

## Testing state

Local runs, each with the gate's own command, over the final tree unless noted:

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; final build's 6 warnings are all the exempt `NU1902` | 18 s (incremental; 90 s first) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 232 s | 600 s |
| ui-smoke (whole) | `Passed!`, 271 of 271 (unchanged) | 239 s (245 s on the first build) | 600 s |
| ui-tests (selected `KitStartWindowTests`, `KitWindowTests`, `StudyViewTests`) | `Passed!`, 32 of 32 | 45 s | 360 s |
| kit-acceptance (selected `KitAcceptanceTests`) | `Passed!`, 14 succeeded, 1 skipped (the Explicit five-seed ensemble), 15 total, count unchanged | 1628 s | 3600 s |
| 21 × `invariant-s47-*` | each decoded `git grep` finds nothing, also with `--untracked` | — | 300 s |

- Both named facts passed. The optics-mueller read-only open took 2.7 s. The absorbing-biaxial-noisy
  one took 597 s, under the 1800 s bound of R0005.
- Elevation ran before the value annotations. The ELEV gate covers the 13 product projects only,
  and the test project is outside its scope, so the edit cannot change its result.
- The first full rebuild also printed the vendored `MathNetNumerics` `SYSLIB0051` warnings. They
  were already in earlier steps' captured builds (e.g. `0427-attempt-02-task.txt`), and this step
  touches nothing there.
- `git status` shows no change under `data/`. No run exceeded its effective deadline.
- The two edited files have no CR byte, and `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0493-build.log`, `0493-build-final.log`
- `specs/0050/.artifacts/0493-elevation.log`
- `specs/0050/.artifacts/0493-ui-smoke.log`, `0493-ui-smoke-final.log`
- `specs/0050/.artifacts/0493-ui-tests.log`
- `specs/0050/.artifacts/0493-kit-acceptance.log`, `0493-kit-acceptance-duration.txt`

## Gotchas

- **The slice moves more than its two functions.** The two functions' bodies must be unchanged,
  `KitAcceptanceTests` compiles after `TestAppScope`, and a copy is forbidden. So everything those
  bodies call moved too, including helpers the facts (not only the builds) use: `refitTimeOf`,
  `assertEmptyLibrary`, `CommittedKitSession` and the four kit id / author values. This is the
  reading most consistent with "bodies unchanged" and "Reuse, never copy".
- `kitId` / `kitAuthor` keep their names, though as public `TestAppScope` names they are generic.
  Renaming them would change the open's body, which the slice forbids. Their docs say which kit
  they belong to.
- `open System` must stay the first open in `TestAppScope`. Opened later, `System.AppContext`
  would shadow `OpticalConstructor.Ui.AppContext` in `AppContext.create` and every `AppContext`
  annotation.
- Inside `TestAppScope`, the moved bodies call `inverseModelOver`, `emptyLibraryAppContext` and
  `kitPickerOf` unqualified, because a module does not name itself.

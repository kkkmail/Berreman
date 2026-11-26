# Step 0362 — impl log

## Progress

- [x] Domain: `UnknownOwner`, `StudyUnknown`, `Study.scopeOf` / `unknownsWith`, `StudyUnknown.owners`.
- [x] Tests: `StudyTests` (+2).
- [x] Development runs: build, constructor-unit-tests, invariants, elevation.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs` — appended after step 361's
  `Sample.openedMaterialsIn`:
  - `UnknownOwner` (`ItemOwner` / `MaterialOwner` / `SessionOwner` / `ExperimentOwner`), with no tag because it is
    derived and never stored;
  - the record `StudyUnknown`: `parameter`, `scope`, `unknown`;
  - `type Study with`:
    - `scopeOf`: the study's `ScopedParameter`, else a default by the reference's case and the parameter's name;
    - `unknownsWith`: the item parameters, then the material components, whose state is `UnknownParameter`, a link's
      follower left out;
  - `type StudyUnknown with`: `owners`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`:
  - opens `System`, `CrystalPhysics`, `MaterialLibrary`, `ItemModels` and `CrystalMaterialFixtures`;
  - test-local CPL1, CPL2 and Phone, made by `ItemDraft.toItem` and freed through `Item.withParameterState`;
  - the absorbing crystal opened through `Study.openMaterial` and freed through `withMaterialParameterState`;
  - two facts.

## Decisions

- **`owners` under `ItemScope` / `MaterialScope` reads the reference's case.** An `ItemParameterRef` gives
  `ItemOwner`, a `MaterialParameterRef` gives `MaterialOwner`. The match is total, so an item parameter scoped
  `MaterialScope`, or the reverse, still has its own one owner. This is the slice's "its one owner, the `ItemOwner`
  of its item or the `MaterialOwner` of its material".
- **`ExperimentScope` owners are `List.distinct` of `users`, in order.** That is "one per user": the fitter holds one
  value per owner, so a repeated user must not open a second value.
- **The follower test compares the whole `StudyParameterRef`**: item or material, id and name. A follower that names
  another item's parameter of the same name stays in.

## Testing state

These are development runs of each gate's own command from `0362.gates`, over the final tree.

- **build:** exit 0, `0 Error(s)`, no lowercase `error`.
  - The final incremental build took 16 s and showed only NU1902 warnings.
  - The first full rebuild took 76 s. It also showed the vendored MathNet SYSLIB0051 pair, which has been there since
    step 0001.
  - I checked that the Domain dll is newer than the last `Studies.fs` edit.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 220s`, with 1518 ELEV004 suppressed, as at step
  0361.
  - I stopped the first run early, made a comment-only edit, rebuilt, and ran elevation once over the final tree.
- **constructor-unit-tests:** `Passed!`, 1597 of 1597 (1595 + 2), in 69 s.
  - Both new `StudyTests` ids are `Passed` in the TRX.
- **invariant-s36-\* (20):** all report `INVARIANT HOLDS`.

The slice requires all of it, and all of it landed. No run went past its deadline.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0362-build-dev.log`
- `0362-elevation-dev.log`
- `0362-constructor-unit-tests-dev.log`
- `0362-invariant-commands.txt`, the twenty decoded invariant commands

## Gotchas

- **A comment-only fix after the first review.** The draft `owners` doc ran to four lines, and the slice caps each doc
  at three. It is now three.
- **Bash heredocs with apostrophes failed in this shell.** The test names hold `'`. I used the Edit tool instead.
- **`{ parameter; scope }` literals are annotated `ScopedParameter` in the tests**, because `StudyUnknown`, declared
  later, shares both field names.

# Step 0493 — state of the world

## Where we are

Spec 0050's Close section, step 493 of 494. The committed kits' read-only open now lives in
`TestAppScope`, which compiles before `WireUiFinalCompositionTests.fs`. Step 494's closing
composition test can therefore open the optics-mueller and absorbing-biaxial-noisy kits through
the same code AT-1 and AT-2 use. Step 494 calls it as
`TestAppScope.openCommittedReadOnly (FolderName "WireUiFinalCompositionTests") kit` and closes with
`TestAppScope.assertCommittedUnchanged`.

## What's working

- Moved `openCommittedReadOnly` and `assertCommittedUnchanged` from `KitAcceptanceTests` into
  `TestAppScope`, public, with the recording and session types; the open now takes the calling
  class's folder (`emptyLibraryAppContext owner`)
- Moved the helpers their bodies call into `TestAppScope` as well (`expect`, `readKitResult`,
  `committedFolder`, `testClockAt`, `kitStartOver`, `relativeFiles`, `refitTimeOf`,
  `assertEmptyLibrary`, the committed kits' ids and authors), so the start screen is composed in
  one place for the read-only open and both builds
- `KitAcceptanceTests` declares neither function any more and calls everything moved through
  `TestAppScope.`, every assertion kept
- kit-acceptance, ui-smoke, the selected ui-tests, build, elevation and the 21 s47 invariants pass
  locally, with counts unchanged

## Tests

Local runs with each gate's own command over the final tree. The supervisor's gate engine is the
authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902` | 18 s (incremental; 90 s first) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 232 s | 600 s |
| ui-smoke (whole) | `Passed!`, 271 of 271, unchanged | 239 s | 600 s |
| ui-tests (selected `KitStartWindowTests`, `KitWindowTests`, `StudyViewTests`) | `Passed!`, 32 of 32 | 45 s | 360 s |
| kit-acceptance (selected `KitAcceptanceTests`) | `Passed!`, 14 succeeded + 1 skipped (Explicit ensemble) of 15, unchanged | 1628 s | 3600 s |
| 21 × `invariant-s47-*` | each decoded grep finds nothing, also with `--untracked` | — | 300 s |

- Both named facts pass: `the committed optics-mueller kit opens read-only against an empty library,
  writes nothing, and its study reproduces the reference reconstruction` (2.7 s), and `the committed
  absorbing-biaxial-noisy kit opens read-only against an empty library, writes nothing, and
  reproduces AT-2's data, model and fit` (597 s, within R0005's 1800 s bound).
- No test was added, renamed or removed, and no run exceeded its effective deadline.
- `data/` is unchanged (`git status`). The edited files are LF only.

```yaml
gates:
  ui_smoke_tests: 271
  ui_tests: 32
  kit_acceptance_tests: 14
```

## Architecture

- `TestAppScope` is the one place a test composes the committed kits' read-only session: the empty
  library context, the start screen (`kitStartOver`) sending `OpenKitReadOnly`, and the Inverse
  window over the opened kit. It also holds the before/after folder recording that proves nothing
  was written.
- Like every other `TestAppScope` helper (step 83), `openCommittedReadOnly` takes the calling
  class's folder.
- `KitAcceptanceTests` keeps only what is AT-1's or AT-2's own: the builds, the expected data, and
  the facts' assertions.

## Deferred

Nothing. Every requirement of the slice is in this round.

## Gotchas

- `open System` must stay the first open in `TestAppScope`. Opened later, `System.AppContext`
  would shadow `OpticalConstructor.Ui.AppContext`.
- `TestAppScope.kitId` / `kitAuthor` are AT-1's optics-mueller kit's. They kept their names
  because the moved body names them.
- The vendored `MathNetNumerics` prints `SYSLIB0051` on a full rebuild. It predates this step, and
  this step does not touch it.

## Changelog

- 2026-11-18 — Step 0493: moved the committed kits' read-only open (`openCommittedReadOnly`,
  `assertCommittedUnchanged`, their types and helpers) from `KitAcceptanceTests` into
  `TestAppScope`. The open now takes the caller's folder, and `KitAcceptanceTests` calls it through
  `TestAppScope`.

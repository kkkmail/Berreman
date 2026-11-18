# Step 0493 — impl plan

## Approach

Move the committed kits' read-only open out of `KitAcceptanceTests.fs` into
`TestAppScope.fs` (compiled before `WireUiFinalCompositionTests.fs`), so step 494's
composition test can open the kits through the same code AT-1 and AT-2 use.

`TestAppScope` gains, after `inverseModelOver`, public:

- `CommittedKitRecording` (the recording's type) and `CommittedKitSession` (the type
  `openCommittedReadOnly` answers; `KitAcceptanceTests` compiles later, so the type must move);
- the helpers the two functions call, which cannot be copied (AGENTS.md "Reuse, never copy"):
  `expect`, `readKitResult`, `committedFolder`, `testClockAt`, `kitStartOver` (the start screen,
  now composed in one place for the open and both builds), `relativeFiles`, the committed kits'
  ids and authors (`kitId`, `kitAuthor`, `absorbingKitId`, `absorbingKitAuthor`), `refitTimeOf`
  and `assertEmptyLibrary`; `relativeDirectories`, called by the two alone, moves private;
- `assertCommittedUnchanged` and `openCommittedReadOnly (owner : FolderName) (kit : CommittedKit)`,
  bodies unchanged but for `emptyLibraryAppContext owner` in place of the literal folder and the
  `TestAppScope.` qualifier dropped inside its own module.

Each moved declaration's doc becomes one line citing spec 0050 Close. `TestAppScope` opens
`System` (first, so `OpticalConstructor.Ui.AppContext` still shadows `System.AppContext`),
`Xunit`, `BerremanTestSupport`, `KitArchive`, `KitHistory`, `KitArchiveStore`, `KitArchiveJob`
and `KitStartView`.

`KitAcceptanceTests` deletes every moved declaration and calls each through `TestAppScope.`
(the facts call `TestAppScope.openCommittedReadOnly (FolderName "KitAcceptanceTests")` and
`TestAppScope.assertCommittedUnchanged`); every assertion is kept.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/TestAppScope.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`

## Risks

- `open System` in `TestAppScope` could shadow `AppContext`; mitigated by opening it first.
- Parameters named `kitId` / `kitAuthor` shadow the module values; only references to the
  module-level values are qualified.
- The qualification of ~350 call sites is mechanical; done by a scripted, string-aware rewrite
  and checked by the build.

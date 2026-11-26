# Step 0201 — impl-log (IMPLEMENT: Send kit… as semantic messages of the kit window)

## Progress

- [x] Shell fields and `inert`.
  - `KitWindowShell` gains `zipPicker : KitZipPicker`, `archive : KitArchiveProxy` and
    `archiveJobs : KitArchiveJob.KitArchiveJobHost`, each with a one-line doc citing S6.
  - `inert` holds `KitZipPicker.none`, `KitArchiveProxy.createInMemory (InMemoryArchiveDisk.create ())` and
    `KitArchiveJobHost.inert`.
- [x] Model field.
  - `kitSend : KitArchiveJob.KitArchiveJobState` comes after `kitChanges`, documented in one line.
  - `initWith` sets it to `KitArchiveJobState.idle`.
- [x] Messages.
  - `SendKit of FileName`, `SendKitProgressed of JobRunner.JobProgress`,
    `SendKitEnded of Result<KitArchiveOutcome, KitArchiveError>` and `CancelSendKit` come after `KeepMyKitChanges`.
  - Each has a one-line doc citing S6.
- [x] Update arms, as the slice words them:
  - `SendKit`: a scratch kit is refused with `A kit with no folder cannot be sent.`; a running send answers the model
    unchanged; otherwise `startArchiveJob (fun observer -> archive.sendKit kit.kitFile folder zip observer)`, then
    `kitSend.started handle` and `kitStatus = None`.
  - `SendKitProgressed`: `kitSend.progressed`.
  - `SendKitEnded`: sets `idle` and one of the three statuses.
  - `CancelSendKit`: `kitSend.cancel ()`, answering the model unchanged.
  - Not behind `guardKitWrite`; the arm's comment cites 001 §E 'Review round-trip'.
- [x] The full-shell builders became record updates of `KitWindowShell.inert`:
  - `ConstructorScene.mount` in `Program.fs`;
  - `recordingShell` and `refusing` in `KitWindowModelTests.fs`;
  - `recordingShell` in `KitWindowTests.fs`.
- [x] Six new facts in `KitWindowModelTests`, plus four helpers:
  - `withFactFolder`, which `withTitledFolder` now calls, so the folder setup has one body;
  - `archiveValue`;
  - `sendingShell` (the real archive and the `immediate` host over one message recorder);
  - `passedThrough`;
  - `exportsZipUnder`.
- [x] Development runs: build, constructor-unit-tests, ui-smoke, ui-tests (`KitWindowTests`), elevation, and the 14
  invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`: the shell fields and `inert`,
  `kitSend`, the four messages and their arms, and `open OpticalConstructor.Domain.KitArchive`.
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`: the mounted shell is
  `{ KitWindowShell.inert with explorer = ExplorerProxy.system; requestClose = window.Close }`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`: the two shells become updates of
  `inert`, plus the helpers and the six facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`: `recordingShell` becomes an update of
  `inert`.

## Decisions

- **`JobRunner` and `KitArchiveJob` are qualified in the view file, not opened.**
  - `JobRunner` was never opened there. Its single-word `JobStatus` / `JobMsg` cases (`Idle`, `Running`, `Completed`,
    …) could shadow names in a 4,300-line file.
  - `KitArchiveJob` brings `KitArchiveProgressIds { bar; text; cancel }`, the label capture step 200's gotcha warns
    about.
  - Only `OpticalConstructor.Domain.KitArchive` is opened. Its record labels resolve by the intersection of all
    fields, and the build confirms it.
- **The cancel fact keeps the job in a test-local deferred host.**
  - The host keeps the job and answers a handle that cancels the fact's own `CancellationTokenSource`.
  - The fact then runs the kept job over an observer of that source's token. The real `KitArchiveProxy.create ()`
    answers `Ok KitArchiveCancelled` before its first file and deletes its `<zip>.tmp`.
  - The fact asserts that neither the zip nor `<zip>.tmp` exists.
- **The zip's folder exists before a send, because the facts create it.** `exportsZipUnder` creates
  `<fact folder>/exports`, as the folder a save dialog answers always exists. The real `writeKitZip` opens
  `<zip>.tmp` with `FileMode.Create` and does not create folders.
- **The progress fact checks every file, not just the first.**
  - After `SendKit` and the first recorded progress only, the send is running and its text starts `1 / `.
  - The fact also checks that the k-th recorded progress reads `k / <total>`.
  - Nothing the slice asks for was dropped.

## Testing state

These are development runs only. The gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 errors. The only warnings are the
  vendored MathNet `SYSLIB0051` ones already on the start tree (step 200's SoW records them); none come from our code.
- **constructor-unit-tests:** 1259 passed, 0 failed (1253 → 1259, +6).
  - In the TRX, all six new identities pass:
    - `Send kit zips the kit's folder and tells how many files it sent`;
    - `a running send shows each zipped file as progress`;
    - `a cancelled send writes no zip and says so`;
    - `a scratch kit is not sent and no job starts`;
    - `a send that fails tells why and leaves the window idle`;
    - `a kit opened read-only is sent, and its folder is unchanged`.
- **ui-smoke:** 236 passed.
- **ui-tests** (selected class `OpticalConstructor.Ui.Tests.KitWindowTests`): 6 passed.
- **elevation** (`ASG_ARC_BASE_COMMIT` = the `.arc-base-json` origin `f395597c…`):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`;
  - known findings 0.
- **The 14 `invariant-s22-*` greps:** each finds nothing.
- **Line endings:** LF. `git diff --numstat` matches `--ignore-cr-at-eol` exactly.

## Artifacts

- `specs/0050/.artifacts/0201-build-01.log`
- `specs/0050/.artifacts/0201-constructor-unit-tests-01.log`
- `specs/0050/.artifacts/0201-ui-smoke-01.log`
- `specs/0050/.artifacts/0201-ui-tests-01.log`
- `specs/0050/.artifacts/0201-elevation-01.log`

## Gotchas

- **With the `immediate` host, a send's progress and end are already recorded when `SendKit` returns.**
  - The arm sets `started` after the host answers, so the model is Running until the recorded messages pass through
    the update.
  - The facts pass them through in order (`passedThrough`).
- **`KitWindowShell.inert` is a static property, and every access builds a new in-memory archive disk.**
  - Every access also builds a new explorer, as before this step.
  - Two `inert` shells never compare equal (`[<ReferenceEquality>]`). That is unchanged.
- **Step 202 owns the visible parts.** This step adds no button or progress row. The `Send kit…` verb, its save dialog
  and the progress row in the kit bar belong to step 202, which reads `kitShell.zipPicker` and `kitSend`.

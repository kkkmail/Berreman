# Implementation plan — 0112

Wire `LauncherWindow` to the existing pending-catalogue import dialog. Keep the
waiting imports at the window IO boundary, show the complete decision count,
open the first pending request on startup or Review imports, and remove only
the applied catalogue when the dialog reports success. Cancel preserves the
pending state. Add the two centralized launcher automation IDs.

Extend `LauncherTests` with the two specified facts. Exercise the real launcher
and import controls for both a fresh, nonconflicting catalogue and the mixed
`Lab glass` conflict. Observe dialog windows semantically and compare library
entries and import logs before and after Cancel. Existing populated fixtures
must explicitly settle imports before launching. Preserve the no-pending final
composition fixture.

Expected source changes: `OpticalConstructor.App/Program.fs`,
`OpticalConstructor.Controls/UiIds.fs`, and
`OpticalConstructor.Ui.Tests/LauncherTests.fs`; change
`WireUiFinalCompositionTests.fs` only if fixture settlement requires it.
Risks are asynchronous modal-window timing, complete Add/Reuse/conflict row
coverage, and accidental duplicate startup dialogs.

The session's higher-priority verification instructions require local tests and
required checks despite the WIRE_UI task-file act-only wording. Run the resolved
gate commands and record actual outcomes. Formal acceptance and checkpoint
baselines remain supervisor-owned.

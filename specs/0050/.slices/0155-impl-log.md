# Implementation log — 0155

## Progress

- Read AGENTS.md, the task, shared and WIRE_UI worker prompts, project prompt, slice contract, naming conventions and manual/010. Inspected the launcher, test helpers, prior slice handoff and compiled gate roster.
- Before implementation: add exactly the two authorized smoke identities, covering the real locked-kit offer and cancellation; then wire the existing folder/mode/access opening function and centralized IDs. Preserve the existing composition fixture.
- After test edits: authored both exact smoke identities. Assertions cover holder/process diagnostics, accessible labels and IDs, shared horizontal row, 12 px gap, public action colours, Main and Inverse read-only retries without repicking, a refused scene save, unchanged recursive file bytes, retained lock after close, and cancellation in both modes. Extended the existing non-lock failure fact to require no retry buttons. No tests were executed.
- Before product edits: add the two documented IDs and track the optional offer row alongside the existing diagnostic. Clear both together, use the existing shared opening function recursively for ReadOnlyKitAccess, and keep all non-lock errors on their existing diagnostic path.
- After product edits: the locked-kit diagnostic names holder.describe and its trailing period; the two accessible buttons reuse surfaceButton and MaterialsWindowView colours in a horizontal StackPanel with Spacing 12. Success and Cancel remove both controls; replacement errors clear the previous row before rendering their own diagnostic. The retry captures the same context, folder and mode without picking again.
- Source review: checked the S43-READ-ONLY-OFFER immutable oracle against the authored assertions, reviewed the existing read-only proxy and scene-save path, and confirmed WireUiFinalCompositionTests remains unchanged. Added an assertion that the row is beneath the diagnostic in the same panel. Source diff counts match with and without --ignore-cr-at-eol; no source CRLF churn was introduced. This was source inspection, not gate execution.
- Before handoff: record the inherited checkpoint counts and completed implementation in the required state-of-the-world path. No implementation requirement is deferred; runtime acceptance awaits the supervisor.

## Files modified

- Created the implementation plan and this log before source edits.
- OpticalConstructor.Ui.Tests/LauncherTests.fs
- OpticalConstructor.App/Program.fs
- OpticalConstructor.Controls/UiIds.fs
- specs/0050/.slices/0155-state-of-the-world.md

## Testing state

No build, tests, analyzers or invariants executed. WIRE_UI Invariant 6 assigns gate execution to the supervisor after worker exit. Exactly two new smoke identities are authorized; existing identities and assertions will be preserved.

Both declared smoke identities are now authored and every existing assertion is retained. The supervisor owns build, elevation, all constructor-unit-tests, all ui-smoke, selected LauncherTests ui-tests, and the eleven s17 invariant gates. Checkpoint 153 records constructor_unit_tests = 1149 and ui_smoke_tests = 219. Those are inherited counts, not measurements from this round; the two new facts have no claimed pass result. No whole-suite ui_tests baseline is invented. commit_ready is false pending supervisor verification, with no implementation blocker.

## Artifacts

No runtime artifacts generated. Persistent run artifacts belong only under specs/0050/.artifacts.

## Gotchas

- The pre-existing specs/0050/.manifest.state.json change belongs to the supervisor and is left untouched.
- A read-only retry must reuse the picked folder and mode without consulting the picker again.
- WireUiFinalCompositionTests is explicitly unchanged by this slice.
- The read-only scene-save assertion uses the existing real control and kitStatus projection; storage access enforcement and constructor ownership remain their existing implementations.
- The read-only holder is a separate KitProxy in the same process. The storage lock deliberately distinguishes proxies; its existing idempotent acquireLock returns that holder for the expected diagnostic.
- No durable shape, migration, parser, serialization, version, gate/census/oracle/checkpoint, analyzer configuration, external repository or notebook changes were made. No Git mutation was performed.

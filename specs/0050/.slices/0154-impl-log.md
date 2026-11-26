# Implementation log — 0154

## Progress

- Read the task, AGENTS.md, worker and project prompts, slice contract, naming conventions and manual/010. Inspected the gate roster and existing kit-opening/window composition.
- Before implementation: replace scratch-window factories with supplied OpenedKit constructors, preserving the existing seed behavior through supplied-kit initializers. Share launcher opening logic and retain Materials/Library behavior.
- Construction decision: initMainWithKit already exists, but the inverse seed is still inside initInverse's scratch factory. Manual/010 explicitly authorizes necessary initializer changes; extract that existing body into initInverseWithKit in TableAndElementRotationView.fs and make the scratch entry point delegate. This additional Ui edit prevents both a discarded scratch kit and a duplicate inverse seed.
- Before test edits: add the immediate picker fixture and the three declared LauncherTests facts; extend both existing constructor launch facts with metadata/lock assertions. Preserve all existing assertions while adapting constructor and launcher arguments.
- After test edits: authored the three exact smoke identities, including real Experiments-bay row projection, cancellation in both modes, malformed-file diagnostics in both modes and error removal on successful retry. The existing Main/Inverse facts also assert .kit-json and lock lifetime.
- After product wiring: constructors now accept OpenedKit, overlay all four stores, initialize from the supplied KitContext and load recorded content. Shared mounting releases the kit on Closed and logs typed failures. Launcher Main/Inverse use Async.StartImmediate with the injected picker and one folder/mode/access open function; the new error TextBlock has its stable name, automation ID and diagnostic accessible name.
- After fixture adaptation: updated all product constructor/launcher call sites in the authorized test files. MainWorkbenchTests now loads an opened kit; mounted hosts close it on Closed and pure-model facts close in finally. Extracted the inverse seed into initInverseWithKit without changing its content; initInverse delegates after successful scratch construction.
- Before source review: inspect argument shapes, test cleanup, semantic IDs and line endings without executing build, test, analyzer or invariant gates.
- After source review: all product constructor calls now supply OpenedKit and all LauncherWindow calls supply a picker factory. Cancellation compares the actual configured kit-folder inventory before and after both clicks. Error assertions identify the malformed experiment, confirm the wrapping/accessible diagnostic and no retained lock, then confirm removal after retry succeeds. MainWorkbenchTests releases its kit even when assertions fail. Code diff line counts match with and without --ignore-cr-at-eol; no CRLF churn was introduced.
- Completed the state-of-the-world and implementation handoff. No implementation requirement was deferred; execution and measured acceptance remain with the supervisor.

## Attempt 02 progress

- Read the retry task and supervisor build diagnostic, worker/project instructions, slice contract and manual/010. Preserve the existing 0154 implementation and supervisor-owned changes.
- Before repair: the reported FS0039 comes from a missing extension-module import in MainWorkbenchTests.fs. CatalogueImportContext.create is declared in Storage/CatalogueImportStore.fs; existing consumers such as AppContext.fs and BuiltinCatalogue.fs open that module. Add the same import to the authorized fixture file without changing its behavior, assertions or test identities.
- After repair: added open OpticalConstructor.Storage.CatalogueImportStore to MainWorkbenchTests.fs, making its existing CatalogueImportContext.create call resolve through the canonical Storage extension. This attempt changes one source line; the earlier wiring, supplied-kit ownership and acceptance assertions remain intact.

## Files modified

- Created the implementation plan and this log before source edits.
- OpticalConstructor.App/Program.fs
- OpticalConstructor.Controls/UiIds.fs
- OpticalConstructor.Ui/TableAndElementRotationView.fs (initializer extraction authorized by manual/010)
- OpticalConstructor.Ui.Tests/TestAppScope.fs
- OpticalConstructor.Ui.Tests/LauncherTests.fs
- OpticalConstructor.Ui.Tests/WireUiCompositionTests.fs
- OpticalConstructor.Ui.Tests/WireUi0040CompositionTests.fs
- OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs
- OpticalConstructor.Ui.Tests/AppContextTests.fs
- OpticalConstructor.Ui.Tests/TableAndElementRotationTests.fs
- OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs
- specs/0050/.slices/0154-state-of-the-world.md

## Testing state

No gates or checks executed. WIRE_UI Invariant 6 assigns build, elevation, constructor-unit-tests, ui-smoke, selected ui-tests and eleven s17 invariants to the supervisor after worker exit. Test identities are limited to the slice's declared effects.

Attempt 02 uses the supervisor's recorded build failure in 0154-attempt-02-task.txt as its diagnostic: FS0039 at MainWorkbenchTests.fs:76. The source cause is repaired, but no successful rebuild or test result is claimed. The same supplied output contains two SYSLIB0051 warnings in vendored MathNetNumerics/Exceptions.cs; this scoped fixture repair does not change that file or suppress those diagnostics. All gates remain pending the supervisor's next execution.

Checkpoint 151 supplies inherited constructor_unit_tests = 1149 and ui_smoke_tests = 215; no whole-suite ui_tests count is recorded there. Slice 0153's operator recovery reports 216 smoke cases, but did not replace that inherited checkpoint. This slice adds exactly three smoke facts; neither an expected count nor a source inspection is reported as a measurement. commit_ready is false pending supervisor verification, with no implementation blocker.

## Artifacts

No runtime artifacts generated. Any persistent run artifacts belong under specs/0050/.artifacts.

Retry evidence: specs/0050/.artifacts/0154-attempt-02-task.txt (supervisor-provided build output; unchanged).

## Gotchas

- Manual/010 overrides the prose's scratch-then-load recipe: initialize from opened.library.kit directly.
- CatalogueImportContext.create is a Storage module extension, so a caller must open OpticalConstructor.Storage.CatalogueImportStore as well as the Domain type's module.
- The pre-existing .manifest.overrides.jsonl and .manifest.state.json changes belong to the supervisor and remain untouched.
- The scoped elevation run must include Ui and its dependents because of the authorized initializer extraction. The compiled gate roster and selection are unchanged.
- The old constructor tryCreate factories were removed; scratch scene entry points remain Result-returning and reuse the same supplied-kit seed. No durable shape, schema, migration, fallback, parser or serializer was added.
- No Git mutation, version change, analyzer configuration change, gate/census/checkpoint change, ASG/STL modification or notebook edit was performed.

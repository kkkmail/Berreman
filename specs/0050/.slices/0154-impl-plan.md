# Implementation plan — 0154

Wire Main and Inverse to an OpenedKit supplied by the launcher. Reuse the existing supplied-kit scene initializer and kit overlay, then load the opened content; manual/010 forbids creating a discarded scratch kit. Each window owns closing its kit and logs any close error. Inject the folder-picker factory into LauncherWindow and use one shared folder/mode/access opening function, with an accessible wrapping error control removed after success.

Update the authorized App, Controls and UI-test files. Manual/010 also authorizes extracting the existing inverse seed into initInverseWithKit in Ui/TableAndElementRotationView.fs; retain initInverse as the Result-returning scratch entry point delegating to it. Add the shared immediate picker fixture, adapt existing window fixtures without removing assertions, and load MainWorkbenchTests from an opened kit. Extend the two existing launch facts and add exactly the three declared smoke facts covering saved experiments, cancellation and malformed content. Exercise actual buttons and inspect the real window/control tree, files and lock lifetime.

Risks are fixture lock ownership, asynchronous UI-thread continuations, snapshot resolution through overlaid stores, and preserving existing test identities and assertions. Keep file fixtures under test output and use existing typed storage APIs. Preserve supervisor-owned manifest changes. Build, tests, analyzers and invariants are executed exclusively by the supervisor under WIRE_UI Invariant 6; this worker records no measured gate outcomes.

## Attempt 02 repair

The supervisor reports FS0039 at MainWorkbenchTests.fs:76: CatalogueImportContext.create is not in scope. The factory is an existing type extension declared in OpticalConstructor.Storage.CatalogueImportStore; MainWorkbenchTests opens only the Domain module. Import the Storage extension module in that authorized test file, retaining the supplied-kit fixture, all assertions and test identities. Update this log and the state-of-the-world for the retry; leave executable verification to the supervisor.

# Implementation plan — 0156

Wire `LauncherWindow` to the existing `ImportDialogWindow` using `ResolveKitSnapshot` whenever the opened kit has snapshot rows, including all-Reuse rows. Apply rebuilds the overlay through `withRebindings` before opening the requested constructor. Cancellation closes the pending kit; an overlay failure closes it and uses the existing launcher error surface. Keep the empty-snapshot and read-only paths on the same opener.

Modify only `OpticalConstructor.App/Program.fs` and `OpticalConstructor.Ui.Tests/LauncherTests.fs`. Preserve `WireUiFinalCompositionTests.fs`. Add exactly the three declared smoke identities, driving the real launcher and dialog through existing IDs, observing opened windows, snapshot-backed material availability, kit locks, database entries and import logs. Reuse canonical snapshot serialization and test-owned kit folders.

Risks are ownership transfer during Apply/Closed, accidental auto-confirmation of Reuse rows, and inadvertently mutating the shared library or a read-only kit. Keep one local Apply flag at the window boundary and retain the existing constructor close handler.

The WIRE_UI worker protocol assigns all gate execution to the supervisor. This worker will implement assertions and record unexecuted verification; it will not run builds, tests, analyzers or invariant checks, or author measured baselines.

Retry 02 repairs the overlay proof: step 0142 deliberately leaves material listings unchanged, so browser membership cannot demonstrate kit-only resolution. Expose the actual stores supplied to the Main constructor model through a read-only composition property, then assert both per-id and exact-version material resolution against the canonical snapshot decoder. Keep the database absence, visible import rows, Cancel/reopen/Apply, lock and no-write assertions, the explicit all-Reuse invocation, and all three test identities. Do not change production listing behavior or gate/census authority.

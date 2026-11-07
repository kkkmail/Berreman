# Implementation plan — 0144

Implement S42-DIALOG in the three authorized F# files. Extend the existing import request with the kit context and its resolved rows, expose its heading and success summary, and route Apply through `KitLibraryContext.confirm`. Keep the existing row rendering, diff, messages and Apply/Cancel handling. Specialize only the four requested action captions for `KitSnapshotOrigin`.

Add exactly the three authorized facts to `ImportDialogTests`, preserving all existing identities and assertions. Build their shared fixture from the Built-in multilayer sample's snapshot closure, reopen the same in-memory folder through a second read-only proxy, and resolve against an empty receiving library with recorded writes and pins. Cover pending defaults, all rows including reuse, successful confirmation, dependency refusal, cancellation, and origin-specific captions through the existing semantic messages and headless view helpers.

Risks are accidentally using the generic apply path without pinning, filtering reuse rows, changing catalogue captions, or mutating the kit or library before explicit confirmation. Use the existing domain operations and recording callbacks to expose those errors. No durable format, window, ID, message, package or version changes are needed.

Source inspection also requires one-line exhaustive-match repairs in `LibraryWindowView.fs`, `OpticalConstructor.App/Program.fs`, and `LibraryWindowTests.fs`. These consumers already match the request union and must recognize its added case to satisfy the zero-warning requirement. Record the deviation from the named edit list in the implementation log; retain their existing behavior and assertions.

The IMPLEMENT worker protocol reserves executable verification to the supervisor. Do not run build, tests, analyzers or invariant gates, or claim passing results. Write the implementation log and state of the world at the supplied paths; leave the pre-existing manifest-state edit untouched.

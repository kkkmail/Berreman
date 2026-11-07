# Implementation plan — 0108

## Attempt 03 — preserve coverage within the declared test identities

Repair the measured ui-tests observation mismatch in ImportDialogTests.fs only. The slice authorizes exactly two new test identities, and the prior attempt added seven independently discovered adapter facts. Keep each adapter scenario and every assertion as a private helper, and call all seven from the already declared window smoke acceptance, outside its UI-thread dispatch. Preserve the original smoke acceptance assertions and the reachability test unchanged. This keeps the reviewed handler coverage while restoring the authorized discovery roster; no spec, oracle, selection, baseline or supervisor state needs editing.

Build Release and run the selected UI classes and all smoke tests under the session's verification requirement. Compare the resulting TRX identities with the retained attempt-01 accepted selection, and compare each helper body with its attempt-02 fact body. Retain fresh evidence under specs/0050/.artifacts/0108-attempt-03-* and update the required worker outputs. Production code is unchanged, so prior elevation and invariant evidence applies without repeating those checks.

## Attempt 02 — registered drop-handler coverage

Address the review's unexercised public adapter in ImportDialogTests.fs only. Install EntryFileDrop.acceptEntryFiles on headless test windows and raise Avalonia's actual routed DragOver and Drop events with controlled data-transfer items. Assert AllowDrop, file-only Copy feedback, one complete ordered FileName-list callback, and no delivery for empty, unavailable or failing platform data, including a later path failure after a valid first path. Keep the two declared acceptance methods and their assertions unchanged; Library/Materials wiring remains outside this slice.

Use the installed Avalonia API and small private test doubles at the platform boundary. Rebuild and run the selected UI classes plus the existing smoke acceptance, retaining logs/TRX under the supplied artifacts folder. Existing product/analyzer/invariant evidence remains applicable if this repair changes tests only. Update the exact worker outputs with current measurements and distinguish previous checks from retry checks.

Implement the S32-DIALOG-WINDOW obligation within the four authorized source/project files. Add the two specified acceptance facts first, reusing ImportDialogTests' in-memory fixture and semantic-ID click helper and ScrollExtentTests' existing small-screen reachability law. The latter will show every built-in envelope plus a collision with two held identities.

Add ImportDialogWindow directly after ImportDialogView in compilation order. Reuse the existing context initializer, update and projection in a HostWindow, with the prescribed title, identity, size and WindowFit hook. Add EntryFileDrop.acceptEntryFiles in the same file as the single platform adapter: file-only Copy feedback, ordered FileName conversion, no callback for empty drops, and contained platform exceptions. Wiring the adapter into Library/Materials belongs to the later explicit slices.

Risks are the Avalonia 12 data-transfer API, clipped collision options, and closing during Elmish dispatch. The two headless acceptance facts exercise the real host and import seam. Session-level verification instructions take precedence over the worker prompt's act-only wording: run the resolved build, elevation, smoke, selected UI and eleven invariant checks, keeping persistent evidence under the supplied .artifacts directory. Preserve the supervisor's existing manifest edit; change no version, schema, baseline, analyzer policy or external repository.

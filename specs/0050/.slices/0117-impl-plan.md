# Implementation plan

Implement spec 0050 slice 0117 in the five authorized source files. Add the catalogue export target and errors, inject catalogue identity generation, and compose export from the existing latest-envelope, reference, catalogue validation, pinning and folder-write seams. Validate names and refuse an existing manifest before any pin or write; follow only user-managed references, retaining explicit built-in selections.

Extend the shared import-store contract with six catalogue export facts so both stores exercise dependency closure, explicit reuse confirmation, pinning, duplicate names, occupied folders, whole-library exports and empty exports. Add the SQLite all-kind pin fact, folder target naming fact, and new error rendering cases. Use the existing BuiltinCatalogue fixture and fresh GUID folders under the test output's LibraryExportTests directory.

Keep import confirmation behavior unchanged. Record implementation progress and write the required state-of-the-world handoff. The IMPLEMENT worker protocol reserves all check and gate execution to the supervisor; no local build, test or analyzer run will be performed. Risks are preserving refusal ordering, avoiding duplicate/cyclic reference traversal, and pinning every kind through the shared proxy.

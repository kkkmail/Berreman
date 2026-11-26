# Where we are

Slice 0110 completes S32-MATERIALS-DROP in S3.2. The Materials window now decides
dropped entry files through its app-owned catalogue import context, opens the
existing import dialog, and displays the applied summary or a typed refusal.
Implementation and all resolved checks are complete; formal acceptance remains
supervisor-owned.

# What's working

- Wired Materials-window file drops to pending import choices without writing before Apply.
- Displayed wrapping import summaries and refusal captions under a stable automation ID.
- Preserved Browse/Select mode, selection and existing confirmation gates during import.
- Added four UI facts and a real-app smoke acceptance proving Apply refreshes the live library and tree.

# Tests

- Release solution build passed with zero warnings/errors in 13.39 seconds.
- Elevation passed with zero findings in five projects, 132 seconds; ELEV004 excluded.
- All-discovered ui-smoke passed 213/213 cases, zero failures/skips.
- The four selected ui-tests classes passed 57/57 cases, zero failures/skips.
- All eleven s13 invariants passed using the resolved snapshot commands.

TRX identity comparison against slice 0109 confirms exactly one added smoke case
(212 to 213) and four added cases in the same selected UI classes (53 to 57).
Every declared new test passed and no existing case disappeared. Edited source
files are LF-only, their whitespace review passed, and normal/ignore-CR diff
statistics match. The initial red build detected the absent new surfaces; a
missing Builder namespace was repaired before the final build.

```yaml
gates:
  ui_smoke_tests: 213
  ui_tests: 57
```

These are measured counts for this slice's selections, not baseline edits.
Evidence is under specs/0050/.artifacts/0110-attempt-01-*; the implementation log
lists the build/elevation logs, test reports, invariant logs and review records.

# Architecture

MaterialsWindowContext gains an injected dialog launcher. DropMaterialEntryFiles
calls the existing decideDroppedFiles member; nonempty decisions open the dialog,
empty decisions preserve the model, and refusals set importError. The composition
root uses ApplyDroppedEntries and dispatches MaterialsImportApplied from the
dialog callback. That message triggers the existing live projections and carries
the shared completion summary. Native drops and the public DropEntryFiles member
dispatch the same message. Status/error state is transient; no durable format,
migration, package or version changes are involved.

# Deferred

None within slice 0110.

# Gotchas

- Material-entry fixtures must hold their referenced category before a drop;
  the standalone test explicitly imports the built-in catalogue first.
- EntryFileDrop lives in ImportDialogWindow.fs and is reused unchanged.
- Import status is a model projection member, matching LibraryWindowModel and ELEV013.
- Session-level verification requirements take precedence over the worker's act-only wording.
- The pre-existing supervisor edit to .manifest.state.json remains untouched.

# Changelog

- 2026-11-07: Wired Materials-window dropped entries to import preview/Apply, inline status and live refresh; added and verified all five specified tests.

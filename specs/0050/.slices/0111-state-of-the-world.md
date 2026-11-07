# Where we are

Slice 0111 completes S32-LIBRARY-DROP in S3.2. The Library window now routes
dropped entry files and catalogue registrations awaiting choices through the
existing import dialog. All slice work and resolved local checks are complete;
formal acceptance and checkpoint updates remain supervisor-owned.

# What's working

- Wired Library file drops to import decisions without applying entries before Apply.
- Opened pending catalogue registrations automatically and added Review imports in Browse and Select modes.
- Removed completed registrations from the pending list and displayed the shared completion summary or typed refusal.
- Preserved selection and confirmation state while refreshing the live Library tree after Apply.
- Added the five declared UI facts and the real-app source-drop smoke acceptance.

# Tests

- Release solution build passed with zero warnings/errors in 37.02 seconds.
- Elevation passed with zero new findings in five projects, 135 seconds;
  ELEV004 excluded as prescribed.
- All-discovered ui-smoke passed 214/214 cases, zero failures/skips.
- The five selected ui-tests classes passed 80/80 cases, zero failures/skips.
- All eleven s13 invariants passed using the unchanged snapshot commands.

TRX identity comparison confirms exactly one added smoke case (213 to 214) and
five added cases in the selected UI classes (75 to 80). Every declared new test
passed and no existing case disappeared. Source review confirms the six
authorized files, LF-only content, no whitespace errors and matching normal /
ignore-CR diff statistics. No product changes followed the passing checks.

```yaml
gates:
  ui_smoke_tests: 214
  ui_tests: 80
```

These are measured counts for this slice's selections, not baseline edits.
Evidence is under specs/0050/.artifacts/0111-attempt-01-*; the implementation log
lists all build, analyzer, test, invariant and review artifacts. Local checks
follow the higher-priority session verification instructions; the worker
protocol's act-only wording does not override them.

# Architecture

LibraryWindowContext now injects openImportDialog over the existing
ImportDialogRequest. DropLibraryEntryFiles uses decideDroppedFiles; nonempty
decisions open ApplyDroppedEntries, empty drops preserve the model, and failures
surface CatalogueImportRefusedErr. RegisterCatalogueFolder opens
SettlePendingCatalogue for awaiting rows. ReviewPendingImports reopens the first
pending catalogue. LibraryImportApplied removes only the completed catalogue id,
publishes applied.summary and clears the error. Other pending imports and
confirmation/session state remain intact.

LibraryWindow composes ImportDialogWindow over its injected app import context
and dispatches completion back into the same MVU loop. Native drops and the
public DropEntryFiles seam dispatch the same message through the existing
EntryFileDrop adapter. All other context constructors supply the new callback.
No parser, import behavior, package, version or durable format was added.

# Deferred

None within slice 0111.

# Gotchas

- EntryFileDrop lives in ImportDialogWindow.fs; request/result types live in
  ImportDialogView.fs. Both are reused unchanged.
- Source identities use ElementValueId.tryToLibraryEntryId, unlike material
  and sample ids' total conversion members.
- The test source file reuses the existing catalogue fixture and entry-file
  serializer. Test folders remain under the test run's output directory.
- The initial red dependency build reported two existing SYSLIB0051 warnings
  in vendored MathNet Exceptions.cs; no suppression or vendored edit was made.
  The final solution build reports zero warnings/errors.
- The initial supervisor edit to .manifest.state.json remains untouched.

# Changelog

- 2026-11-07: Wired Library drops and pending registrations to import review/Apply, added Review imports and completion refresh, and verified all six declared tests and resolved checks.

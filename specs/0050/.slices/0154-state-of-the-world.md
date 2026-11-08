# Where we are

Slice 0154 wires the product launcher and both constructor windows to folder kits after the kit opening, experiment persistence and scene persistence steps. Main and Inverse pick and open a kit before constructing their windows. Attempt 02 repairs the missing Storage extension-module import identified by the supervisor's build failure in MainWorkbenchTests. The WIRE_UI worker protocol assigns executable verification to the supervisor after this handoff; a successful rebuild is not yet established.

# What's working

- Wired Main and Inverse through the injected kit-folder picker and shared opening function.
- Composed each constructor from its opened kit and snapshot-overlaid library stores; released the kit on window close.
- Added the accessible launcher kit-error text and removed it after a successful retry.
- Adapted existing composition fixtures and authored the three declared smoke facts for saved experiments, cancellation and opening failures.
- Reused the inverse seed through a supplied-kit initializer, preserving the fallible scratch entry point.
- Imported the canonical CatalogueImportContext factory extension in MainWorkbenchTests to address the reported FS0039.

# Tests

No build, test, analyzer or invariant gate was executed by this worker. The supervisor owns build, elevation, constructor-unit-tests, ui-smoke, the selected ui-tests classes and all eleven s17 invariants. Existing test identities and assertions were preserved; exactly three ui-smoke identities were added in LauncherTests. Those assertions drive real launcher buttons, inspect the opened constructor's experiment row, verify metadata/lock lifetime and inspect the launcher's error control, cancellation and successful retry.

The retry task supplies a failed supervisor build: CatalogueImportContext.create was out of scope in MainWorkbenchTests.fs:76. Attempt 02 adds the existing Storage extension module import without altering tests or product behavior. Build and subsequent gates await supervisor execution. The supplied build output also records two vendored MathNet SYSLIB0051 warnings; this repair neither changes nor suppresses them.

The following counts are inherited from checkpoint 151, not measured or authored by this round. The 0153 operator recovery reports 216 smoke cases without replacing the inherited checkpoint. No whole-suite ui_tests baseline is invented, and the three new facts are not counted as passed before execution.

```yaml
gates:
  constructor_unit_tests: 1149
  ui_smoke_tests: 215
```

# Architecture

KitOpening.openKitAt retains ownership of initialization, reading, overlay resolution and failure cleanup. ConstructorScene.storesOf calls KitLibraryStores.overlaidWith once per window. The window initializes directly from opened.library.kit, threads the shared application proxies and loads the recorded content through loadKit. Its Closed handler calls the supplied kit's close and logs any typed error through Logger.logError.

Main uses initMainWithKit. The inverse seed had no equivalent supplied-kit entry point, so manual/010's construction authority was applied to extract initInverseWithKit in Ui/TableAndElementRotationView.fs; initInverse delegates after scratch construction succeeds. The old body was removed from initInverse. This is the necessary additional Ui edit beyond the original effect file list, with no duplicated seed or discarded scratch kit.

LauncherWindow receives TopLevel -> KitFolderPicker. Async.StartImmediate starts the picker on the UI thread; cancellation does nothing, and a selection calls the shared folder/mode/access opening function. Errors are rendered as a wrapping TextBlock with the centralized Name, AutomationId and diagnostic accessible name, between the title and surface actions. Success removes the error. Materials and Library retain their existing shared launchers. No durable shape or serialization format changed.

# Deferred

No implementation work was deferred. Executable verification remains assigned to the supervisor by the act-only worker protocol; commit_ready is false until that verification is available.

# Gotchas

- Manual/010 forbids scratch construction followed by replacement when a kit is already supplied. Keep the supplied-kit initializers as the shared seed owners.
- CatalogueImportContext.create belongs to OpticalConstructor.Storage.CatalogueImportStore; opening only the Domain module does not expose that extension.
- Each constructor owns a distinct opened kit. Fixture writers close before the launcher reopens their folder, and MainWorkbenchTests closes both mounted and pure-model kit contexts.
- The malformed experiment fixture uses the canonical E1.experiment-json filename and deliberately invalid JSON; openKitAt must release its failed-open lock.
- Test folders live under test output. Cancellation compares the configured kit-folder inventory; no fixture writes into data or user Documents.
- Source diff line counts agree with --ignore-cr-at-eol. The supervisor's pre-existing manifest edits remain untouched.
- Gate snapshots, census/oracle, checkpoints, analyzer settings, version rings, ASG/STL and notebooks were not modified. No Git mutation was performed.

# Changelog

- 2026-11-08: Wired kit picking, constructor ownership and launcher diagnostics; reused supplied-kit initialization, adapted fixtures and authored three smoke facts for supervisor verification.
- 2026-11-08 (attempt 02): Added the missing Storage factory extension import to MainWorkbenchTests after the supervisor's FS0039 diagnostic; retained acceptance coverage and handed verification back to the supervisor.

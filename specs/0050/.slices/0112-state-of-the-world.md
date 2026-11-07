# Where we are

Slice 0112 wires the Launcher to the existing import decision dialog for
S32-LAUNCHER-PENDING in S3.2. Implementation is complete; the resolved local
checks all pass, with formal acceptance and checkpoints owned by the supervisor.

# What's working

- Wired startup and Review imports to the complete pending catalogue decisions.
- Counted every waiting row and removed only successfully applied catalogues.
- Preserved pending work and stored entries on Cancel.
- Added real-launcher coverage for fresh Add, all-Reuse, and mixed-conflict imports.
- Kept populated launcher fixtures explicitly settled before opening.

# Tests

- Confirmed the startup regression before wiring: no import dialog opened;
  six existing launcher smoke cases passed.
- Release solution build passed with zero warnings and errors (16.88 seconds).
- Elevation passed with zero findings in five projects (144.05 seconds),
  excluding ELEV004 as prescribed.
- All-discovered ui-smoke passed 215/215, with no failures or skips.
- Selected LauncherTests and WireUiFinalCompositionTests ui-tests passed 9/9,
  with no failures or skips.
- All eleven s13 invariants passed.

TRX review confirms exactly one added smoke case (214 to 215) and exactly the
declared new UI fact alongside all eight existing selected fact identities.
No cases disappeared. The acceptance covers fresh Add rows, all-Reuse rows,
mixed Add/Reuse/name-collision rows, unchanged entries and logs on Cancel,
complete settlement on Apply, and an unchanged restart with no pending work.

```yaml
gates:
  ui_smoke_tests: 215
  ui_tests: 9
```

These are measured results for this slice's selections, not checkpoint edits.
Logs, TRX files, and the acceptance review are under
`specs/0050/.artifacts/0112-attempt-01-*`, listed in the implementation log.
Source review found only the four authorized files, LF-only content, no
whitespace errors, and no line-ending churn. No product edits followed checks.

# Architecture

The launcher owns the waiting list at the window IO boundary. Its startup
handler and review action construct the existing `ImportDialogWindow` over the
shared `CatalogueImportContext`. Successful Apply reports the request, removes
that catalogue ID, and rebuilds the existing launcher content. Cancel does not
change the list. The launcher never imports rows itself. Dialogs are owned
modeless children, so closing the launcher closes its dialog too. Successful
library recreation refreshes the waiting list from the new context.

The two new automation IDs live on `ConstructorUiId`. No domain, storage,
package, version, or durable-format changes were needed.

# Deferred

None within slice 0112.

# Gotchas

- `TestAppScope.freshAppContext` settles imports but retains the original
  immutable pending snapshot. Both authorized launcher fixtures clear that
  snapshot after the helper explicitly settles it; no shared fixture changes
  or implicit product imports were introduced.
- Writable catalogue fixtures live beneath the isolated test-output locations,
  never in the shared shipped catalogue or committed data folders.
- The session's higher-priority verification instructions require local checks
  despite the task-file act-only wording. Supervisor checkpoints and frozen
  contracts remain unchanged.
- The pre-existing `.manifest.state.json` edit belongs to the supervisor and
  was left untouched.

# Changelog

- 2026-11-07: Wired pending startup imports into the Launcher and added both
  declared acceptance facts, covering Add, Reuse, and mixed conflicts; all
  resolved local checks passed.

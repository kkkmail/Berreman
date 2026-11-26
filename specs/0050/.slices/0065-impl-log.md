# Step 0065 implementation log

## Progress

- Read the worker protocols, repository rules, slice and resolved gate roster.
- Before implementation: map the existing usage seam, store histories, proxy
  records and test fixtures; reuse the existing PinState introduced in step 0061.
- Implemented Domain replacement: per-version pins, idempotent pin verbs, latest
  pin decision input, pinned removal errors and shared unknown-material-version
  diagnostics. Removed the usage lookup types/functions and experiment adapters.
- Updated all 40 authorized source/project files, including fixtures, UI error
  matches and proxy stubs. Renamed declared facts, removed the two seam facts and
  added the two pin facts. These also exercise preservation across metadata and
  lifecycle edits and the diagnostic list for multiple pinned versions.
- Before local verification: review record construction, caller signatures and
  documentation for stale usage assumptions, then build both constructor test
  projects and execute relevant tests. No acceptance result is claimed.
- Source review found exactly the 40 declared source/project files changed, no
  undeclared source edits, all declared live fact names present and no remaining
  retired usage symbols. Fact counts: LifecycleTests 16 -> 14, MaterialProxyTests
  24 -> 25, SampleProxyTests 29 -> 30 (net zero). Changed sources use LF, with
  matching normal/ignore-CR diff counts and clean whitespace inspection.
- Local development builds succeeded for Ui.Tests and Tests with dependencies.
  The constructor build reported zero warnings/errors. The UI dependency build
  reported two pre-existing SYSLIB0051 warnings in vendored MathNet Exceptions.cs;
  there were no compiler/project warnings in Optical Constructor code.
- All 159 tests in the nine affected constructor classes passed, with zero
  failures or skips. All 303 tests in the 14 affected UI classes then passed,
  also with zero failures or skips. This includes the selected classes' smoke
  facts and the Analytics acceptance tests. No implementation work remains.

## Files modified

- Worker plan and this implementation log at the supervisor-supplied paths.
- The 40 Domain, Ui, Tests and Ui.Tests files enumerated by the slice's effects.

## Testing state

Acceptance gates have not been run. The supervisor runs the compiled roster.
Local development builds, 159 selected constructor tests and 303 selected UI
tests passed. These results do not assert an acceptance gate pass. The supervisor
must run the resolved roster; commit readiness is not asserted ahead of it.

## Artifacts

- Persistent run output belongs under `specs/0050/.artifacts/`.
- `0065-local-build.log`, `0065-local-constructor-build.log`.
- `0065-local-constructor-tests.log`, `0065-local-constructor-results/` (TRX).
- `0065-local-ui-tests.log`, `0065-local-ui-results/` (TRX).
- `0065-source-review.py` and `0065-source-review.log`; editing scripts also
  remain in this artifacts directory.

## Gotchas

- The initial working tree contains a supervisor-owned modification to
  `specs/0050/.manifest.state.json`; leave it untouched.
- The development environment's required local verification takes precedence
  over the worker protocol's act-only restriction. Gate authority remains with
  the supervisor; do not invent baseline counts or green-light acceptance.
- Explicit pins freeze physics, not metadata. This follows the slice's decision
  table; no pin is inferred from an experiment's version reference. The version
  id constructors return references and do not pin the store.
- Latest available supervisor checkpoint for constructor tests is step 63 (807);
  UI smoke is step 57 (208). There is no ui_tests checkpoint in the current file.

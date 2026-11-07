# Where we are

Slice 0118 implements S33-SHARED-WINDOW-PIECES. The approved operator recovery is
verified and ready for commit; the arc remains stopped and escalated. The failed
attempt was committed as `0552847`; accepted 0117 is `32c7211`. The operator's
census decision is `.manual/008-step-0118-picker-tests-recovery.md`.

# What's working

- Both library windows use MaterialsWindowView's shared ordinary verb helpers.
  The shared renderer preserves captions, stable ids, keys, appearance and handled
  press delivery. The distinct colored backup-selection renderer remains.
- The shared folder picker resolves an optional start location, selects only the
  first returned folder and posts its callback on the UI thread. Cancellation
  and unavailable providers leave the caller unchanged.
- AppContext's composition shared by create and recreateLibrary supplies
  `Some locations.cataloguesFolder`; the standalone import factory keeps `None`.
- All four new AppContextTests facts pass. The fixture uses public app-builder
  provider registration, isolated headless sessions and a nonparallel collection;
  runtime storage proxies avoid Avalonia's NotClientImplementable interface issue.

# Operator repair and authority

The FS0905 collision is removed by renaming the private binding to
`providerInstance`, retaining the `storageProvider` member. The first runtime
check then found Avalonia returns the starting directory URI with a trailing
separator. The assertion now trims that separator before comparing the same
requested directory. Every assertion and all four identities remain; product
code was not changed during this recovery.

The previous handoff incorrectly called the three extra picker facts
retry-authorized. A coverage retry and selected class authorize neither new test
identities nor a census change. The operator has now retained the facts in
`.manual/008`, with three exact `create` rows effective at step 118 in
`.operator-test-effects-jsonl`. The earlier step-52 rename is preserved. The
published step's one declared fact plus those three effects gives four new facts.

# Measured verification

All 15 unchanged 0118 gates passed on 2026-11-07:

- Build: 0 warnings, 0 errors.
- ui-smoke: 215 passed, 0 failed, 0 skipped; exact census matched.
- Selected ui-tests: 126 passed, 0 failed, 0 skipped; exact census matched,
  including the configured-folder fact and all three picker facts.
- Elevation and all eleven invariant-s14 gates passed. Because the product code
  was already committed, a supplementary elevation run against accepted 0117
  covered the four affected product projects and found zero new findings.

The selected classes remain AppContextTests, ImportDialogTests,
LibraryWindowTests, MaterialsWindowTests and ScrollExtentTests. The 126 selected
cases are not a whole-suite baseline. The smoke floor remains 215; the resolved
constructor baseline remains 1014. Expected-red record 116 is empty.

Full publication validation passed through step 0494. The profile-1 receipt now
binds the operator effects and updated ASG prompts; all 988 compiled artifacts
remain byte-identical. Published requirements, lifecycle/origin registries,
supervisor state, counters, checkpoints and expected-red records are unchanged.
No checkpoint 117 was created and no version was bumped.

Evidence: `.artifacts/0118-operator-repair/verification-summary.json`,
`gate-results.json`, `test-inventory.json`, copied TRX reports,
`elevation-from-0117.log` and `final-review.md`. The initial failed picker run is
retained as `picker-preflight.log`. The final gate build has zero warnings; the
initial rebuild reported two existing SYSLIB0051 warnings in vendored MathNet.

# Separate ASG repair

ASG 0090 `.manual/031-auth-evidence-and-test-census-authority.md` records shared
provider-auth evidence filtering and explicit judge/worker census authority.
377 relevant tests and strict mypy passed. Real auth/quota signals remain
recognized; XML/source text read by a successful worker no longer suffices.
Repair budgets and coverage obligations are unchanged.

# Handoff

Wait for the operator to commit ASG and BRM. No commit or relaunch occurred in
this repair. After commit and a relaunch instruction, check readiness and use
`--accept-current-slice --executability-profile 1` with a fresh monitor ticket.
Do not reset counters, fabricate checkpoint 117, or promote the selected UI count
to a whole-suite floor. No completion stat tools are due while the arc is escalated.

# Changelog

- Attempt 01: extracted shared window helpers and configured the export suggestion.
- Attempt 02: added real helper coverage; unsupported locator API failed to compile.
- Attempt 03: switched to public provider registration; direct storage interfaces
  failed the NotClientImplementable check.
- Attempt 04: replaced interfaces with runtime proxies; the binding/member name
  collision still failed compilation.
- Operator recovery, 2026-11-07: repaired fixture compilation and URI comparison,
  recorded census authority, verified the full gate roster and left the arc stopped.

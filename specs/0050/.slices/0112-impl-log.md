# Implementation log — 0112

## Progress

- Read the task, worker protocols, project instructions, slice, naming rules,
  and resolved gate snapshot. The specific WIRE_UI act-only rule supersedes
  the legacy local-gate procedure.
- Before implementation: inspect launcher composition, pending-catalogue
  settlement, import-dialog controls, and existing headless fixtures.
- Inspection found that `TestAppScope.freshAppContext` explicitly settles the
  database but returns its original immutable `pendingImports` snapshot. The
  two authorized launcher fixtures therefore need a local empty pending list
  after that settlement; the shared fixture and unrelated tests stay unchanged.
- Before edits: add the declared structure fact and one startup acceptance fact
  covering fresh Add rows, a changed manifest with only Reuse rows, and a mixed
  Lab-glass collision with Add/Reuse rows. Reuse the existing serializers,
  catalogue writer, import proxies, and semantic control IDs.
- Verification instruction resolution: the session's higher-priority developer
  instructions require appropriate tests and required checks. They take
  precedence over the task-file act-only rule. Local verification will run;
  formal gate acceptance and measured checkpoint ownership remain supervisor-owned.
- Added the two IDs, both declared facts, and local settled-context fixture
  updates. The first compile caught a test helper using `MaterialError.describe`;
  corrected it to the existing `caption.value` renderer. The next Release
  solution build passed with zero warnings/errors.
- Confirmed the startup regression before product wiring: launcher ui-smoke
  ran seven cases, six passed and the new acceptance failed because the observed
  opened-window collection was empty. This is the missing startup-dialog path.
- Before product wiring: keep pending imports at the launcher IO edge, reuse
  `ImportDialogWindow` unchanged, and rebuild the content only after successful
  Apply or library recreation. Own the dialog from the launcher so closure does
  not leak a child window. Guard startup opening to its first Opened event.
- Completed product wiring and all resolved local checks. The real-window
  acceptance passes for fresh Add rows, an all-Reuse manifest change, and mixed
  Add/Reuse/name-collision rows. Cancel preserves held entries, materials, and
  import logs; Apply settles every row, removes the notice, and leaves both
  Lab-glass identities held. Reopening each unchanged catalogue has no pending
  import.
- Final acceptance review compares TRX identities against the prior smoke run
  and the selected classes' pre-change source: exactly one added smoke case
  and one added selected UI fact, with no removed cases. All four authorized
  source files use LF, have matching normal/ignore-CR diff statistics, and pass
  whitespace review. No product edits followed the passing checks.

## Files modified

- `specs/0050/.slices/0112-impl-plan.md`: implementation approach.
- `specs/0050/.slices/0112-impl-log.md`: durable progress record.
- `OpticalConstructor.App/Program.fs`: waiting list, complete-row notice,
  review action, first-open dialog, successful-Apply removal, and pending-state
  refresh after library recreation. Import dialogs are owned modeless children,
  consistent with the existing import surfaces.
- `OpticalConstructor.Controls/UiIds.fs`: two documented launcher IDs.
- `OpticalConstructor.Ui.Tests/LauncherTests.fs`: two declared facts and the
  explicitly settled launcher fixture.
- `OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`: only the
  already-settled launcher's pending snapshot is cleared; composition behavior
  and assertions remain intact.

## Testing state

- Red regression confirmed: six existing launcher smoke cases pass; the new
  startup fact fails because no import window opens.
- After wiring, the resolved Release solution build passes with zero warnings
  and errors (16.88 seconds).
- Elevation passed with zero findings across five projects (144.05 seconds;
  ELEV004 excluded as prescribed).
- All-discovered ui-smoke passed 215/215, zero failures or skips (88.04 seconds),
  exactly one case above the previous measured 214.
- Selected ui-tests passed 9/9, zero failures or skips (11.61 seconds).
  Pre-change source has eight selected fact identities; the ninth is precisely
  the declared no-pending-notice fact.
- All eleven s13 invariants passed with the unchanged snapshot commands.
- Test result-directory arguments route TRX files into the supplied artifacts
  folder; selections and assertions are unchanged. Formal acceptance and
  checkpoint updates remain supervisor-owned. No blocker remains.

## Artifacts

- `0112-attempt-01-red-build.log`, `0112-attempt-01-red-build-fixed.log`:
  initial test-helper compile correction and successful pre-wiring build.
- `0112-attempt-01-red-tests.log`, `0112-attempt-01-red/launcher.trx`:
  observed missing-dialog regression.
- `0112-attempt-01-build.log`: successful post-wiring build.
- `0112-attempt-01-checks.py`: resolved command runner; remaining gate logs,
  `0112-attempt-01-check-results.json`, and `0112-attempt-01-trx/` capture outcomes.
- `0112-attempt-01-review.py`, `0112-attempt-01-acceptance-review.json`:
  exact test-identity additions, no removals, and source/line-ending review.

## Gotchas

- The working tree already contains an operator/supervisor change to
  `specs/0050/.manifest.state.json`; leave it untouched.
- The required smoke fact must cover both fresh nonconflicting imports and
  mixed conflicts, counting all decisions rather than only collisions.
- The fixture correction in `WireUiFinalCompositionTests` is necessary because
  the spec's assumed empty pending snapshot is stale in the current shared
  helper. Only the settled snapshot supplied to the launcher changes; every
  composition assertion and other window keeps its existing code.
- The first full compile emitted two existing SYSLIB0051 warnings in vendored
  MathNet `Exceptions.cs`. No suppression or vendored edit was made; both later
  solution builds reported zero warnings and errors.

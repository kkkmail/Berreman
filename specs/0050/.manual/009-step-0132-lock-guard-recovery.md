# Step 0132: bounded kit-lock guard recovery

The operator approved the four-step plan in `.slices/0132-01.postmortem-review-md`.
The failed implementation and reports are committed as `90b0649`;
accepted 0131 remains `6d45e90`. Preserve the four S42 attachment facts and their
implementation. The arc stays stopped until the operator commits this repair
and explicitly instructs a relaunch.

This ruling extends step 0132's repair authority to
`Berreman/OpticalConstructor/OpticalConstructor.Storage/KitFolderStore.fs` and
existing lock regression coverage in
`Berreman/OpticalConstructor/OpticalConstructor.Tests/KitProxyTests.fs`, under
S41-KIT-LOCK. It authorizes a narrowly scoped IO proxy for deterministic testing
of the same guard retry used by production. Preserve every existing assertion
and test identity. No new discovered facts or census amendment is required.

The shared guard must retain exclusive cross-process/file coordination,
delete-on-close cleanup, ownership rechecks and non-overwriting publication.
Retry only guard-open access-denied native code 5 and sharing/lock violations
32/33. Use at most 101 opening attempts with up to 100 ten-millisecond waits;
scheduling and the native IO call can add elapsed time. Other failures return
immediately, and exhausted retries return `KitIoErr` carrying the original
exception type, HRESULT and guard path. Acquisition and release use this same
policy. A failed release retains this proxy's ownership for a subsequent retry.
Do not add an unbounded wait, discard a foreign owner or leave temporary files.

All later edits to this shared lock code must preserve the repaired contract,
including steps 0133, 0138, 0196, 0204, 0307 and 0484. The published ledger,
oracle and gate snapshots are unchanged. Validate all 494 steps and refresh the
profile-1 receipt after adding this ruling's project-prompt pointer; preserve
all 988 compiled artifacts byte-for-byte.

Verification must cover repeated synchronized empty/stale/malformed contention,
deterministic transient/permanent guard failures, release failure/recovery,
foreign ownership and cleanup. Run all 14 unchanged 0132 gates and require all
1,076 constructor cases with exact census, zero failures/skips and no warnings
from owned code. Evidence belongs under `.artifacts/0132-operator-repair/`.

This recovery is an explicit exception to the standing committed-green
`--accept-current-slice` rule. After the operator's commit, resume 0132 normally
with `--operator-reply <recovery-note> --executability-profile 1`. The worker must
preserve the verified repair, review both S41 and S42 obligations and return it
for normal supervisor verification. Only that successful full run publishes
checkpoint index 131, raising the constructor floor from 1,072 to 1,076, and
completes expected-red bookkeeping. Do not accept the slice directly, fabricate
a checkpoint, reset counters, waive a failure or change the supervisor state.

ASG source requires no change for this incident.

The recovery build exposed FS0046 warnings for two pre-existing local bindings
named `process`, in Domain `Kit.fs` and Storage `KitFolderStore.fs`. Rename only
those locals to `processText` and `ownerProcess`, preserving rendering and process
liveness behavior, to satisfy the required zero-warning build. The attachment
implementation remains unchanged.

## Measured completion (2026-11-07)

The original delete-on-close primitive reproduced native sharing violation 32
and access denied 5 under synchronized contention on this Windows machine.
The warning-free reproduction recorded 6,883 sharing failures, 1,382 access
denials and 7,735 successful opens, with no guard file left behind. The original
incident did not record its HRESULT; the reproduced mechanism supports the
diagnosis without claiming that missing measurement.

The shared bounded guard now serves acquisition and release. Existing facts
exercise 25 rounds each of empty, stale and malformed filesystem contention,
scripted retry limits and immediate failures, permanent sharing/access denial,
and ownership retention after failed release. All existing assertion lines and
fact identities remain. The attachment tests are byte-identical to `90b0649`.

- Focused kit suite: 58 passed, zero failed or skipped.
- Three independent-process scenarios (empty/stale/malformed), each with three
  contenders: one winner, two losers naming that winner, clean release.
- All 14 unchanged slice gates passed. Final build: zero warnings/errors;
  elevation: seven affected projects, zero new findings; constructor suite:
  1,076 passed, zero failed/skipped, exact census. All eleven invariants passed.
- All 494 steps validated; all 988 compiled artifacts remain byte-identical.
  The profile-1 receipt was refreshed for the project-prompt pointer and passes
  validation. Published requirements, oracle, state, checkpoints, expected-red
  records, test baselines and contract registries remain unchanged.

Detailed evidence and review: `.artifacts/0132-operator-repair/final-review.md`.
Future normal-resume handoff: `.artifacts/0132-operator-repair/operator-reply.md`.
The repair is ready for the operator's commit. No commit, checkpoint publication
or relaunch was performed. ASG is unchanged.

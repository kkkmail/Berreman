# Step 0210: ui-smoke deadline recovery

2026-11-09. The operator approved solutions 1 and 2 in
`.slices/0210-01.postmortem-md`: raise the smoke deadline and file the ASG defect,
then wait for the operator's commit. The arc is stopped at index 209 / step 0210.
The slice work and postmortem are already committed as `90ab50b`; no isolation of
the recovery from an escalation commit is required.

## Cause and approved change

The supervisor killed ui-smoke after 180.3 seconds without a completed result.
The worker's completed runs passed all 237 cases in 192.0 and 193.7 seconds.
The descriptor omitted `attempt_timeout_seconds`, so the runtime allocation
`runtime_estimate_seconds: 180` also became its hard deadline. The suite has
grown with the app's startup work. A timeout is not evidence of a failing test.

Add only `attempt_timeout_seconds: 600` to the ui-smoke descriptor in both
`specs/0050/.spec-gates` and `.gates/OpticalConstructor/ui-smoke.gates`. The
frozen spec roster supplies this arc; the live catalog supplies future specs.
Keep the command, selection, pass conditions, count baseline, TRX capture and
skip policy unchanged. The runtime estimates remain estimates; this change
explicitly separates the execution allowance from them.

This is a wall-clock allowance, not weaker acceptance. It is the operator's
specific exception to the frozen-descriptor restriction and is consistent with
manuals 013/014: no failure is masked, no test is retired and no floor is lowered.
600 seconds provides headroom beyond the observed 192–194 seconds; a linear
projection over the remaining arc is not a guaranteed runtime prediction.

The project prompt now requires reporting a development run that exceeds its
gate deadline, including the measured duration and effective deadline. ASG's
missing operator notification and missing effective-deadline summary in worker
tasks are filed as an OPEN `asg-error` in ASG spec 0090 manual 034 and
`ASG-ERRORS.md`. This recovery does not implement that ASG source fix.

## Validation and publication

Run the unchanged ui-smoke command once with the new 600-second allowance over
the current Release output. Require all 237 accepted cases, zero failures/skips,
and an exact accepted census. Preserve the measured output and TRX under
`.artifacts/0210-deadline-recovery/`. Build, elevation and constructor tests
already passed under the supervisor; no product, schema, catalogue or test edit
is part of this recovery. Do not rerun `export-item-models.fsx`.

Validate the next boot's compilation in an artifact preview while retaining
all 209 completed slice descriptors. The first preview exposed an omitted
publication step in the postmortem: profile 1 hashes the effective deadline as
part of each command binding, so changing only the two descriptors produces
`COMMAND_DRIFT` and cannot boot. Use ASG's existing `rebind-commands` operation
for this explicitly approved deadline change. It rebinds only ui-smoke's 216
check hashes in `.execution-contract-json` and `.acceptance-oracle-json`.
There is no `.execution-progress-json` in this arc; the operation therefore has
no progress reference to carry. Requirements, checks, checkpoints, completed
slices and commits remain identical. The artifact preview
confirms this exact change and all 494 steps then validate.

Do not rewrite published slice snapshots, the receipt, supervisor state,
numeric checkpoints or consumed counters during this preparation. Normal boot
compilation refreshes the 122 pending smoke snapshots and republishes the
receipt before runtime validation. The old receipt is intentionally stale
between this amendment and that boot. Never patch it by hand or remove profile 1.

## Commit and resume

Wait for the operator's commit and explicit relaunch request. This recovery
overrides the general committed-green acceptance rule: resume normally with an
operator reply and `--executability-profile 1`, without `--accept-current-slice`.
Even though the user committed the implementation, checkpoint 209 is absent.
The supervisor must run normal review and all 19 gates and publish its measured
checkpoint (expected constructor 1,281 and smoke 237) before step 0211.

At that boot, confirm that the pending ui-smoke snapshots, including 0210, carry
`attempt_timeout_seconds: 600`, compilation and receipt validation succeed, and
the old ui-smoke near-budget warning no longer applies. Reattach the PID monitor.
Do not reset consumed counters or edit STL. Original postmortem and worker
reports remain historical evidence.

## Measured result and commit handoff

The unchanged smoke command passed **237 of 237 cases** in **190.9 seconds**
under the explicit **600-second** deadline, with zero failures/skips and an exact
accepted census. This is a single-gate verification; the supervisor must still
perform the normal 19-gate slice cycle after the operator's commit and relaunch.

The 494-step production-compiler preview passed with a valid profile-1 receipt.
It preserves all 209 completed steps and updates exactly 122 pending smoke
snapshots, including 0210 and 0494. The published 988 slice/gate artifacts remain
untouched until boot. The existing rebinding operation was applied to the live
contract/oracle metadata and matched the preview exactly: 216 ui-smoke command
hashes in each metadata file, with no requirement or progress edit.

Evidence is in `.artifacts/0210-deadline-recovery/`: `gate-result.json`, the
stdout/stderr logs, `ui-smoke.trx`, `verification-summary.json`,
`publication-preview.json`, the failed pre-rebind preview and successful rebound
preview, `command-rebind-applied.json` and `final-scope-check.json`.

Product, catalogue and test files, numeric checkpoints, supervisor state and
consumed counters remain unchanged. No commit or restart was performed by the
orchestrator. The budget amendment and the OPEN ASG issue are ready for the
operator to commit in BRM and ASG respectively. The arc remains stopped.

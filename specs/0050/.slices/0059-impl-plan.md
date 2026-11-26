# Step 0059 implementation plan

## Authorized relaunch, 2026-11-06

Preserve the retirement committed in `a6fb67f`. The operator committed the
recovery in `035dc0b` and explicitly authorized this normal profile-1 relaunch.
Read `.manual/006-step-0059-case-count-accounting.md` and review the existing
product change against accepted slice 0058,
`a402443293089bb84b224b3028285cd2bdfa70d0`, and immutable obligation
S21-RETIRE-EF-PROOF. Do not repeat the removal transform or restore its inputs.

Review the seven deleted files, the three narrowly edited files, the retained
SQLite dependency and configuration key, and the four retired test methods.
The original test source and recovered evidence account for one four-row
theory and three facts. Checkpoint 57 remains 801; the authorized decrease is
seven executed cases, producing the expected 794. Preserve the existing
`.test-case-counts-json` evidence and every published contract and gate.

Update only the supplied `0059-impl-plan.md`, `0059-impl-log.md` and
`0059-state-of-the-world.md` worker outputs. Preserve the historical recovery
record, replace its stale wait-for-commit handoff with the authorized relaunch
status, and include all mandatory state-of-the-world headings. No product or
test edits are indicated by the review. No new implementation belongs to 0059.

The normal review, gate and checkpoint cycle remains mandatory. Under the
RETIRE_CONTRACT worker's Invariant 6, the supervisor performs that cycle after
this worker exits; the worker neither runs gates nor writes checkpoints. The
existing recovery measurements (794 constructor cases, 10 selected UI cases,
all 13 unchanged gates passed) are historical evidence, not fresh results of
this relaunch. Return the structured handoff with no worker-run gates and no
claim of acceptance. The supervisor must record its measured 794 and complete
full-suite case evidence at checkpoint index 58 before entering 0060, retaining
checkpoint 57 unchanged and never using `--accept-current-slice`.

Risks: a review based on HEAD alone would miss the already-committed product
retirement; counting methods instead of rows recreates the old 797 floor; the
10 selected UI cases must not become a whole-suite baseline. Leave the
supervisor's initially dirty `.executability-json`, `.manifest.state.json`
and `.supervisor-lock.diag` untouched. ASG and STL remain read-only.

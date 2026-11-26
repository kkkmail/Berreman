# Executed-case accounting for authorized test retirements

Operator approval, 2026-11-06: complete repair steps 1–4, verify, then wait for
the operator to commit. Keep the arc stopped. ASG changes are separately
authorized for this repair; the worker's standing ASG read-only rule remains.

## Ruling

Test-effect identities denote methods; the MTP succeeded counter counts executed
cases. Credit a retired method with the cases measured for that method in the
preceding accepted whole-suite checkpoint. A theory's rows are distinct cases.
Never infer a credit from the observed decrease, pad the remaining suite, waive
a floor or rewrite a numeric checkpoint. Missing evidence requires validated
recovery of the archived full report.

- Step 0059 removes four methods: one four-row theory and three facts. Its
  authorized decrease is seven cases: 801 becomes 794, as S21-RETIRE-EF-PROOF
  already requires. Preserve the implementation committed in `a6fb67f`.
- Step 0151 preserves the rows of the migrated SceneName methods in
  SceneRoundTripTests, including both blank-name theory rows. A move must not
  collapse rows into a single fact. Other declared effects remain binding.
- Step 0152 removes five ExperimentCollectionProxyTests methods, comprising
  six executed constructor cases. The prose saying five fewer confuses methods
  with cases; six is the correct constructor decrease. UI effects are unchanged.
- Step 0260 removes three SceneName methods comprising four cases, and adds
  three SetupModelTests facts: the resulting constructor decrease is one, not
  the unchanged count stated in the prose. Preserve every declared identity,
  migrated row, replacement assertion and all other effects.

These arithmetic corrections amend the prose through this operator ruling.
The published spec, oracle, test-effect ledger, descriptors and gates remain
unchanged. Runtime measurements, not these example totals, establish future
case evidence; an unexpected census or case loss still fails verification.

## Recovery and verification

Recover checkpoint 57's 801-case evidence from the successful archived 0057
whole-suite TRX, its gate event and stdout in `.artifacts/0056-operator-repair/`.
Validate the report against both 0057 and 0058 and their identical gate contract,
the accepted checkpoint count/commit, and the absence of intervening test
identity effects. Record the archive paths, hashes and timestamp. Verify the
retired source did not change between the archived run and accepted 0058.
Store the recovered evidence in `.test-case-counts-json`; retain checkpoint 57
at 801. Do not manufacture checkpoint 58 or alter supervisor state.

Refresh the profile-1 receipt for the corrected ASG validator and the project
prompt's ruling pointer, preserving all 494 compiled steps and gate snapshots.
Run all 13 unchanged 0059 gates, its absence checks and exact observed census.
Require 794 constructor passes, zero failures/skips, the selected UI tests and
all nine invariants. Keep verification evidence in
`.artifacts/0059-operator-repair/` and update the slice handoff.

## Relaunch after the operator commits

For this recovery only, step 0059 is an explicit exception to the standing
committed-green acceptance rule: resume it normally with an operator note and
`--executability-profile 1`, **without `--accept-current-slice`**. The normal
gate/checkpoint cycle must record the measured 794 at checkpoint index 58 and
persist its full-suite case evidence before step 0060. Tell the worker the
committed retirement already exists and direct it to this ruling and the fresh
verification. Do not repeat or restore the retired implementation.

Do not relaunch, accept or commit as part of repair steps 1–4. Wait for the
operator's commit and subsequent relaunch instruction.

## Verification completed

2026-11-06 14:42:38–14:44:03 UTC: all 13 unchanged gates passed. Constructor:
794 passed; selected AppContextTests/LauncherTests: 10 passed. No failures or
skips; exact censuses match and the fresh report loses precisely the seven
authorized rows. All absence searches passed. A supplemental elevation run
against accepted commit `a402443` checked the already-committed App changes:
zero new findings. Build reported only two existing vendored MathNet warnings.

ASG: 46 final focused/pipeline/runtime tests passed; strict mypy passed all
eight changed source files. The broader run passed 311 tests and found one
existing three-versus-four-digit slice-label assertion failure, reproduced
against committed ASG code. It is recorded in ASG 0090 `.manual/028`.

All 494 compiled steps/gate snapshots and published contracts are unchanged.
The current profile-1 receipt validates. Recovered checkpoint-57 case evidence
records 801; numeric checkpoint 57 remains 801 and checkpoint 58 is absent.
Supervisor state, expected-red records and committed product sources are
unchanged. No commit or restart was performed. Full evidence is under
`.artifacts/0059-operator-repair/`, including `verification-summary.json` and
`0059-elevation-accepted-base.log`.

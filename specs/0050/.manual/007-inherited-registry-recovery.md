# Recovery of inherited contract lifecycle publication at 0069

Operator approval, 2026-11-06: implement the four-step plan in
`.slices/0069-01.postmortem-review-md`, verify and document it, then wait for the
operator's commit. Do not relaunch the arc as part of this repair. ASG changes
are separately authorized; the worker's standing ASG read-only boundary remains.

## Ruling

An inherited contract has a global identity and a separate current-arc
lifecycle. ASG publication must prepare the latter for planned inherited
IMPLEMENT_CONTRACT/MODIFY_CONTRACT work before binding the executability
receipt. Implementation then changes progress in an existing row. Preserve
the global identity, origin registries and all existing local metadata and
progress. Do not loosen receipt validation to allow arbitrary identity changes.

At 0069, the first MaterialProxy lifecycle row was required by the worker
protocol but invalidated the published receipt. Preserve that implemented row
and the product implementation committed by the operator in `308fead`.
Initialize only the remaining validated inherited entries for 0071 CategoryProxy,
0074 SampleProxy and 0095 CatalogueImportProxy. The published ledger, oracle,
specifications, slice descriptors and gate snapshots remain unchanged.

The supervisor's reliability stop must emit the terminal escalation event after
postmortem reporting. The watcher must recognize the matching persisted
escalation when an older stop omits that final event. This repairs notification;
it does not authorize automatic repair, acceptance or restart.

## Acceptance and resumption

Run all 15 unchanged 0069 gates. Require 835 constructor cases against the
accepted 811 baseline, 208 UI-smoke cases, the selected MaterialsWindowTests,
and all ten s09 invariants, with no failures or skips. Record fresh evidence
under `.artifacts/0069-operator-repair/`. Since the product changes are already
committed, separately check elevation against accepted 0068 (`38c6d3c`) so the
recovery's documentation/registry diff cannot hide the product scope.

Keep checkpoint 67 at 811 and leave checkpoint 68 absent. Preserve supervisor
state, expected-red records and full-suite case evidence. After the operator's
commit and explicit relaunch instruction, resume 0069 normally with an operator
note and **`--executability-profile 1`**, **without `--accept-current-slice`**.
This is an explicit exception to the standing committed-green acceptance rule:
normal review, all gates and the measured checkpoint at index 68 are still
required before advancing. Do not repeat or restore the existing implementation.

ASG implementation log: `C:/GitHub/AI-Strategy-Generator/specs/0090/.manual/030-inherited-registry-and-escalation-events.md`.

## Verification completed

2026-11-06 20:54:46–20:57:12 UTC: publication and all 15 unchanged 0069 gates
passed. Constructor: 835; UI smoke: 208; selected MaterialsWindowTests: 35.
All exact censuses match, with zero failures/skips. Build has zero warnings
from our code; two existing SYSLIB0051 warnings are in vendored MathNet.

The receipt validates. All 988 compiled artifacts are byte-identical, all
existing current-arc registry entries are preserved, and exactly the three
remaining inherited rows were added. Product sources, origin registries,
published requirements, supervisor state, checkpoints, expected-red records,
case evidence, Git HEAD and index are unchanged. No commit or restart occurred.

ASG: 454 regression tests passed; strict mypy passed all three changed Python
files. `verification-summary.json`, `contract-validation.json`, gate logs and
copied TRX reports are under `.artifacts/0069-operator-repair/`.

Supplemental elevation against accepted 0068 (`-Base 38c6d3c`) passed across
seven affected/dependent projects: zero new findings, 143 seconds. This covers
the committed product changes that the standard gate's working-tree scope did
not select. Only the mandated ELEV004 exclusion applied; known findings remain
empty. Evidence: `0069-elevation-accepted-base.log` in the same artifact folder.

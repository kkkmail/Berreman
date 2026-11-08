# Step 0150: warning and kit-picker coverage recovery

The operator approved the four-step plan in
`.slices/0150-01.postmortem-review-md` on 2026-11-08. Entry HEAD `7addce1`
contains the failed attempt; accepted 0149 (`aa5ff97`) remains the comparison
base for the complete slice. Preserve its implementation and six declared facts.

## Authorized repair

Alongside the original four 0150 files, authorize these two test files:

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitContentFileJsonTests.fs`:
  replace the eight ambiguous nested JSON-index expressions with explicit `.[]`
  indexing. Preserve the malformed values, assertions and test identities.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs`:
  extend the three existing picker facts with calls to `KitFolderPicker.ofWindow`
  using the existing isolated headless/storage fixture. Preserve every original
  library-picker assertion and use separate fixture instances for kit observations.

Cover resolved/unresolved suggestions, title and single selection, the first
local selection, empty cancellation, a non-local first selection without falling
through to later local entries, and provider acquisition/resolution/dialog
failures. Assert no folder creation. Observe results, options and captured calls
outside the production exception handler, so it cannot swallow assertion failures.
Correct any defect these tests measure within this behavior, without expanding
the public API. Add no test identity or census row. Manuals 008–010 remain binding.

An incremental zero-warning summary does not clear an earlier diagnostic. Its
source fix and actual recompilation must establish that it has been resolved.
This does not authorize warning suppression or a broader build-policy change.

## Verification and handoff

Run a Release solution rebuild (`dotnet build Berreman.slnx -c Release -t:Rebuild`
from `Berreman/`), focused JSON/picker tests, and all sixteen unchanged 0150 gates.
Require zero owned-code warnings, all 1,157 constructor cases, all 215 smoke
cases, the exact fourteen non-smoke AppContextTests identities, and zero
failures/skips. The selected UI count is not a whole-suite baseline. Include the
committed implementation and affected dependents in supplemental elevation
against accepted `aa5ff97`.

Validate all 494 steps and refresh the profile-1 receipt for the project-prompt
pointer while retaining all 988 compiled artifacts byte-for-byte. Preserve the
published spec, oracle, gate snapshots, test effects, supervisor state, counters,
checkpoints, baselines, expected-red records, contract registries and versions.

Keep evidence and the future operator reply in `.artifacts/0150-operator-repair/`.
Update the implementation log and state-of-world handoff, retaining inherited
YAML floors. Complete verification, then wait for the operator's commit. No ASG
runtime/source repair is required for this incident.

## Explicit recovery exception

For 0150 only, this overrides the standing committed-green acceptance rule.
After the operator commits and requests relaunch, resume normally with
`--operator-reply <recovery-note> --executability-profile 1`, without
`--accept-current-slice`. Normal supervisor review and all gates must publish
checkpoint index 149, measured case evidence and expected-red bookkeeping before
advancing. Do not fabricate those records, reset budgets or relaunch before that
instruction.

Later changes to the JSON test file preserve the corrected indexing (0217,
0221, 0260–0261, 0288, 0290–0291, 0313, 0359, 0382, 0384, 0395, 0404, 0460).
Later AppContextTests edits preserve the extended identities and fixture
isolation; future adapters require their own exercising coverage.

## Measured implementation

The approved repair changes exactly eight index expressions and extends the
three existing picker facts. Every original assertion and identity is retained;
the real kit adapter is exercised through the existing fixture with separate
provider/window instances. Its production implementation required no change.

The forced Release rebuild passes with no owned-code warnings and no FS3369.
It reports two SYSLIB0051 warnings in vendored MathNet `Numerics/Exceptions.cs`,
kept distinct from the repaired project. All nine focused JSON facts and three
extended picker facts pass. One preliminary test invocation used an unsupported
interior wildcard and ran zero tests; exact existing method names corrected the
invocation, and both logs are retained. This required no product change.

All sixteen unchanged gates pass. The regular gate build reports zero warnings
and errors. The exact census is 1,157 constructor cases, 215 smoke cases and
fourteen selected UI identities, with zero failures/skips. Supplemental elevation
against accepted 0149 passes all four affected product projects with zero
findings under the existing ELEV004 exclusion. All 494 steps validate, the
refreshed profile-1 receipt validates, and all 988 compiled artifacts are
byte-identical. Evidence is in `.artifacts/0150-operator-repair/final-review.md`.

The implementation and state-of-world reports now describe this verified repair.
Inherited YAML floors remain 1,151 constructor and 215 smoke cases. Checkpoint
149 and supervisor acceptance remain pending; no state, count, Git, version or
ASG source mutation or arc relaunch was performed. The complete repair is left
for the operator's commit.

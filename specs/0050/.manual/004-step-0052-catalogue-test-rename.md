# Step 0052: authorize one catalogue test rename

Operator approval, 2026-11-06: implement the proposed repair and complete its
verification, then wait for the operator to commit. Do not restart the arc.

For spec 0050, authorize the single PropagationTests fact rename from
`the curated Library seeds exactly the named SeedSamples` to
`the curated Library contains exactly the catalogue samples`, effective at
step 52. Record it as a step-52 operator test-effect rename in the
`constructor-unit-tests` census.

This supersedes name preservation only for that fact in S17-REPOINT-CONSTRUCTOR.
Preserve its catalogue-backed body, assertions, numeric expectations and suite
count. Every other fact keeps its name. Keep all text-absence checks unchanged,
including the broader checks at steps 56 and 58. The published spec, oracle,
compiled descriptors and gates remain unchanged.

Refresh the profile-1 receipt after validating the census mapping. Run the
unchanged step-52 acceptance searches and all ten gates in `0052.gates`, requiring
805 passing constructor tests with the exact accepted identities. Record fresh
evidence under this arc's `.artifacts/` and update the handoff. Acceptance and
restart remain pending the operator's commit and instruction.

## Verification completed

2026-11-06 10:35:55 UTC: all ten unchanged gates passed through ASG's evaluator.
Constructor tests: 805 passed, zero failed/skipped, exact accepted census and
renamed fact confirmed. Both acceptance searches found nothing. The compiler
validated the rename through step 494 and refreshed the receipt while preserving
all 494 compiled descriptors and gate snapshots byte-for-byte.

Build: zero errors; two existing SYSLIB0051 warnings in vendored MathNet's
Exceptions.cs, no warnings from our code. Elevation reported no in-scope product
project touched; this slice and repair change only tests.

Evidence: `.artifacts/0052-operator-repair/gate-results.json`,
`test-inventory.json`, `constructor-unit-tests.trx`, `acceptance-searches.json`,
`contract-validation.json`, and the complete `verification.log` in that folder.
Arc state, checkpoints and expected-red records are unchanged. No commit or
restart was performed.

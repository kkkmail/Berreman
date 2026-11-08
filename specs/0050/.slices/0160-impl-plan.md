# Implementation plan — slice 0160

## Resume after the ASG live-input repair — 2026-11-08

Entry HEAD is `7392e82` (`0050-0160-manual`). Compared with the verified repair
at `37cb549`, it only commits the operator's `.specrc` backend selection
(`openai-codex` → `claude-code`), postmortem 03 with its review, and the previous
handoff files. Product, schema, test, elevation-driver and gate inputs are
unchanged. Per the task's operator note, ASG 0090 manual 033 fixes the false
stale-receipt and source-mutation guards. Valid `.specrc` edits and any
`.manual/` edits made during the arc are allowed, so they are neither a defect
nor a scope violation. Keep the selected backend and every concurrent operator
edit as they are.

Under [manual 012](../.manual/012-step-0160-material-schema-recovery.md) and
[manual 013](../.manual/013-arc-origin-elevation-recovery.md), keep the committed
material-schema repair, the codec/schema/aggregate retirement, all assertions
and the declared test effects: one migration, seven deletions and one addition.
No new product work is indicated. The work is source inspection only:

1. Confirm the product diff against accepted 0159 (`8d82da6`) and the empty
   retirement search.
2. Confirm the effective material schema, the `tryLoadSchema` / `validateWith`
   seams, the declared numeric-category fact and the byte-identical migrated
   table fact.
3. Confirm that `.arc-base-json` records origin
   `f395597c071910aaa215518626563f6c784bfb62`.

Then refresh only the three supplied handoff paths.

## Verification and handoff

Invariant 6 reserves every check and gate for the supervisor. This round runs
no build, test, analyzer, invariant or validation command and returns
`gates_run: []` / `gate_results: {}`. The operator's earlier evidence (fifteen
gates, twelve analyzed projects, 1,136 constructor cases with exact census) is
not supervisor acceptance.

Normal review and all fifteen frozen gates must publish measured checkpoint 159
and case evidence before step 0161. The YAML keeps the inherited 1,142 floor.
The run keeps `--executability-profile 1`. This round does not use
`--accept-current-slice`, does not change frozen contracts, does not reset
budgets and does not edit STL, ASG or git.

# Resume 0160 after ASG live-input repair

Read manuals 012 and 013. The operator ruled that agent switches and other valid
`.specrc` edits, and all edits under `.manual/` at any depth, are allowed during
a running arc. ASG 0090 manual 033 fixes the false stale-receipt and source-mutation
guards. This supersedes the stop-before-switch advice in postmortem/review 0160-03.
Preserve the current operator-selected backend and any concurrent operator edits.
Do not classify the mere presence of those edits as a worker defect or scope
violation. Report an actual invalid configuration or conflicting requirement
only when supported by concrete evidence.

Preserve the committed material-schema repair, all assertions and declared test
effects. Elevation uses the immutable arc origin
f395597c071910aaa215518626563f6c784bfb62 through ASG_ARC_BASE_COMMIT. Commits do not
narrow coverage. Previous operator evidence (all fifteen gates, twelve analyzed
projects, 1,136 constructor cases with exact census) is not supervisor acceptance.
Return for normal review and verification, then publish measured checkpoint 159
and case evidence before 0161. Retain the inherited 1,142 floor and declared
seven deletions/one addition. No new product work is indicated by this tooling
incident. Do not change frozen contracts, reset budgets, edit STL, or use
--accept-current-slice. The launch retains --executability-profile 1.

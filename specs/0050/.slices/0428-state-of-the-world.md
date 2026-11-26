# Where we are

Slice 0428 is resumed after the operator's commits 926d4cbd (implementation
and R0003 identity-provider repair) and 0931367d (regenerated reference kit).
R0003 resolves the earlier effects conflict. Source review found every
implementation obligation present and no further product change needed.
This worker updated only the three canonical handoff documents. Normal
supervisor review and the unchanged 26-gate roster must publish measured
checkpoint 427 before advancing; this worker does not publish a checkpoint.

# What's working

- Preserved the shared element-identity provider and fresh production GUIDs.
- Preserved AT-1's three per-build fixed identities and consumption assertions.
- Preserved exact-file comparison, the regenerated reference and unchanged lab data.
- Recorded the resolved recovery and the evidence for supervisor verification.

# Tests

No build, test, builder invocation or gate was run by this worker in the
resumed round. IMPLEMENT Invariant 6 reserves gate execution and verdicts
to the supervisor. All implementation work is present; verification is
pending the supervisor, with no unresolved implementation or authority issue.
The structured exit uses commit_ready=false because this worker does not
certify a gate result.

The following are historical operator measurements, read from
[gate-results.json](../.artifacts/0428-operator-repair/gate-results.json).
They corroborate the operator's recovery note and do not replace the normal
supervisor run.

| Gate | Operator result | Wall time | Current deadline |
|---|---|---|---|
| build | Passed | 79.7 s | 600 s |
| elevation | Passed, 0 new findings in 13 projects | 218.1 s | 600 s |
| ui-smoke | 264/264, 0 failures/skips | 240.0 s | 600 s |
| ui-tests | 18/18, 0 failures/skips | 28.4 s | 360 s |
| kit-acceptance | 4/4, 0 failures/skips | 46.5 s | 3600 s |
| All 21 invariant-s41-* gates | Passed | 0.2 s each | 300 s each |

No recorded operator run exceeded its effective deadline. No new worker
run adds a duration or result. The saved elevation log uses arc origin
f395597c071910aaa215518626563f6c784bfb62 with ELEV004 excluded. The saved
build reports NU1902 advisories and two SYSLIB0051 warnings in vendored
MathNet; no warning originates in owned code.

The supervisor must run 0428.gates as declared: build, arc-origin elevation,
all ui-smoke cases, ui-tests selecting StudyViewTests and DataStepTests,
kit-acceptance selecting KitAcceptanceTests (including the exact-file fact),
and all 21 s41 invariants. Preserve every selector, deadline, census and
count floor. Run normal profile-1 recovery without --accept-current-slice;
publish checkpoint 427 only from the supervisor's green measurements.

This baseline block is inherited unchanged from the preceding 0428 handoff;
it is not a new measurement. The supervisor's checkpoints and case evidence
remain authoritative. Operator run counts above do not replace these values.

```yaml
gates:
  constructor_unit_tests: 1761
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

R0003's ElementIdProvider is declared in TableAndElementRotationView.fs.
Its system member owns fresh GUID creation; initWith stores the provider
as elementIds, addElement consumes it, and mkElement uses the system member.
Both AddElement and AddElementBoundTo converge on that same add path.
The old freshId helper is deleted.

Each AT-1 build initializes its own remainingSlots list from three distinct
ElementId values before sending its existing messages. A fourth request
fails and the build asserts that no identity remains unused. The provider
changes construction inputs without changing any stored contract or
semantic message. R0001's series dispatch and R0002's analyzer law remain
unchanged.

The comparison reuses buildOpticsMuellerKit, relativeFiles and assertSameBytes.
It compares the ordered union of both recursive inventories, excludes only
root .oc/, and reports the built absolute folder, first differing relative
path and missing/extra/different-bytes reason. The committed kit is read-only;
failed comparison output is retained after closing its kit.

Operator manual 034 records regeneration of all 116 root/library files and
a passing comparison on a second independent build. Commit 0931367d contains
the resulting changes to 42 setups and 53 experiments; unchanged generated
files require no second diff. Lab data did not change. The four-line
AGENTS.md reference-kit maintenance bullet remains intact.

# Deferred

No implementation is deferred to another slice. The supervisor's normal
review, gate execution and checkpoint publication are the remaining lifecycle
steps for 0428. AT-2 work belongs to its already-planned 0432 slice.

# Gotchas

- R0003 supersedes the previous attempt's authority blocker. The earlier
  failed-attempt log and post-mortem remain historical evidence.
- At 0432, inject this same provider for AT-2's added reflected detector;
  preserve both fixtures' injection through 0494.
- Do not regenerate or hand-edit the committed kit merely to repeat completed
  operator work. Future changes to its output require the existing comparison
  and byte-preserving regeneration in the same step.
- Preserve commits 926d4cbd and 0931367d, published contracts, test identities,
  gate descriptors, consumed counters, arc origin and supervisor metadata.
- .manifest.state.json and .supervisor-lock.diag were dirty on entry and were
  left untouched. No Git mutation, external-repository edit, version bump,
  notebook edit, production edit or reference-data write occurred in this round.
- The active supervisor owns recovery. This worker launches no nested
  supervisor and writes no synthetic checkpoint.

# Changelog

- 2026-11-15, original attempt 01: Added the comparison, generated the reference and documented regeneration ownership.
- 2026-11-15, original attempt 02: Diagnosed the random slot identities and reported the missing production edit authority.
- 2026-11-15, operator recovery: Authorized and committed the shared provider, deterministic fixture and regenerated reference.
- 2026-11-15, resumed attempt 01: Reviewed committed recovery and saved operator evidence; replaced the stale blocked handoff for normal supervisor verification.

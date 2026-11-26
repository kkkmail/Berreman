# Where we are

Step 0430 implements S12.1's AT-1 exchange path following the built-kit comparison
and committed-kit read-only path. The one authorized SendKit/OpenKitZip acceptance
fact is implemented in KitAcceptanceTests.fs. Supervisor verification is pending.

# What's working

- Added AT-1 coverage through SendKit and OpenKitZip with real archive proxies.
- Replayed recorded progress and completion messages through both update functions.
- Asserted successful archive counts and exclusion of every `.oc` zip entry.
- Compared every reopened kit, library and data file by relative path and bytes.
- Compared nonempty QZ-LR start-point residual vectors element for element.
- Reused the existing kit builder, window composition and file-comparison helpers.

# Tests

No build, test, builder invocation or gate was run. IMPLEMENT Invariant 6 assigns
all gate execution and verdicts to the supervisor after worker exit. The final
JSON reports empty gate results and commit_ready=false because verification is
pending, not because implementation or authority is blocked.

Run the unchanged 0430 roster: build; elevation from the immutable arc origin;
all ui-smoke cases; ui-tests selecting StudyViewTests and DataStepTests;
kit-acceptance selecting KitAcceptanceTests; and all twenty-one s41 invariants.
Exactly one authorized fact was added. No existing identity or assertion was
removed. The expected acceptance count increases from five to six; this is not
a measured test result. No development duration or deadline exceedance exists.

The baseline block below is inherited unchanged from 0429. The supervisor's
checkpoints and case evidence remain authoritative; no baseline, floor or
checkpoint was authored or changed by this worker.

```yaml
gates:
  constructor_unit_tests: 1761
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

The existing buildOpticsMuellerKit creates a fresh kit through semantic messages,
retaining its deterministic identities and clock. The new fact installs a real
KitArchiveProxy and an immediate KitArchiveJobHost in that model's kitShell.
It sends the kit to optics-mueller.zip in the builder's run directory, outside
the kit, records SendKitProgressed/SendKitEnded, and replays them through the
window update. It requires a finished archive, one progress message per file,
the complete file count, the written zip and an idle send job.

A KitStartContext over the same app context and clock extracts into the fresh
empty received/optics-mueller directory under that run. Its immediate host records
OpenKitZipProgressed/OpenKitZipEnded. Replaying those messages must open the target
through showKit, leave no error or unresolved offer, and return the archive job to
idle. The zip inventory independently rejects every KitAppCachePath. Both folders'
file sets use the private kitContentFiles helper moved from the existing committed
comparison fact; assertSameBytes compares every file outside `.oc/`.

The reopened window uses TestAppScope.inverseModelOver. For both window models,
QZ-LR's parameterLayoutWith supplies startVector; prefitWith, dataWith and
residualWith compose over that model's studyForwardContext. Each pipeline must
answer Ok, each residual must be nonempty, and the arrays must compare exactly.
No optimizer is substituted for the residual and no tolerance is introduced.
Nested finalizers close both kits before deleting their builder-owned run folder.

# Deferred

No implementation requirement is deferred. Supervisor review, gate execution
and measured checkpoint publication remain as the prescribed lifecycle work.

# Gotchas

- Preserve R0001's independent-series dispatch, R0002's analyzer traversal and
  R0003's deterministic element identities; their code is unchanged.
- Replaying the completion messages is essential: the immediate host records
  callbacks while the initial update is still constructing its running model.
- The extraction target keeps the optics-mueller leaf so its named kit file
  resolves normally. `.oc` may be recreated by opening, so inspect the zip itself
  for exclusion and omit `.oc` from the folder comparison.
- Product code, durable formats, committed data, versions, gates, census and
  external repositories were not edited. No Git mutation was performed.
- `.manifest.state.json` was already dirty and remains supervisor-owned.
- Source diff inventories with and without CR-at-EOL ignoring agree; no
  line-ending churn was introduced. No run artifacts were produced.

# Changelog

- 2026-11-15: Implemented AT-1 archive round-trip and exact start-residual coverage, and saved the supervisor handoff.

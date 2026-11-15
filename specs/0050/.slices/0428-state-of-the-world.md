# Where we are

Slice 0428, attempt 02, is blocked on a concrete effects conflict for
S121-COMMITTED-KIT. The previous attempt added the exact-byte comparison,
generated the reference kit and added the AGENTS.md maintenance bullet.
Supervisor verification exposed random slot identities in that output.
The required element-ID provider belongs in a production file outside
0428's authorized effects. Implementation is incomplete; commit_ready=false.

# What's working

- Preserved the complete reference-kit comparison and all earlier acceptance assertions.
- Traced all 95 differing files to the three slot IDs minted by AddElement.
- Preserved generated reference files, lab data, series dispatch and analyzer traversal.
- Recorded the missing production edit authority and the minimum conforming repair.

# Tests

No gate, build, test or builder invocation was run by this worker in attempt
02. IMPLEMENT Invariant 6 reserves gate execution and verdicts to the
supervisor. Diagnosis read its retained failure output and saved console.

Historical supervisor measurements from
../.artifacts/0428-01-console-20261115T183705Z.log:

| Gate | Supervisor result | Duration | Deadline |
|---|---|---|---|
| build | Passed, twice | 83.5 s / 3.9 s | 600 s |
| elevation | Passed | 229.6 s | 600 s |
| ui-smoke | Passed | 230.2 s | 600 s |
| ui-tests | Passed, selected classes | 28.7 s | 360 s |
| kit-acceptance | Failed; one residual identity after retries | 105.4 s including retries | 3600 s per attempt |

The final filtered kit-acceptance run reports one failure, zero passes and
zero skips in 18.561 s. The other 21 invariant gates were not reached after
the failing gate. No measured deadline was exceeded, and no worker run adds
a new result. Current acceptance remains red; this is not a pending-only
verification handoff.

The unresolved identity is
`OpticalConstructor.Ui.Tests.KitAcceptanceTests.the built optics-mueller kit equals the committed kit file by file`.
The test reports `BullshitCheck-2.experiment-json` at byte 596, where
`experiment.scan.blocks[0].axes[0].elementId` differs. Both folder inventories
contain 141 files excluding .oc/. All 53 experiment files and 42 setup files
differ; every differing JSON scalar is one of three generated slot IDs.
All remaining bytes, including the two fit runs and all lab data, agree.

The following baseline block is inherited unchanged from attempt 01 and
0427's handoff. The supervisor owns measured baselines and checkpoints.

```yaml
gates:
  constructor_unit_tests: 1761
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

`KitAcceptanceTests.buildAcceptanceSetups` sends three `AddElement` messages.
`TableAndElementRotationView.fs:3094` dispatches them through
`AddElementBoundTo` to `addElement`, whose slot record at line 1722 assigns
`freshId ()`. That function at line 813 calls `Guid.NewGuid()` directly.
The model injects item IDs but offers no element-ID provider or semantic
message accepting a slot ID. The new slot IDs persist in setups and scans.

The minimum proposed repair is to add an `ElementIdProvider` record and
model field in
`Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`,
default it to normal GUID generation, and have `addElement` obtain its ID
through the provider. The already-authorized KitAcceptanceTests fixture can
then supply three stable elevated identities before sending AddElement,
analogous to its existing ItemIdProvider. Generate and copy the reference
again through the real builder, preserving byte comparison, all assertions
and the existing test identities. No durable format change is needed.

**Authority conflict, owner: supervisor/operator, step 0428.**
S121-COMMITTED-KIT requires exact file bytes and a build through semantic
messages. .manual/001 §Q explicitly says the kit is built "ONLY through the
semantic messages"; .spec-md S12 and the immutable oracle retain that rule.
0428's effects allow KitAcceptanceTests.fs, the narrow AGENTS.md bullet and
enumerated kit files, but exclude the UI production file above. R0001/R0002
authorize additional production changes only at 0427 and expressly do not
carry that edit authority into 0428. Additional authority for that UI file
is required to complete the conforming repair. No new/renamed/deleted test
identity or relaxed acceptance is requested.

Changing fixture records after AddElement would rewrite persisted identities
outside the semantic messages. Normalizing IDs in the comparison would
weaken the byte oracle. Copying another random build into the reference
would leave the next run's mismatch intact. None of those changes was made.

# Deferred

Nothing is delegated to a later slice. This slice is blocked until the
operator resolves the explicit edit-scope conflict. The element-ID injection,
reference regeneration and supervisor verification are unfinished work for
0428, not accepted deferrals.

# Gotchas

- Evidence: ../.artifacts/0428-attempt-02-byte-diagnosis.json contains all
  95 differing paths, field/value pairs, file hashes and extracted historical
  supervisor gate-result records. It is diagnostic evidence, not a gate run.
- Retained failure source:
  `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/bin/x64/Release/net10.0/KitAcceptanceTests/3c8cf9f1c7ac4dfb983e9f3280c05e24/optics-mueller/`.
  Its analyzer ID is `26782f04-7c0e-4fe2-a3e5-02f4d69f6a42`; the reference
  holds `9ae0725e-7431-4092-9544-b5d347716ff8`.
- Attempt 01's generation provenance remains under
  ../.artifacts/0428-kit-generation/. Its claim that implementation was
  complete is superseded by the measured repeatability defect above.
- No source, reference file, test, gate, oracle, contract, census, checkpoint,
  version, .manual file or supervisor state was edited in attempt 02.
  Existing .expected-red-observed-json and .manifest.state.json edits were
  preserved. No Git mutation, external-repository edit or notebook edit occurred.
- Do not use the supervisor diagnostic's generic allowlist suggestion:
  the requirement remains exact-byte equality with no failure waiver.

# Changelog

- 2026-11-15, attempt 01: Added the comparison, generated 116 reference files and documented regeneration ownership; handed verification to the supervisor.
- 2026-11-15, attempt 02: Diagnosed 95 byte mismatches as three uninjectable random slot IDs; preserved acceptance and reported the required UI edit authority. Slice remains blocked.

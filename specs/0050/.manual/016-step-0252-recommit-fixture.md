# Step 0252: valid copied-item fixture and ordered evaluation reuse

2026-11-10. The operator approved the four preparation steps in
`.slices/0252-01.postmortem-review-md`, and additionally authorized implementing
both ASG warning defects before handoff. The operator already committed the
0252 worker changes and postmortems as `73c2b10`. No isolated commit or dirty
implementation is required. Wait for the next commits and explicit relaunch.

## Authorized changes

Extend step 0252's edit authority to the existing fact
`InverseConstructorTests.recommitting a copied experiment preserves its assigned physical ids`
in `OpticalConstructor.Ui.Tests/InverseConstructorTests.fs`. Its three assigned
IDs must identify stored kit items, as the real copy path requires since 0227.
Give the initially unbound inverse sample a valid plate binding, then record two
linear-polarizer preset items and the plate's stack-sample item using the same
three IDs, distinct labels, `ItemDraft.toItem` and `KitLibraryContext.recordItem`
before writing the copied experiment. Preserve every assertion and the identity.
The later glass-film rebind still clears the sample's physical assignment.

Also authorize exposing the existing ordered Result collector in
`OpticalConstructor.Domain/ItemEvaluation.fs` and replacing the duplicated local
collector in `OpticalConstructor.Ui/TableAndElementRotationView.fs` with calls to
it. Preserve item order, the first error and the rest of the run-law implementation.

At 0254, 0259, 0260 and 0261 retain the fixture's valid stored items and matching
bindings. When 0261 moves assignments to `SlotContent.ItemInSlot`, retain all
three stored items and the existing preservation/clearing assertions. This is
authority for these concrete repairs, not permission to waive tests or rewrite
the published ledger, census, acceptance, gate selection, floors or budgets.
No test is added, renamed or removed by the recovery; no operator census row is
needed. Any additional unrelated failing fixture requires its own authority.

## Verification and resume

Release-build, verify the repaired class, and run the whole non-smoke UI suite
once to find inherited failures before handoff. Run all 19 frozen gates for
0252, requiring 1401 constructor cases, 245 smoke cases and 85 selected UI cases,
zero failures/skips, exact accepted identities, all fourteen invariants and
elevation across all 12 projects from the immutable arc origin. Validate all
494 steps and refresh the profile-1 receipt through supported publication while
preserving all 988 compiled artifacts. Record measured evidence below and under
`.artifacts/0252-operator-repair/`.

Wait for the operator's BRM and ASG commits. This recovery is an explicit
exception to the general committed-green acceptance rule: resume normally with
the operator reply and `--executability-profile 1`, without
`--accept-current-slice`. The supervisor must publish measured checkpoint 251
before 0253. Do not manufacture that checkpoint or reset consumed counters.
Reattach the monitor. STL remains untouched; ASG's implementation is documented
in its spec 0090 manual 035. Do not launch the arc during preparation.

## Measured preparation

Both repairs are complete. Release build succeeded with zero warnings/errors.
All 19 frozen gate evaluations passed: constructor 1401, smoke 245, selected UI
85, each with zero failures/skips and exact accepted test identities; all fourteen
invariants passed. Elevation covered all 12 projects from `f395597` and found zero
issues. The additional whole non-smoke UI run passed 674 tests with zero skips or
failures. No test identity, assertion, census, gate, floor or allowance changed.

Evidence: `.artifacts/0252-operator-repair/gate-results.json`, corresponding
stdout/stderr logs and TRX files, `whole-ui-results.json` and
`verification-summary.json`. The approved project prompt is republished through
the supported compiler: 494 steps validated and all 988 compiled files remain
byte-identical. The receipt is refreshed for the accompanying ASG prompt changes.
Git HEAD/index, arc progress, all existing checkpoints and acceptance files are
unchanged; checkpoint 251 remains absent. ASG manual 035 records both tool fixes.

The handoff is ready for commits. Do not launch before the operator commits.

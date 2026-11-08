# Step 0145: fallible scratch-kit construction

The operator approved the four-step plan in
`.slices/0145-01.postmortem-review-md`. Entry HEAD `d015bc2` contains the partial
0145 implementation and reports; accepted 0144 remains `85aa190`. Preserve
that implementation and all seven authored identities. Keep the arc stopped
until the operator commits this repair and explicitly instructs a relaunch.

## Construction authority

This ruling overrides 0145's Domain-only location and infallible return contract
for `KitContext.createInMemory`. Keep `KitContext` in Domain and implement its
extension in `OpticalConstructor.Storage/KitFolderStore.fs`, authorized alongside
the four source files originally named by 0145. Its signature is:

```fsharp
static member createInMemory
    (kitFile : KitFile)
    (clock : ClockProvider)
    : Result<KitContext, KitError>
```

Create one fresh `InMemoryKitFolder`, obtain its first proxy using the existing
Storage `KitProxy.createInMemory`, call `initialiseKit kitFile`, and map success
to a context with that proxy, metadata, `ReadWriteKitAccess` and clock. Propagate
initialization errors unchanged. Do not call `openKit`, acquire a lock or call
`recordStamps`; preserve initialization's own-write bookkeeping. The clock is
captured, not consulted during construction. Reuse canonical metadata, lock and
snapshot serialization; no new proxy field, serializer, durable shape or fallback.

Add the already-declared in-memory-only fact
`OpticalConstructor.Tests.KitProxyTests.an in-memory kit context holds its kit file and writes through its own folder`.
Require successful construction, the prescribed metadata and write/read behavior,
canonical metadata reads, isolated contexts and absence of a created lock through
the existing observable seam. Preserve the seven existing identities and every
existing assertion. No census amendment is needed. Preserve manual 009's guard
contract without changing its implementation.

## Forward construction contract

Every role reads this ruling when creating or consuming scratch-kit or scene
initializers. The accepted 0128 factory relocation and typed serialization errors
remain authoritative. The following overrides reconcile future requirements now;
implement each future feature at its declared step, not early in 0145.

- **0150:** `KitOpening.scratchKit () : Result<KitContext, KitError>` uses the
  Storage extension. Preserve the scratch test identity, metadata defaults and
  fresh ID; tests require successful construction through their fixture.
- **0152:** override the instruction that scene entry points keep their
  signatures. The shared pure scene initializer receives an initialized
  `KitContext`; scratch entry points propagate construction errors. Amend the
  App, TestWindows, constructor-test and UI-test callers affected by those
  signature changes in the same step. App and diagnostic composition report the
  error before opening a constructor. Never throw away a construction error,
  silently use a dummy kit or keep an unsafe infallible facade.
- **0153–0154 and later:** initialize an already-opened scene from its supplied
  context, without first constructing a scratch kit that will be discarded.
  Keep ownership/error propagation coherent when loading scenes and opening
  picked-folder windows. Tests use shared success fixtures and preserve their
  declared identities/assertions, subject to their own authorized test effects.
- **0267/0299/0420:** retain `KitFile.create`'s added defaults and the existing
  scratch-kit ID mint. Returning a Result does not change those obligations.

Direct scratch references: 0145, 0150, 0152, 0267, 0299 and 0420. Checked explicit
initializer consumers: 0148, 0153, 0154, 0184–0189, 0201–0202, 0223, 0225,
0238–0243, 0252, 0259–0260, 0265–0266, 0273, 0279–0280, 0282–0283, 0310,
0312, 0315, 0319, 0323–0324, 0328, 0410, 0412, 0414, 0421, 0432, 0449,
0488 and 0490. 0148 precedes the initializer change and keeps current behavior.
All later indirect callers inherit this contract too; the inventory is not an
exemption for an unlisted caller. This authorizes necessary construction/caller
changes only, with no migration, new fallback path or unrelated feature expansion.

## Verification and resume

Run focused relevant tests and all fourteen unchanged 0145 gates. Require 1,137
constructor cases (1,129 plus all eight declared additions), exact profile-1
census, zero failures/skips and zero warnings from owned code. Include Domain and
all affected dependents in elevation: accepted `85aa190` is the comparison base
even though the partial attempt was committed separately. Do not narrow gates.
Validate all 494 steps and refresh the profile-1 receipt for the project-prompt
pointer while retaining all 988 compiled files byte-for-byte. Preserve published
contracts, oracle, supervisor state, counters, checkpoints, expected-red records,
baselines, operator test effects and contract registries.

Evidence and the future operator reply belong in `.artifacts/0145-operator-repair/`.
Update the implementation log and state-of-world with measured results, retain
the inherited YAML baseline of 1,129, and wait for the operator to commit.

This is an explicit exception to committed-green `--accept-current-slice`.
After the operator's commit and relaunch instruction, resume 0145 normally using
`--operator-reply <recovery-note> --executability-profile 1`. Preserve the verified
implementation for normal review and full supervisor verification. Only that
successful run publishes checkpoint index 144 and the measured 1,137-case floor
and completes expected-red bookkeeping. Do not accept directly or fabricate
state/checkpoints. ASG source requires no change for this incident.

## Measured implementation

The approved repair adds the fifteen-line Storage extension and the declared
in-memory-only fact. The committed Domain behavior, seven other 0145 identities,
all prior assertions and manual 009's guard implementation are preserved.
The constructor reuses the existing folder/proxy/serialization boundary and
propagates initialization failure through `Result.map` without an unwrap.

All 92 focused kit cases passed. All fourteen unchanged slice gates passed;
the gate build has zero warnings/errors, and all 1,137 constructor cases passed
with exact census and zero failures/skips. All 494 steps validated and all 988
compiled files remain byte-identical; the refreshed profile-1 receipt validates.
Additional elevation against accepted 0144 passed all seven affected projects
with zero new findings. Results, source review and final scope evidence are in
`.artifacts/0145-operator-repair/final-review.md`.

No checkpoint or baseline was published by the repair. No commit or relaunch was
performed. Future UI consumers are governed by this ruling, not claimed as
implemented or runtime-verified here.

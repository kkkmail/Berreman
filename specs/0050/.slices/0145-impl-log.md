# Implementation log

## Operator-authorized recovery

The operator approved the four-step plan in `0145-01.postmortem-review-md`.
Entry HEAD `d015bc2` contains the partial attempt and its reports; accepted
0144 remains `85aa190`. The historical authority conflict below is resolved
by `.manual/010-step-0145-scratch-kit-construction.md` and its project-prompt
pointer. Future scratch/scene consumers inherit that construction ruling.

- Added `KitContext.createInMemory` as a Storage extension returning
  `Result<KitContext, KitError>`. Its fifteen lines create one private folder
  and canonical proxy, initialize through that proxy, and map success to the
  read-write context. Errors pass through unchanged. It does not consult the
  clock, open the kit, acquire a lock or scan stamps. All pre-existing Storage
  code, including manual 009's guard repair, is unchanged.
- Added the missing in-memory-only fact under its exact declared identity.
  It checks metadata, access and clock capture, canonical serialized reads,
  absence of a lock, empty initial file/snapshot listings, write/read behavior,
  isolated contexts, metadata replacement and own-write stamp bookkeeping.
  Every prior assertion line and fact name/order is retained. The committed
  Domain implementation and seven existing 0145 identities are preserved.
- Recorded the forward signature/caller changes for 0150, 0152 and subsequent
  consumers without implementing their scheduled features early. No serializer,
  proxy field, durable format, retry loop or ASG source was added or changed.
- Initial full build succeeded with no owned-code warnings (two existing
  vendored MathNet SYSLIB0051 warnings). All 92 focused kit cases passed, with
  zero failures/skips. All 494 steps validate; all 988 compiled artifacts are
  byte-identical; the profile-1 receipt is refreshed and validates.

Full verification and final scope evidence are recorded in
`.artifacts/0145-operator-repair/final-review.md`; the operator-resume note is
beside it. These recovery measurements are independent of the historical
IMPLEMENT worker's gate-execution restriction. Preserve supervisor state,
checkpoints, counters, expected-red and baselines. Wait for the operator to
commit and instruct a relaunch. Normal resume must include
`--executability-profile 1` and the operator reply, without direct acceptance,
so the supervisor publishes checkpoint 144 with the measured constructor floor.

All fourteen unchanged slice gates subsequently passed: the gate build has zero
warnings/errors; the five-project working-tree elevation gate has no findings;
all 1,137 constructor cases passed with exact census and zero failures/skips;
all eleven s17 invariants passed. Supplemental elevation against accepted 0144
passed all seven affected projects with zero new findings. The final protected-file
check and complete evidence are documented in the final review cited above.

## Progress

- Read AGENTS.md, the task, both worker protocols, the project prompt, slice 0145, its resolved gates, naming conventions, manual 001 sections E/F, the S43-OPEN-SEAM oracle, and existing Domain/Storage/test implementations.
- The only initial working-tree change is the supervisor-owned specs/0050/.manifest.state.json. Leave it untouched.
- Before test authoring: add the folder-naming fact and three shared opening facts, giving seven of the eight authorized concrete identities. Cover successful initialization, preservation of an existing kit, foreign-lock refusal and read-only recovery. Exercise typed failures and existing lab-file preservation within those same identities.
- After test authoring: added those four fact declarations (seven concrete identities through the two subclasses). The shared facts also cover nonempty lab folders, the supplied clock instant, metadata-byte preservation, stamp recording, construction/initialization failures and read failures with no initialization. No existing assertions or identities were removed.
- Before Domain implementation: add only the three independent requested contracts. Normalize separators and trailing slashes without IO, treating a drive designator left after trimming as an empty leaf. Route all opening through the existing `openKit`; preserve every error except the specifically authorized `NotAKitFolderErr` initialization branch.
- After Domain implementation: added `KitFolder.namedKitFile`, reference-equality `KitFolderPicker` and `KitContext.openOrInitialise`, each documented with spec 0050 S4.3. Opening reuses existing locking, stamping and access-control behavior; no proxy field or persisted shape changed.
- Reviewed the source diff without executing gates. Changes are additions in exactly the four authorized source files; existing facts are intact. Numstats agree with and without ignoring CR at line ends. The shared tests remain on the abstract contract, so each runs under both concrete subclasses. Bound the optional lab-file path at its elevated origin.
- Confirmed the blocking dependency: Storage/KitFolderStore.fs supplies the sole canonical metadata serializer, lock serializer/parser and snapshot round-trip validator to Domain's folder factory. Storage already references Domain. Initializing through that factory returns a Result; the requested infallible constructor has no authorized error channel or injected serialization dependencies.
- Before handoff: write the required state-of-the-world with the precise unfinished contract, missing test identity and authority decision. Preserve the immutable census and baseline; do not claim the required eight-case increase or slice completion.
- After handoff preparation: wrote the state-of-the-world at the supplied path, including all required sections, inherited checkpoint 142, the missing eighth identity and the construction-boundary authority question. The structured exit reports no gates and commit_ready false because the slice is blocked.

## Files modified

- Created the supplied 0145 implementation plan and this log.
- Updated OpticalConstructor.Domain/Kit.fs and KitFolders.fs with the three independent contracts.
- Updated OpticalConstructor.Tests/KitTests.fs and KitProxyTests.fs with seven authorized concrete test identities.
- Created the supplied 0145-state-of-the-world.md.

## Testing state

No checks or gates run, per IMPLEMENT Invariant 6. The supervisor owns build, elevation, all-discovered constructor-unit-tests and the eleven s17 invariants. No pass or measured count is claimed.

Blocked, not commit-ready. S43-OPEN-SEAM is incomplete: `OpticalConstructor.Domain.KitFolders.KitContext.createInMemory` and `OpticalConstructor.Tests.KitProxyTests.an in-memory kit context holds its kit file and writes through its own folder` are absent. The test belongs to step 0145, path OpticalConstructor.Tests/KitProxyTests.fs, suite/discovery gate constructor-unit-tests; it is already authorized, but its production dependency cannot meet the prescribed boundary/signature. No placeholder or failing reference to a nonexistent symbol was introduced. Seven concrete test identities were authored rather than the required eight; this is source accounting only.

The most recent recorded constructor count is 1129 at checkpoint 142. The state-of-world repeats that inherited count and does not author a new baseline.

## Artifacts

- Worker documents use only the supplied specs/0050/.slices/0145-* paths.
- No persistent run artifacts created.

## Gotchas

- S43-OPEN-SEAM's Domain-only, infallible `KitContext.createInMemory : KitFile -> ClockProvider -> KitContext` conflicts with the current serialized in-memory proxy. `KitProxy.createInMemory` is declared in Storage/KitFolderStore.fs; `InMemoryKitFolder.createKitProxy` requires four Storage-supplied functions. The factory must preserve canonical metadata text and snapshot validation, not replace those dependencies with stubs. A Domain-to-Storage reference would cycle with Storage's existing Domain reference.
- The operator/supervisor must authorize a revised construction boundary or dependency-injection signature and its effects to resolve this conflict. No published spec, gate, census or oracle will be amended by this worker.
- Concrete authority decision for the operator: may step 0145 implement the constructor as a Storage extension using the existing factory and return `Result<KitContext, KitError>`, with Storage/KitFolderStore.fs added to authorized edits and the downstream S43-OPENING contract adjusted? Alternatively, specify the permitted lower-level serializer seam and its authorized move. The current slice explicitly fixes a Domain member, a two-argument infallible signature, four editable files, and no additional member/option/verb; none of those authority changes is inferred here.
- No git mutation, version bump, neighboring repository change, frozen-contract edit, checkpoint edit or analyzer suppression was performed. No requested work is scheduled for a later slice: this step is blocked on the explicit authority decision above.

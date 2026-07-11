# Step 020 — IMPLEMENT — impl-plan

## Slice

Spec 0038 Part H (AC-H1). Establish the entry-lifecycle foundation shared by the
material and sample stores: version numbering, the active/inactive state, a
cross-store versioned reference, the "which versions are in use" seam, and the
ONE pure version-creation decision function both stores will call (steps 21/22).
Pure Domain + pure tests only — no store wiring this round (that is steps 21/22
and 25).

## Approach

New module `OpticalConstructor.Domain/Lifecycle.fs` (`namespace
OpticalConstructor.Domain`, `module Lifecycle`), placed at the end of the Domain
compile order (depends only on `MaterialLibrary.MaterialId` and
`Library.SampleId`, both already compiled earlier). Contents:

- `VersionNumber` — single-case `int` DU with `.value`, `.next` (successor), and
  `static member first` (the value every entry starts at).
- `MaterialVersionId = { materialId : MaterialId; version : VersionNumber }` and
  `SampleVersionId = { sampleId : SampleId; version : VersionNumber }` — the exact
  keys a versioned experiment binding will pin (step 25).
- `EntryLifecycle = ActiveEntry | InactiveEntry` — named two-state DU (no naked
  bool) for live-vs-retired.
- `VersionRef = MaterialVersionRef of MaterialVersionId | SampleVersionRef of
  SampleVersionId` — the cross-store versioned reference.
- `VersionsInUse = { versionsInUse : unit -> Set<VersionRef> }` — the in-use seam
  (`[<ReferenceEquality>]`, function-valued field), plus `usageOf` mapping a
  `VersionRef` + the seam to a `VersionUsage`.
- `VersionUsage = VersionUsed | VersionUnused` — the elevated "is this version
  used?" flag (no naked bool in the decision signature).
- `VersionDecision = MutateInPlace | MintNextVersion | KeepCurrent`.
- `VersionPayload<'Physics,'Metadata> = { physics; metadata }` — the generic
  payload shape used for both the stored latest version and the incoming save, so
  ONE rule serves both stores (materials: `MaterialComplexity` physics + name/
  description/category metadata; samples: `SampleStructure` physics + name/
  description metadata).
- `decideVersioning` — the single pure rule. Takes injected structural-equality
  comparators (`physicsEqual`, `metadataEqual`) so it never forces an equality
  constraint on the caller's physics type and stays trivially testable; the real
  stores pass `(=)` over their value-tree physics. Decision table over
  (physicsChanged, metadataChanged, usage): identical → `KeepCurrent`; physics
  identical + metadata changed → `MutateInPlace` (metadata-only never versions);
  physics changed + unused → `MutateInPlace`; physics changed + used →
  `MintNextVersion`.

Tests: new `OpticalConstructor.Tests/LifecycleTests.fs` enumerating the full
used/unused × physics-changed/identical × metadata-only decision table plus the
`VersionNumber`/id/lifecycle/ref/`usageOf` surface.

## Files to modify

- `OpticalConstructor.Domain/Lifecycle.fs` (new).
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (register module).
- `OpticalConstructor.Tests/LifecycleTests.fs` (new).
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (register test).

## Risks

- `Set<VersionRef>` needs structural comparison — satisfied (Guid/int backed).
- Record-literal inference in tests — annotate literals with their record type to
  avoid `materialId`/`version` field-name overlaps (`SampleLayer` also has
  `materialId`).
- Exhaustive matches only (`--warnaserror+:25`).
- Interpretation: the stored input carries metadata as well as physics (needed to
  tell `KeepCurrent` from a metadata-only `MutateInPlace`) — noted in Gotchas.

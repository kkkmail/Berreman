# Step 020 — IMPLEMENT — impl-log

## Progress

- [x] Read system prompt (`implement_worker.system-md` + shared `arc-runner.system-md`),
      project prompt (`arc-runner.user-md`), slice spec (`020.slice-md`), and the
      surrounding Domain/Tests code (`MaterialLibrary`, `ElementId`/`Library`,
      `WorkbenchSettings`, proxy conventions).
- [x] Write `Lifecycle.fs` — versioning types + the shared decision rule.
- [x] Register `Lifecycle.fs` in the Domain fsproj (end of compile order).
- [x] Write `LifecycleTests.fs` — the decision table + supporting surface.
- [x] Register `LifecycleTests.fs` in the Tests fsproj.
- [x] **Attempt 02 (build-retry):** diagnose and fix the `build`-gate failure —
      `LifecycleTests.fs:98` `Assert.False(ActiveEntry = InactiveEntry)` (FS0505).
- [x] Finalize impl-log + state-of-the-world.

## Files modified

- **NEW** `Berreman/OpticalConstructor/OpticalConstructor.Domain/Lifecycle.fs` —
  `module Lifecycle` under `namespace OpticalConstructor.Domain`. Contains
  `VersionNumber` (`.value` / `.next` / `first`), `MaterialVersionId`,
  `SampleVersionId`, `EntryLifecycle`, `VersionRef`, `VersionsInUse`
  (`[<ReferenceEquality>]`), `VersionUsage` (+ `ofBool`), `usageOf`,
  `VersionDecision`, `VersionPayload<'Physics,'Metadata>`, and `decideVersioning`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
  — added `<Compile Include="Lifecycle.fs" />` (after `LibraryFacets.fs`) with a
  spec-referencing comment.
- **NEW** `Berreman/OpticalConstructor/OpticalConstructor.Tests/LifecycleTests.fs` —
  `module LifecycleTests`; 15 xUnit facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — added `<Compile Include="LifecycleTests.fs" />` (after `PolarizerBehaviorTests.fs`).

## Decisions

- **One rule, injected comparators.** `decideVersioning` takes
  `physicsEqual` / `metadataEqual` as parameters rather than imposing an
  `equality` constraint on `'Physics`. This keeps the single rule genuinely
  shared by both stores without forcing either physics type (`MaterialComplexity`,
  `SampleStructure`) to be an equality type at the Domain rule surface, and makes
  the rule trivially unit-testable with lightweight stand-in payloads. The real
  stores (steps 21/22) pass `(=)` — structural equality — over their value trees;
  "structurally different physics" is exactly `not (physicsEqual …)`.
- **`VersionPayload` carries both physics and metadata for the stored side too.**
  The slice text names the stored input "physics payload", but distinguishing
  `KeepCurrent` (identical save) from a metadata-only `MutateInPlace` requires
  comparing the stored metadata as well — so the stored payload is the full
  `{ physics; metadata }` record, symmetric with the incoming save. (See Gotchas.)
- **`VersionUsage` DU, not a bool.** The "whether used" input is a named two-case
  DU (no naked bool in the decision signature), with `ofBool` at its computation
  site and `usageOf` resolving it through the in-use seam.
- **`Lifecycle.fs` placed last in the Domain compile order.** It depends only on
  `MaterialLibrary` (`MaterialId`) and `Library` (`SampleId`), both already
  compiled far earlier; no later Domain module depends on it this round, so the
  end of the list is the least-disruptive slot.

## Testing state

**Attempt 02 (build-retry).** The previous attempt shipped without building and
the `build` gate failed. Because a build-failure retry cannot be fixed blind, I
built the affected projects **to diagnose** — the arc-runner's gate engine
remains the sole gate authority (Invariant 6); this session neither self-reports
gate results as authoritative nor green-lights the round.

- Root cause: `LifecycleTests.fs:98` `Assert.False(ActiveEntry = InactiveEntry)`
  raised **FS0505** — inside a method call F# parses `ident = expr` as a
  *named-argument assignment*, not an equality test, so it saw `Assert.False()`
  with zero positional args. Line 97's `Assert.True(ActiveEntry <> InactiveEntry)`
  uses `<>` and was unaffected.
- Fix: wrap the equality in extra parens so it is unambiguously a boolean
  expression — `Assert.False((ActiveEntry = InactiveEntry))`. One-line, test-only;
  `Lifecycle.fs` was already correct and unchanged.
- Verified: `dotnet build Berreman.slnx -c Release` → **0 errors** (7 pre-existing
  warnings, none from slice 020, none FS0025); `dotnet test … --filter
  ~LifecycleTests` → **15 passed, 0 failed**. `LifecycleTests.fs` re-checked LF-clean.

The remaining prose below is the original correctness reasoning; it still holds:

- New code references only in-scope symbols (`MaterialId`, `SampleId`,
  `MaterialIds.*`, `SampleId.create`) with the same access patterns as existing
  compiling code (`LibraryFacetsTests`, `LibraryProxyTests`, `PropagationTests`).
- All matches are exhaustive (`--warnaserror+:25`); single-case `let (…) = this`
  bindings follow the established `.value` idiom.
- `Set<VersionRef>` is valid — every constituent (`Guid`, `int`) is comparable.
- `decideVersioning` stays fully generic (comparators injected, no inferred
  equality constraint), so no less-generic-than-annotated surprise.
- Line endings verified LF at the byte level (`od -c`) for both new files — no
  CRLF churn (an MSYS `grep $'\r'` false-positive was ruled out by hexdump).

15 new facts added to `OpticalConstructor.Tests`, all expected to pass:
`constructor_unit_tests` 571 → 586. Other gate counts unchanged.

## Artifacts

None — pure Domain + pure tests, no captured logs/screenshots/traces.

## Gotchas

- **FS0505 `Assert.False(x = y)` (the build-retry root cause).** In F#, `=` inside
  a call's parentheses is read as a named-argument assignment, so
  `Assert.False(ActiveEntry = InactiveEntry)` fails to compile ("does not take 0
  arguments"). `<>` is safe; `=` needs extra parens:
  `Assert.False((ActiveEntry = InactiveEntry))`. Prefer `Assert.Equal`/`<>` forms,
  or double-paren any `=` fed to `Assert.True`/`Assert.False`.
- **Stored payload includes metadata.** The slice phrases the stored input as the
  "physics payload", but `KeepCurrent` (byte-identical save) can only be told from
  a metadata-only `MutateInPlace` by also comparing the stored metadata. So
  `VersionPayload` (used for both stored and incoming) carries `{ physics;
  metadata }`. This is faithful to the acceptance ("identical saves keep the
  version" AND "metadata-only edits never version" are distinct outcomes) — it just
  reads more into "payload" than the literal word "physics".
- **`decideVersioning` never mutates or persists anything** — it is a pure
  classifier returning `MutateInPlace | MintNextVersion | KeepCurrent`. Applying
  the decision (the actual overwrite / mint) is the store's job in steps 21/22;
  this slice only lands the decision + foundation, as scoped.
- **`VersionsInUse` is a seam, truthful for the live session only** until step 25
  wires it over the experiment-store descriptors (and fully truthful when
  persistence arrives). Do not assume it reflects on-disk experiments yet.
- **Case/type name collisions** (`SampleId`, `MaterialId`, `VersionNumber`) — the
  type-qualified static members (`SampleId.create`, `VersionNumber.first`) resolve
  to the member, matching the existing `newSampleId`/`MaterialId.create` idiom;
  record literals in tests are type-annotated to avoid `materialId`/`version`
  field-name overlap with `SampleLayer`.

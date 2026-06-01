# Architecture critique -- 003.slice-md cycle 2

## Summary

Clean. The two cycle-1 blockers are resolved in retry-02: the `.gitignore` edit is
now a tidy +5-line LF append (`*.binz` only, no `*.autosave` — correct, that is slice
013), and the direct `Numerics.csproj` `ProjectReference` was dropped so Math.NET flows
transitively (Storage → OpticalConstructor.Domain → Berreman → Numerics) with the SoW
now documenting that edge. Layering, the single `StorageError` channel, validate-then-bind,
and the Softellect `.binz` reuse are all spec-correct. The one new, actionable note is a
test-coverage gap: the `Matrix<Complex>` converter is never exercised with a non-zero
imaginary part. Everything else is a standing carry-forward note for later slices.

## Spec fit

- Schema envelope matches R-2: `schemaVersion` (`const "1.0"`), `beamTree`/`systems`
  required, the storage-owned `unitOfMeasure`/`materialEntry`/`sourceSpec` `$defs`, and
  the physics `$defs` left as permissive `true` anchors for Parts B–G. `required` is
  correctly limited to the fields the slice-002 aggregate actually carries, so no later
  slice is cornered into shipping a field before its part lands. The envelope `$id`,
  `const` version, and the `unitOfMeasure` enum (in-order match for the `UnitOfMeasure`
  DU) are all as the spec dictates.

- **`additionalProperties` is unset everywhere (carry-forward).** For a format whose
  purpose (binding rule 4) is to be the hand/LLM-editable canonical definition validated
  on load, permissive `additionalProperties` means a mistyped key (`systmes`, `beamtree`)
  passes validation and is then silently dropped by the binder — exactly the error class
  validate-on-load exists to catch. Defensible under minimum-implementation (rule 6) since
  no directive asks for it, but the slice that closes the envelope should set it
  consciously rather than inherit the gap by accident. No change required this slice.

## Risks

- **The `ComplexMatrixConverter` imaginary path is never exercised (new).** All three
  test fixtures build their optical stacks from `OpticalProperties.vacuum`, which is
  eps = identity, mu = identity, rho = zero (`MaterialProperties.fs:64` and the
  `fromEpsion` defaults) — every matrix element has a zero imaginary part. The converter
  writes `[c.Real, c.Imaginary]` pairs and reads `cellEl.[0]`/`cellEl.[1]` back
  (`ProjectJson.fs:41-42,57-58`); a transposed index, a dropped imaginary component, or a
  re/im swap would round-trip vacuum identically and pass AC-A5/AC-D6/AC-I1 unchanged.
  Since this converter is the load-bearing reason whole-project `=` holds (per the SoW
  Architecture note), the round-trip proof would be materially stronger with one fixture
  whose `OpticalProperties` carries a non-zero, non-symmetric complex tensor. Worth a
  single added assertion; not a blocker, and slice 005's AC-B9 beam-tree round-trip may
  absorb it.

- **Sidecar relative-path pattern still admits parent traversal (carry-forward).** The
  `sidecars` pattern `^[^/\\][^:]*\.binz$` (schema line 48) correctly rejects absolute
  paths and drive letters, but `..\..\x.binz` matches. The spec only requires "relative,"
  so this is not a violation now; when a later slice resolves these paths against the
  project folder, the traversal must be re-clamped at the IO boundary.

- **`JsonSchema.FromText` global `$id` registry (carry-forward).** The `lazy` cache
  loads once and stores the `Error` rather than re-throwing, so within this assembly it is
  sound and the Gotchas document it. The residual hazard is cross-assembly: any other
  component loading the same `$id` in-process would hit "Overwriting registered schemas."
  A real constraint the canonical `$id` imposes process-wide, not actionable this slice.

## Evolvability

- **`writeSidecar<'T>` keeps the project off `.binz` by convention only (carry-forward).**
  AC-I4 is satisfied — no call site pickles a project — but the generic signature lets a
  future sweep/field-map slice violate binding rule 4 with no compiler complaint. A
  type-level guard is out of scope (rule 6); the judge should carry the fact that the
  binding-rule-4 firewall is documentation-enforced into the slices that add sidecar
  producers.

## Bottom line

This is shippable from an architecture standpoint and is materially cleaner than cycle 1:
both cycle-1 blockers (the `.gitignore` line-ending churn and the redundant direct C#
project reference) are fixed, and the SoW now records the transitive Math.NET edge that
was previously omitted. Layering is correct (Storage → Domain → engine, Softellect /
Math.NET as admitted leaf deps, no reverse edges), the `StorageError` channel is the
single non-throwing seam the spec asked for, and every owned AC has a focused test that
passes. My read: ship. The one new finding (imaginary-component round-trip is untested)
is a hardening note, not a defect, and the remaining items are standing carry-forwards for
later slices. The judge decides.

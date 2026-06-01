# Code judge -- 003.slice-md cycle 2

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0022\.slices\003.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0022\.slices\003-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0022\.slices\003-impl-log.md`
- Gate results: build = pass, unit-tests = pass, constructor-unit-tests = pass
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0022\.artifacts\architecture_critic\003-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0022\.artifacts\reuse_critic\003-02-reuse-critic.md`

## Rationale

This slice clears `done-green` ground. All three gates are green, and every
acceptance criterion the slice owns is exercised by a test in the diff: AC-A2
(validate-on-load admits a valid document), AC-A5/AC-D6 (SI magnitudes stored
verbatim, the `defaultUnit` label preserved — `ProjectJsonRoundtripTests.fs`),
AC-I1 (whole-project `saveProject`→`openProject` `=` identity), AC-I2 (schema
violation → `SchemaValidationError`), AC-I3 (`schemaVersion` mismatch →
`SchemaValidationError`, no migration — `RoundTripTests.fs`), and AC-A6/AC-I4/AC-I5
(`.binz` sweep-table round-trip, `.binz` extension, missing-sidecar →
`SidecarMissing` — `SidecarTests.fs`). The SoW and impl-log line up with the diff:
the `schemaVersion` envelope injection, the `Errors → SchemaValidation → ProjectJson
→ ProjectFile → Sidecar` compile order, the `Matrix<Complex>` converter, and the
`*.binz`-only `.gitignore` append are all present as described.

Both cycle-1 blockers are resolved in retry-02. The `.gitignore` edit is now a clean
+5-line LF append (`*.binz` only, no premature `*.autosave` — correct, that is slice
013), and the redundant direct `Numerics.csproj` `ProjectReference` was dropped so
Math.NET flows transitively (Storage → OpticalConstructor.Domain → Berreman →
Numerics), with the SoW now documenting that edge. The architecture critic confirms
layering is correct (no reverse edges, Softellect/Math.NET as admitted leaf deps), the
`StorageError` DU is the single non-throwing channel R-1 demanded, and validate-then-bind
is honoured. The reuse critic confirms **no production-code reuse violations**: the
named primitives are called per binding rule 2 (`System.Text.Json` +
`FSharp.SystemTextJson`, `Softellect.Sys.Core.serialize`/`BinaryZippedFormat`,
`BinaryZippedFormat.fileExtension`), and `StorageError` wraps nothing from
`Softellect.Sys.Errors`. Binding rule 4 holds — no `writeSidecar` call site carries an
`OpticalConstructorProject` (AC-I4).

The only new finding this cycle is the architecture critic's note that the
`ComplexMatrixConverter` (`ProjectJson.fs:34-62`) — the one piece of genuinely new,
non-trivial logic in the slice and, per the SoW, the load-bearing reason whole-project
`=` holds — is exercised only with `OpticalProperties.vacuum` fixtures, whose tensors
have zero imaginary part throughout. I weighed routing back to add a non-zero,
non-symmetric complex fixture, but two things hold it under the `done-green` line.
First, I inspected the converter directly: `Write` emits `c.Real` then `c.Imaginary`
and `Read` takes `cellEl.[0]` then `cellEl.[1]`, with consistent row/col iteration on
both sides — the code is **correct on inspection**, so this is a missing *regression
test* for correct code, not a latent defect (the critic agrees: "a hardening note, not
a defect"). Second, the converter is `private` infrastructure exercised through the
public `serializeProject`/`deserializeProject` round-trips, which are tested — the
`done-green` coverage criterion (new public surface exercised by a test in the diff) is
met; the gap is test *strength*, not test *existence*, and no owned AC requires a
non-zero imaginary fixture. Against a slice both critics recommend shipping, with the
binding minimum-implementation rule (rule 6) explicitly in force, the defensible call
is to ship and carry the hardening forward — exactly as the architecture critic offered
("slice 005's AC-B9 beam-tree round-trip may absorb it").

Carry-forwards recorded for later slices (none actionable now, all out of scope under
rule 6): the imaginary-component regression test (best landed with slice 005 AC-B9);
`additionalProperties` left unset on the envelope (set consciously when the schema is
closed); the `sidecars` relative-path pattern admitting `..` traversal (re-clamp at the
IO boundary when paths are resolved); the process-global `$id` registry hazard
(cross-assembly only); the documentation-only binding-rule-4 firewall on
`writeSidecar<'T>` (carry into the slices that add sidecar producers); and the reuse
critic's test-scaffolding consolidation (F1–F3) plus the defensible F4/F5 near-misses —
all advisory, and the repo's standing convention is per-module private fixtures with no
shared helper to reuse.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All three gates green; every owned AC (A2/A5/A6/D6/I1/I2/I3/I4/I5) is exercised by a passing test in the diff. Both cycle-1 blockers are fixed in retry-02 (the .gitignore is a clean +5-line LF *.binz append; the redundant direct Numerics.csproj reference was dropped with Math.NET flowing transitively and the SoW documenting it). Layering is correct, StorageError is the single non-throwing channel, binding rule 4 holds (no project on the .binz path), and the reuse critic finds no production-code reuse violations. The sole new finding -- the ComplexMatrixConverter is exercised only with zero-imaginary vacuum fixtures -- is a missing regression test for code that is correct on inspection (Write Real/Imaginary, Read [0]/[1], consistent iteration), not a defect; the converter is private infra exercised via the tested public round-trips, so the coverage criterion is met. With both critics recommending ship and minimum-implementation binding, this ships, carrying the imaginary-component test forward (slice 005 AC-B9) along with the other standing carry-forwards.", "retry_hint": ""}
```

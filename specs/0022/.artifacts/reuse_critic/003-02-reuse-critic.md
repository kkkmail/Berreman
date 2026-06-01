# Reuse critique -- 003.slice-md cycle 2

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` — the five new Storage `.fs` modules, the
  new `schema/*.json`, the `.gitignore`/`.fsproj` diffs, the four sibling
  `OpticalConstructor.Tests` modules, and the cited engine helpers in
  `Berreman/Berreman/**` (`MathNetNumericsMath.fs`, `MaterialProperties.fs`, `Media.fs`).
- Files inspected: ~18/200 (no `max_files` cap trip).
- Extensions: the task pinned `.py,.md,.json`, but this is a pure-F# repo with no
  `.py`; the reuse-bearing surface is `.fs`, so I walked `.fs` helper roots plus the
  new schema `.json` and the MD inputs.

## Findings

### F1: New test fixtures re-declare the established `vacuumSystem` / `light` / `node` pattern

- **Worker added:** private `vacuumSystem`, `light`, and a `node` builder in
  `ProjectJsonRoundtripTests.fs:23-41`, plus another `vacuumSystem`/`light` and an
  inline root node in `RoundTripTests.fs:22-42`.
- **Existing helper:** the same fixture shape already exists, near-verbatim, at
  `BeamTreeTests.fs:17-35` (`vacuumSystem`, `light`, `node`) and again at
  `BeamRoutingTests.fs:24-40`. The `light = IncidentLightInfo.create
  (WaveLength.nm 600.0<nm>)` line is byte-identical to `BeamTreeTests.fs:26`.
- **Why it matters:** four copies of the vacuum-stack + incident-light fixture now
  drift independently. When the `OpticalSystem`/`BeamNode` record gains a field
  (materials → slice 004, sources → slice 007) every copy must be edited in lockstep
  or the test project stops compiling.
- **Suggested action:** advisory — extract a shared `OpticalConstructor.Tests`
  fixtures module (`vacuumSystem`, `light`, a parameterized `node`) consumed by all
  four modules. **Caveat for the judge:** the repo has *no* shared test-fixtures
  module today — every existing test module declares its own `private` fixtures, so
  the worker *followed* the standing convention rather than diverging from it. This is
  duplication-by-precedent. Weigh accordingly.

### F2: `okOr` is a near-miss of the existing `okOrFail`

- **Worker added:** `okOr : Result<'a,_> -> 'a` in `ProjectJsonRoundtripTests.fs:50-53`
  (unwrap `Ok`, else `failwith $"unexpected storage error: {e}"`).
- **Existing helper:** `okOrFail : Result<BeamNode, BeamTreeError> -> BeamNode` in
  `BeamTreeTests.fs:37-40` — identical shape (`Ok v -> v | Error e -> failwith ...`).
- **Why it matters:** two same-shape Result-unwrap helpers under different names invite
  a third in the next test module. The existing one is needlessly monomorphic; the
  worker's is already generic.
- **Suggested action:** advisory — promote one generic `okOrFail : Result<'a,'e> -> 'a`
  to the shared test helper proposed in F1 and drop both local copies. Low stakes; same
  convention caveat as F1.

### F3: `withTempFile` defined twice within this slice

- **Worker added:** `withTempFile` in `RoundTripTests.fs:44-47` and again in
  `SidecarTests.fs:21-24` — same create-temp-path / `try f` / `finally delete` shape,
  differing only in filename prefix and extension.
- **Existing helper:** no pre-slice copy exists (grep finds it only in these two new
  files), so per the rubric this is a near-miss against the *sibling new file* rather
  than against committed code — surfaced so the judge sees it, not asserted as a
  reuse-of-existing miss.
- **Why it matters:** the temp-file lifecycle (GUID path, cleanup-in-`finally`) is
  exactly the scaffolding that should be written once; two copies is the start of the
  same drift F1 describes.
- **Suggested action:** advisory — fold into the same shared test-helper module
  (F1/F2), parameterizing the extension. Honest framing: both copies are new in this
  diff, so this is "don't ship the duplicate," not "you missed an existing helper."

### F4: `ComplexMatrixConverter` re-derives the engine's matrix decompose/rebuild idiom (near-miss)

- **Worker added:** the manual row/column walk in `ProjectJson.fs:34-62` — nested
  `WriteStartArray`/`GetDouble` loops flattening a `Matrix<Complex>` to `[re, im]`
  pairs and rebuilding via `Matrix<Complex>.Build.DenseOfArray`.
- **Existing helper:** the engine exposes the decompose half as `ComplexMatrix.re` /
  `ComplexMatrix.im` (`MathNetNumericsMath.fs:177-185` — exactly the
  `[| for i.. -> [| for j.. -> m.[i,j].Real/.Imaginary |] |]` pattern) and the rebuild
  half as `ComplexMatrix.create : #seq<#seq<Complex>>` (`MathNetNumericsMath.fs:149`).
- **Why it matters:** the per-element extraction/reconstruction logic is duplicated
  rather than delegated.
- **Suggested action:** **leave as-is is defensible; document why.** The converter is
  deliberately registered on the *raw* Math.NET `Matrix<Complex>`, not the
  `ComplexMatrix` single-case wrapper, because FSharp.SystemTextJson unwraps
  `Eps`/`Mu`/`Rho` → `ComplexMatrix3x3` → down to the bare matrix before the converter
  sees it; the engine helpers operate one type-level up on the wrapper, so reuse would
  force a wrap/unwrap. The duplication is shallow (one flatten, one build) and the
  divergence is justified by the unwrap boundary. A one-line comment noting the
  `.re`/`.im`/`create` equivalents would suffice. Advisory.

### F5: raw `Thickness.Thickness` bypasses the `Thickness.nm` smart constructor used by sibling tests

- **Worker added:** `Thickness.Thickness filmThickness` (`ProjectJsonRoundtripTests.fs:27`)
  and `Thickness.Thickness 2.5e-7<meter>` (`RoundTripTests.fs:26`), constructing the DU
  case directly from a raw `<meter>` literal.
- **Existing helper:** `Thickness.nm`/`mkm`/`mm` smart constructors (`Media.fs`),
  used in the sibling `BeamRoutingTests.fs:66,75` (`Thickness.nm 250.0<nm>`).
- **Why it matters:** minor — direct case construction is the less-used path.
- **Suggested action:** leave as-is, likely intentional: these tests assert
  *byte-identical SI preservation* (AC-A5/AC-D6), so pinning the exact stored `<meter>`
  magnitude rather than routing through a unit conversion is the defensible choice.
  Documented only so the judge knows it was weighed, not flagged.

## Bottom line

No production-code reuse violations: the storage modules call the named primitives
(`System.Text.Json` + `FSharp.SystemTextJson`, `Softellect.Sys.Core.serialize`/
`BinaryZippedFormat`, `BinaryZippedFormat.fileExtension`, the domain constructors) per
binding rule 2, and the new `StorageError` DU is the net-new sole channel as directed —
nothing wraps `Softellect.Sys.Errors`. The substantive findings are all in test
scaffolding (F1–F3, mild and consolidatable; F4/F5 are defensible near-misses), and the
repo's standing convention is per-module private fixtures with no shared helper to
reuse. My read: not substantive enough to compel a re-spawn on reuse grounds — a
follow-up consolidating the test fixtures/helpers would pay off as slices 004/007 extend
the aggregate, but it is optional and the judge owns that call.

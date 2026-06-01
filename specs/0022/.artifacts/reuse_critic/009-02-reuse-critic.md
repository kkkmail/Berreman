# Reuse critique -- 009.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (repo root). Focused on the
  optimization/analytics/engine seams the slice touches: `Analytics/Variables.fs`,
  `Analytics/StandardLightVariables.fs`, `Analytics/Examples/*.fsx`, `Berreman/Media.fs`,
  `Berreman/Fields.fs`, `Berreman/Dispersion.fs`, `Berreman/Constants.fs`,
  `BerremanTests/AnalysisFunctionsTests.fs`, the three new `OpticalConstructor.Optimization`
  files, and `OptimizationInterfaceTests.fs`. Targeted greps (`getSys`, `sumSq`/norm helpers,
  `applyVector`/`DesignParameter`, forward-difference gradient) swept the rest of the tree.
- Files inspected: ~30 read/grepped (well under max_files 200; no cap trip).
- Extensions: the task's configured set (`.py,.md,.json`) does not match this pure-F# repo,
  so the substantive walk was `.fs`/`.fsproj` plus the `.md`/`.json` spec files. Recording the
  mismatch so the judge knows the walk targeted the real code, not the literal template.
- Excluded per rubric / §A.8: `MathNetNumerics/` (vendored C# clone) and ALGLIB's own routines.

## Findings

### F1: `DesignParameter` record re-declares the `ArbitraryVariable` shape

- **Worker added:** `type DesignParameter = { name; getSys; lower; upper }`
  (`DesignParameters.fs:27`).
- **Existing helper:** `type ArbitraryVariable = { variableName; range; scale; getSys }`
  at `Analytics/Variables.fs:32-38`, carried by `RangedVariable.ArbitraryVariableRange`
  (`Variables.fs:48`) and consumed by `calculate` at `Variables.fs:263` (`v.getSys s z`).
- **Why it matters:** Both records wrap the identical
  `getSys : OpticalSystem -> double -> OpticalSystem` seam plus a name/label. They diverge
  only in the scalar payload: `ArbitraryVariable` carries a plotting `Range<double>` + `scale`
  (sweep semantics), `DesignParameter` carries an SI `lower`/`upper` box pair (optimization
  semantics). That divergence is real, not cosmetic — a sweep range and a fit bound are
  different concepts — so the two are not interchangeable, but a future reader sees two
  near-identical "named getSys carrier" records and must learn which layer owns which.
- **Suggested action:** **Leave as-is.** R-3 explicitly mandates "a `DesignParameter`
  (new record)" reusing the *seam* (the `getSys` signature), not the whole `ArbitraryVariable`
  record, and the slice does reuse the signature verbatim. The semantic split (sweep range vs
  fit bound) justifies a distinct type. Recording it only so the judge has the near-miss on the
  table; no action needed unless the judge wants a shared comment cross-linking the two carriers.

### F2: `sumSq` hand-rolled instead of an existing norm helper

- **Worker added:** `let private sumSq (r : float[]) = Array.fold (fun acc v -> acc + v*v) 0.0 r`
  (`AlglibAdapter.fs:23`), used for the SSR objective and (with `sqrt`) the residual L2 norm at
  lines 63, 107, 168.
- **Existing helper:** `Berreman.Fields.l2Norm : #seq<Complex> -> float` (`Fields.fs:17-19`),
  the engine's sum-of-squares-then-`sqrt` over a complex vector.
- **Why it matters:** `sumSq` overlaps conceptually with `l2Norm`, so on the surface this looks
  like a duplicated norm. In substance it is not a clean reuse target: `l2Norm` is fixed to
  `Complex` and bakes in the `sqrt`, whereas the adapter needs the *squared* sum over a real
  `float[]` (it adds `sqrt` only at two of three call sites). Reusing `l2Norm` would force a
  real→`Complex` boxing of every residual and lose the squared-vs-rooted distinction. The only
  other sum-of-squares in the tree are test-local (`FourierTransformTests.fs:164,177`) or
  vendored (`MathNetNumerics/.../Quaternion.fs:74`), neither a first-class engine helper.
- **Suggested action:** **Leave as-is.** A one-line private fold over `float[]` with no
  Complex/`sqrt` baggage is the right local primitive here, and §A.8 forbids routing the
  optimization math through `MathNetNumericsMath.fs` anyway. Borderline near-miss recorded for
  completeness; not worth a change.

## Bottom line

The diff faithfully reuses the seam R-3 names — `DesignParameter.getSys` mirrors
`ArbitraryVariable.getSys` exactly, the wedge-angle closure follows the established
`match sys.substrate with Some (Wedge w) -> … | _ -> sys` pattern from
`Examples/Wedge_ActiveCrystal.fsx:47`, and thickness/dispersion mappings stay explicit closures
with no reflection. Both findings are spec-sanctioned or genuinely-distinct near-misses with
"leave as-is" actions, and the one duplication a reviewer might reach for (reusing MathNet's
`ForwardDifferenceGradientObjectiveFunction` for the gradient) is correctly avoided because
§A.8 reserves optimization for ALGLIB. Nothing here is substantive enough to justify a
re-spawn; the judge can route this as a clean reuse pass.

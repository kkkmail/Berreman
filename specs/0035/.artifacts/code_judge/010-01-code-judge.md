# Code judge -- 010.slice-md cycle 1

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0035\.slices\010.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0035\.slices\010-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0035\.slices\010-impl-log.md`
- Gate results: build = pass, unit-tests = pass, constructor-unit-tests = pass, ui-smoke = pass, ui-tests = pass
- Critic critiques: (none — no critic ran this cycle)

## Rationale

All five deterministic gates are green and no critic emitted a finding, so the
review reduces to (a) whether the diff meets the slice-spec contract and (b)
whether the SoW/impl-log honestly describe what the diff does. Both hold.

**Slice-spec requirements are met, verified against `git diff HEAD`.** The active
and magnetic rungs now carry a Constant-vs-Dispersive sub-branch via the new
`ComponentDispersion` DU (a named two-case DU, not a naked bool — consistent with
the project's elevate-every-primitive discipline). The dispersive facets
(`gyrationDispersion : GyrationClass<DispersionFormula>`,
`polderDispersion : PolderValue<DispersionFormula>`) are stored *independently* of
the constant `gyration`/`polder` facets, so unchecking Dispersive restores the
constant losslessly (proven by the fourth test). `toComplexity`
(`MaterialComplexityEditor.fs`) builds `RhoWithDispValue` / `MuWithDispValue` under
the sub-toggle and `RhoWithoutDispValue` / `MuWithoutDispValue` otherwise;
`ofComplexity` switched from `Result.bind` to `Result.map` and now seeds a
dispersive ρ/μ verbatim into the dispersive facet (seeding never fails). The
`UnsupportedComplexity` view-only case is deleted from `MaterialComplexityEditError`
— I grepped the tree and the only surviving references are doc comments and
spec/history files; no code path matches or constructs it. The engine builders
`RhoWithDispValue.toRhoWithDisp` (`Active.fs`) and `MuWithDispValue.toMuWithDisp`
(`Dispersion.fs`) are untouched (`git diff --stat` shows zero changes to those
files), as the spec required.

**New public surface is exercised, and the tests cross-check the real engine, not
a re-implementation.** Four new tests in `MaterialComplexityTests` cover: (1)
`toComplexity` of an on-Dispersive uniaxial gyration builds the exact
`GyrationClass<DispersionFormula>` and its `toRhoWithDisp` assembly equals a
hand-built `Rho.type_3_4_6_Crystal` with the enantiomorph sign at every wavelength
of `visibleGrid`; (2) the Polder analogue against `toMuWithDisp` /
`GyromagneticMu`; (3) an `ofComplexity`→`toComplexity` round-trip of a dispersive
ρ and μ compared by sampled tensor value through the engine builders; (4) the
lossless Dispersive uncheck for both rungs. The expected tensors independently
reconstruct the engine's class routing and handedness sign — confirmed against
`assembleRho` in `Active.fs:133-143` — so the tests are genuine cross-checks, not
tautologies. They reuse the module's pre-existing `rhoClose`/`muClose`/`visibleGrid`
tolerance helpers rather than hand-rolling new epsilon logic, matching the project
convention. This satisfies the `done-green` test-coverage criterion for every new
message, the `ComponentDispersion` DU, the generalised `gyrationComponents` /
`setGyrationComponent`, and the reshaped `toComplexity` / `ofComplexity`.

**SoW and impl-log line up with the diff.** Every claim — the independent facets,
the six new messages, the generic gyration helpers, the class-sync helper
(`syncGyrationDispersion` at the three mutation points), the axis-on-both-facets
rule in `ChooseGyrationAxis`, and the count bump `constructor_unit_tests 440 → 444`
— is present in the code exactly as described. The one edit outside the declared
`touches` (a forced 1-line drop of `UnsupportedComplexity` from the
`editErrorReason` OR-pattern in `OpticalConstructor.TestWindows/MaterialEditorView.fs`)
is fully disclosed in both the SoW and impl-log and is an unavoidable, mechanical
consequence of deleting the DU case — leaving it would break exhaustiveness and
fail the build gate, and keeping the DU case would violate the acceptance clause
that it "MUST no longer exist." That is not scope creep and not a layering
violation; it is the minimum edit required to keep the solution compiling. The
deferred dispersive-editing UI is legitimately out of this slice's `touches`
(`OpticalConstructor.Ui`/`.TestWindows` are not Domain/Tests) and is recorded
honestly as future work; nothing in this slice's own scope is deferred.

No finding rises to `route-back-to-worker`, and nothing suggests a deep
architectural problem or self-contradiction that would warrant escalation. The
slice fully meets `done-green` ground.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and no critic ran. The diff meets every slice-spec requirement verified against git diff: the ComponentDispersion sub-branch with independent dispersive facets (lossless uncheck), toComplexity building RhoWithDispValue/MuWithDispValue under the sub-toggle, ofComplexity seeding a dispersive rho/mu verbatim, and the UnsupportedComplexity case deleted (no surviving code references; engine toRhoWithDisp/toMuWithDisp untouched). Four new tests cross-check toComplexity and the ofComplexity round-trip against the real engine builders over a wavelength grid and prove the lossless uncheck, reusing the module's existing tolerance helpers. SoW and impl-log match the diff; the single edit outside the declared touches (a 1-line drop of the deleted error case in TestWindows/MaterialEditorView) is a disclosed, build-forced consequence, not scope creep.", "retry_hint": ""}
```

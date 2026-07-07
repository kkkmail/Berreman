# Code judge -- 001.slice-md cycle 1

## Inputs read

- Slice spec: C:\GitHub\Berreman\specs\0033\.slices\001.slice-md
- State-of-the-world: C:\GitHub\Berreman\specs\0033\.slices\001-state-of-the-world.md
- Impl-log: C:\GitHub\Berreman\specs\0033\.slices\001-impl-log.md
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none filed this cycle)

## Rationale

All five gates in the roster pass, and no critic critique was filed this
cycle, so the verdict rests on my own verification of the worker's claims
against the diff (`git diff HEAD`) and on the test-coverage rule for new
public surface. Both hold.

**The diff does what the slice demands, item for item.** `ElementId.fs`
adds `SampleLayer`, `PeriodGroup`, `StackItem`, and `SampleStructure`
(films / substrate option / lower option, None = vacuum) with a pure
`expandedFilms` member that is verbatim the `RepeatBuilder.expand` shape
(`List.replicate g.count g.cell |> List.concat` — I compared against
`OpticalConstructor.Ui/RepeatBuilder.fs:24`). `Sample.materialId` +
`Sample.thickness` are gone, replaced by `structure : SampleStructure`,
with `substrate : SubstrateKind` retained as the geometry facet. All 11
seeds are re-seeded structurally, and the three named shapes match the
slice exactly: QW = `Repeated` 20×[glass λ/4; vacuum λ/4] plus one trailing
glass `SingleLayer`; EUV = `Repeated` 100×[Mo; Si]; langasite = films
[langasite] with `lower = Some "silicon"`. `MaterialLibrary.fs` adds
`euv-molybdenum` / `euv-silicon` over the engine's `euvMolybdenum` /
`euvSilicon` presets and `active-crystal` over `planarCrystal` with n₁₁
2.315 / n₃₃ 2.226 / ρ₁₂ 1.5e-6 — the exact constants deleted from
`Propagation.fs`. In `Propagation.fs`, the `propertiesOf` glass-fallback
map, `activeCrystalProperties`, `langasiteOnSiliconWithDisp`, and every
`sample.id` string branch are deleted; `resolveSampleMaterials :
MaterialLibrary -> Sample -> Result<ResolvedSample, MaterialError>` is the
single typed resolution seam (unknown id → `Error (UnknownMaterialId _)`,
no fallback anywhere), and `sampleToSystem` is now a total
`ResolvedSample -> WaveLength -> OpticalSystem` with no branching.
`sampleMueller`/`T`/`R` and all four sweep builders take `ResolvedSample`;
the TestWindows host resolves once per run (`runResolvedSampleOpt`) and
surfaces the error as the chart title/description.

**The acceptance criterion is asserted by a real test.** The new
`legacyExpectedSystem` in `PropagationTests.fs` replicates the pre-0033
hand-built per-id systems verbatim (including the 41-layer QW film list and
the 100-pair EUV stack) and `Assert.Equal<OpticalSystem>` compares every
seeded sample's structurally-built system record-for-record at its
reference wavelength — films, substrate, and tensors — with descriptions
dropped, a recorded and harmless deviation since the acceptance names film
count / substrate / tensors only. Unknown-id behavior is covered by two
typed-error tests (film and lower half-space), and every-seed-resolves plus
the `expandedFilms`-mirrors-`RepeatBuilder.expand` test round out the five
added tests. New public surface is therefore exercised:
`SampleStructure.expandedFilms`, `resolveSampleMaterials`, the rebuilt
`sampleToSystem`, and the four new material built-ins (whose tensors the
legacy-equality test pins) all have direct assertions;
`resolveMaterialWithDisp` is exercised through the resolution tests and
`resolveMaterial` is re-expressed through it under existing coverage.

**SoW and impl-log line up with the diff, including the deviations.** The
worker recorded two justified departures in Gotchas: (1) a `vacuum`
built-in the slice's list omitted but the QW re-seed requires under
"unknown id is a typed Error, never a fallback" — added with a new
`MaterialCategory.Vacuum` case rather than miscategorising it as Glass,
which is the defensible choice; (2) a one-line schema-enum extension in
`optical-constructor-project.schema.json`, one file outside the declared
`touches`, needed to keep the AC-I10 schema-validation test green. Both are
minimal, explained, and consistent with the project prompt's
"pick a sensible default and record it" rule. Test counts (306
constructor tests, +5 added, none removed) mean the `count_at_least`
baselines can only have grown.

Minor observations, not blocking: the host's error-message chart path has
no dedicated UI test, but the typed-error seam it projects is
domain-tested, which is where CLAUDE.md directs behavior tests; and
`SampleLayer.materialId` / `SampleStructure.lower` remain bare strings,
which the slice explicitly defers to step 2. Nothing here would justify
spending a re-spawn cycle.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and no critic filed a finding. Diff verification confirms every slice requirement: SampleLayer/PeriodGroup/StackItem/SampleStructure with a pure expandedFilms mirroring RepeatBuilder.expand; Sample.materialId+thickness replaced by structure with substrate kept as geometry; all 11 seeds re-seeded structurally (QW 20x[glass;vacuum]+trailing glass, EUV 100x[Mo;Si], langasite over silicon lower); euv-molybdenum/euv-silicon/active-crystal built-ins added with the exact constants removed from Propagation.fs; propertiesOf fallback and all sample.id branching deleted; resolveSampleMaterials returns typed UnknownMaterialId errors; sampleToSystem is total over ResolvedSample; sweeps and hosts re-pointed, resolving once per run. The acceptance is asserted by a legacy-equality test comparing structurally-built systems record-for-record against verbatim replicas of the pre-0033 hand-built systems, plus two unknown-id typed-error tests; five tests added, none removed. The two recorded deviations (a required vacuum built-in the slice omitted; a one-line schema enum extension outside touches) are minimal and justified. SoW and impl-log match the diff exactly.", "retry_hint": ""}
```

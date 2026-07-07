# Slice 001 — impl plan (make the sample stack DATA)

## Goal

Replace the per-sample-id string branching in `Propagation.sampleToSystem` with a
data-driven `SampleStructure` on `Library.Sample`, resolved against the
`MaterialLibrary` (unknown id = typed `Error`, never a fallback), and rebuild
`sampleToSystem` as a total `ResolvedSample -> WaveLength -> OpticalSystem`.

## Approach

1. **`OpticalConstructor.Domain/ElementId.fs`** — add `SampleLayer`
   (`materialId : string` + `thickness : Thickness`), `PeriodGroup`
   (`cell : SampleLayer list`, `count : int`), `StackItem = SingleLayer | Repeated`,
   and `SampleStructure` (`films : StackItem list`, `substrate : SampleLayer option`,
   `lower : string option` where `None` = vacuum) with a pure `expandedFilms`
   member mirroring `RepeatBuilder.expand` (`List.replicate count cell |> List.concat`).
   Replace `Sample.materialId`/`Sample.thickness` with `structure : SampleStructure`,
   keeping `substrate : SubstrateKind` as the geometry facet. Re-seed all 11 samples
   structurally (QW = Repeated 20×[glass λ/4; vacuum λ/4] + trailing glass;
   EUV = Repeated 100×[Mo; Si]; langasite = films [langasite] + lower silicon;
   plates = structure.substrate layers).

2. **`OpticalConstructor.Domain/MaterialLibrary.fs`** — add
   `resolveMaterialWithDisp : MaterialLibrary -> string -> Result<OpticalPropertiesWithDisp, MaterialError>`
   (and re-express `resolveMaterial` through it); add built-ins `vacuum` (needed by the
   QW spacer layers — new `Vacuum` category case), `euv-molybdenum` / `euv-silicon`
   (over `OpticalProperties.euvMolybdenum` / `euvSilicon`) and `active-crystal`
   (over `OpticalProperties.planarCrystal`, n11 2.315 / n33 2.226 / rho12 1.5e-6,
   the constants currently inlined in Propagation.fs).

3. **Storage schema** — extend the `materialEntry.category` enum with `"Vacuum"`
   (otherwise the AC-I10 export-validates test fails on the new built-in).

4. **`OpticalConstructor.Domain/Propagation.fs`** — delete `propertiesOf` (the glass
   fallback map), `activeCrystalProperties`, `langasiteOnSiliconWithDisp`, and the
   `sample.id` branching. Add `ResolvedSample` (name + `LayerWithDisp` films/substrate +
   `OpticalPropertiesWithDisp` lower) and
   `resolveSampleMaterials : MaterialLibrary -> Sample -> Result<ResolvedSample, MaterialError>`.
   Rebuild `sampleToSystem : ResolvedSample -> WaveLength -> OpticalSystem` total over
   the expanded structure (no rotation yet — step 20). Re-point `sampleMueller`/`T`/`R`
   and the four sweep builders at `ResolvedSample`.

5. **`OpticalConstructor.TestWindows/TableAndElementRotationView.fs`** (host) —
   resolve the bound sample ONCE per chart run against `MaterialLibrary.standard`;
   a resolution `Error` yields an empty chart whose description carries the message.
   Rebuild the Details band view from the structure (films/substrate/lower) instead of
   the removed per-id band table; colours re-keyed by material id.

6. **Tests** — update `PropagationTests` (structural `glassSample`, resolve-then-solve),
   `LibraryProxyTests` (structural sample literal). Add: unknown-material-id resolution
   returns typed `Error` (films and lower); every seeded sample resolves Ok; the
   structurally-built QW / EUV / plate / langasite systems equal the previously
   hand-built ones (film count, substrate, tensors — `OpticalProperties` records
   compare structurally, precedent StackEditTests.fs:286).

## Risks

- `--warnaserror+:25`: the new `Vacuum` category case must not break exhaustive
  matches — checked: no exhaustive `match` over `MaterialCategory` exists.
- MathNet matrix equality is value-based (StackEditTests already relies on it) —
  used for the "systems equal" acceptance test.
- ui-smoke / ui-tests gates: Ui project doesn't touch `Sample` fields (verified by
  grep); only TestWindows does.

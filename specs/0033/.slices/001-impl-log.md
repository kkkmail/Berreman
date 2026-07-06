# Slice 001 — impl log

## Progress

- [x] Recon: read slice, Propagation.fs, ElementId.fs, MaterialLibrary.fs, host
      (TableAndElementRotationView.fs), PropagationTests.fs, schema, gates.
- [x] ElementId.fs: `SampleLayer` / `PeriodGroup` / `StackItem` / `SampleStructure`
      (with pure `expandedFilms` member mirroring `RepeatBuilder.expand`);
      `Sample.materialId` + `Sample.thickness` replaced by `structure : SampleStructure`
      (geometry facet `substrate : SubstrateKind` kept); all 11 seeds re-seeded
      structurally (QW = `Repeated` 20×[glass λ/4; vacuum λ/4] + trailing glass;
      EUV = `Repeated` 100×[Mo; Si]; langasite = films [langasite], lower = silicon;
      plates as `structure.substrate` layers).
- [x] MaterialLibrary.fs: new `resolveMaterialWithDisp` seam (`resolveMaterial`
      re-expressed through it); new built-ins `vacuum` (new `Vacuum` category case),
      `euv-molybdenum` / `euv-silicon` (over `OpticalProperties.euvMolybdenum` /
      `euvSilicon`), `active-crystal` (over `OpticalProperties.planarCrystal`,
      n₁₁ 2.315 / n₃₃ 2.226 / ρ₁₂ 1.5e-6 — the constants formerly inlined in
      Propagation.fs).
- [x] Storage schema: `materialEntry.category` enum += `"Vacuum"` (keeps the AC-I10
      export-validates test green for the new built-in).
- [x] Propagation.fs: deleted `propertiesOf` (glass-fallback map),
      `activeCrystalProperties`, `langasiteOnSiliconWithDisp`, and ALL `sample.id`
      string branching; added `ResolvedSample` (LayerWithDisp films/substrate +
      dispersive lower) and
      `resolveSampleMaterials : MaterialLibrary -> Sample -> Result<ResolvedSample, MaterialError>`
      (unknown id = typed Error, never a fallback); `sampleToSystem` rebuilt as a
      total `ResolvedSample -> WaveLength -> OpticalSystem` (no rotation — step 20);
      `sampleMueller`/`T`/`R` and the four sweep builders re-pointed at `ResolvedSample`.
- [x] Host (TestWindows/TableAndElementRotationView.fs): resolves the bound sample
      ONCE per chart run (`runResolvedSampleOpt` over `MaterialLibrary.standard`);
      a resolution Error yields an empty-series chart whose title/description carry
      the message; VaryR1/R2/λ branches and the Ψ/Δ readout consume the resolved
      sample; Details band view now reads the structure (films → substrate → lower)
      instead of the removed per-sample-id band table, colours re-keyed by material id,
      labels via the library display name.
- [x] Tests updated: PropagationTests (structural `glassSample` + `resolveOrFail`
      helper; seeded tests resolve-then-solve; langasite lower = silicon),
      LibraryProxyTests (structural sample literal).
- [x] New tests (acceptance): unknown film material id → typed
      `Error (UnknownMaterialId _)`; unknown lower half-space id → typed Error;
      every seeded sample resolves Ok; `expandedFilms` mirrors `RepeatBuilder.expand`;
      and `legacyExpectedSystem` — a verbatim replica of the pre-0033 hand-built
      per-id systems — compared record-for-record (films, substrate, tensors) against
      every seeded sample's structurally-built system (MathNet matrix equality is
      value-based: Matrix.BCL.cs `Equals` → `Storage.Equals`; precedent
      StackEditTests.fs:286).
- [x] Local verification (advisory — the gate engine re-runs authoritative gates):
      build 0 errors; BerremanTests 84 passed; OpticalConstructor.Tests 306 passed;
      ui-smoke 54 passed; ui-tests 249 passed. Capture:
      `specs/0033/.artifacts/001-local-verify.log`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Propagation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-project.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PropagationTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryProxyTests.fs`

## Testing state

All five suites pass locally (advisory run): build clean (0 errors,
`--warnaserror+:25` respected), unit-tests 84, constructor-unit-tests 306,
ui-smoke 54, ui-tests 249. 5 tests were ADDED (2 unknown-id typed-error,
seeded-samples-resolve, legacy-system-equality, expandedFilms) and none
removed, so every `count_at_least` baseline can only have grown. No blockers.

## Artifacts

- `specs/0033/.artifacts/001-local-verify.log` — captured build/test outcomes.

## Gotchas

- **A `vacuum` material entry was required but not enumerated by the slice.** The
  slice lists euv-molybdenum / euv-silicon / active-crystal as the missing built-ins,
  but the re-seeded quarter-wave stack's unit cell references vacuum SPACER FILMS
  (`SampleStructure.lower = None` only covers the half-space). With "unknown id is a
  typed Error, never a fallback", the QW sample cannot resolve without a library
  entry. Added built-in `vacuum` over `OpticalProperties.vacuum` with a new
  `MaterialCategory.Vacuum` case (a non-breaking DU addition — no exhaustive match
  over `MaterialCategory` exists anywhere; verified) and extended the schema's
  category enum. Alternative (miscategorising vacuum as Glass) was rejected as a
  data lie that would leak into the materials panel's Glass filter.
- **Schema file touched outside the slice's `touches` list.** One line
  (category enum) — required to keep the AC-I10 "export validates against the
  materialEntry $def" test green with the new Vacuum category.
- The schema JSON's WORKTREE copy was CRLF before this round (index was LF);
  normalized to LF per the repo's line-ending policy. `git diff --numstat` is
  identical with and without `--ignore-cr-at-eol` — no churn introduced.
- The langasite-on-silicon system's `description` changes from the old hard-coded
  "Langasite thin film on silicon substrate." to the sample name (all samples now
  uniformly carry `Some sample.name`). The acceptance compares film count /
  substrate / tensors; the legacy-equality test compares with descriptions dropped.
- The Details band view now shows the QW stack as glass ×20 / vacuum ×20 / glass ×1
  (the true `Repeated`-plus-trailing structure) instead of the old collapsed
  glass ×21 / vacuum ×20 summary — more truthful to the data; purely cosmetic.
- `MeasurementMode.capturesTransmitted/-Reflected : bool` members predate this slice;
  left untouched (out of scope).
- Operator note: the project prompt's "Operator note" section is present but empty —
  no operator constraints in flight this attempt.

# Reuse critique -- 010.slice-md cycle 3

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (repo root).
- Files inspected: ~16/200 — the four new Optimization sources
  (`Ellipsometry.fs`, `MeritFunction.fs`, `LocalRefinement.fs`, `InverseFit.fs`),
  the revised `LocalRefinementTests.fs`, the two edited `.fsproj`s, plus the
  slice-009 seams and prior reuse candidates re-checked for resolution
  (`OptimizationInterface.fs`, `AlglibAdapter.fs`, `DesignParameters.fs`,
  `Storage/SpectralImport.fs`, `Storage/MaterialImport.fs`, `Domain/Units.fs`,
  `Analytics/AnalysisFunctions.fs` `psiDelta`, `Fields.fs` `IncidentLightInfo`).
- Extensions: the task's `.py,.md,.json` bound is the generic template's; this is
  a pure-F# tree, so the evidence base is the `.fs`/`.fsproj` sources plus the
  slice MD/JSON inputs. No `max_files` cap was hit.

## Findings

### F1: `InverseFit` CSV scaffolding is now a third copy of the Storage import trio (broader extent of the judge-accepted cycle-1 duplication)

- **Worker added:** `InverseFit.fs:56–70` — `inv` (`CultureInfo.InvariantCulture`),
  `tryFloat` (invariant `Double.TryParse` → `float option`), and `unitFromHeader`
  (header substring → `Units.UnitOfMeasure`); plus `parseMeasurementCsv`
  (`InverseFit.fs:77–109`), whose body — `CsvFile.Parse`, header→unit selection,
  `csv.Rows |> Seq.choose` with a `cols.Length >= 2` guard, `tryFloat` on both
  columns, `List.ofSeq`, the `List.isEmpty → Error` check, and the outer
  `try/with → MalformedMeasurementCsv` wrapper — is structurally identical to the
  existing spectrum parser.
- **Existing helper:** `Storage/SpectralImport.fs:25–69` — `inv`/`tryFloat`/
  `unitFromHeader` (lines 25–38) and `parseSpectrumCsv` (lines 46–69), the same
  three private helpers and the same parse-loop skeleton. Those in turn already
  mirror `Storage/MaterialImport.fs:34–53` (`inv`/`tryFloat`/`unitFromHeader`), so
  with this slice the trio exists in **three** modules.
- **Why it matters:** the impl-log's accepted-duplication note (Gotchas; in-code
  note at `InverseFit.fs:46–55`) frames the duplication as "two ~4-line helpers."
  That undersells the actual extent: it is the three-helper trio *and* the
  ~25-line `parseMeasurementCsv` control structure, and the trio is now a 3-way
  copy across the import family. The `unitFromHeader` copies have also already
  drifted — `MaterialImport` defaults to `Micrometer` and recognises `nm` first,
  while `SpectralImport`/`InverseFit` default to `Nanometer`; none of the three
  recognises the `Millimeter`/`ElectronVolt`/`Wavenumber` cases that
  `Domain/Units.UnitOfMeasure` (`Units.fs:20–27`) already enumerates. That drift
  is exactly the maintenance cost duplicated header logic invites.
- **Suggested action:** advisory only, and I am **not** recommending a re-spawn.
  The consolidation blocker the judge weighed in cycle 1 is unchanged: a shared
  CSV seam would need either an `Optimization → Storage` project reference (which
  does not exist and would invert the dependency direction) or lifting the
  FSharp.Data row-scaffolding into a Domain-level module and editing
  `SpectralImport.fs`/`MaterialImport.fs`, none of which this Optimization-scoped
  slice owns. The defensible action is to **leave as-is** but (a) widen the
  in-code/Gotchas note so it names the full parse-body extent and the now-3-way
  copy rather than "two ~4-line helpers," and (b) record the header-unit drift as
  the shared, pre-existing gap so a future Storage-owning slice can extract a
  single `Domain`-level CSV/unit-header seam for all three importers. The judge
  decides whether the existing accept already covers this broader extent.

## Non-findings (checked, not raised)

- **`LocalRefinementTests.ssr` vs `AlglibAdapter.sumSq`.** The test's
  `ssr` (`LocalRefinementTests.fs:61`, `Array.sumBy (fun v -> v*v)`) computes the
  same sum-of-squares as `AlglibAdapter.sumSq` (`AlglibAdapter.fs:23`). But
  `sumSq` is `private` to the adapter and deliberately not part of the G.1
  surface, so there is no reachable helper to reuse; a one-line test-local
  sum-of-squares is trivial with no maintenance cost. Not a finding.
- **Cycle-2 F1 (the `InverseFit.MeasuredQuantity` DU) is resolved.** It is now a
  `type MeasuredQuantity = TargetQuantity` alias (`InverseFit.fs:39`) consumed
  directly into `FitTarget.quantity`, with the 1:1 translation `match` removed —
  exactly the suggested action. No re-statement remains.
- **Cycle-1 F2 (test fixtures hand-rolling the `IncidentLightInfo` literal) is
  resolved.** Fixtures use `IncidentLightInfo.create`/`createInclined`
  (`LocalRefinementTests.fs:55–59`).
- The load-bearing seams are reused correctly, not forked: `modelValue` reads
  `Solution.func` (the chart evaluator); `EllipsometricFunction.evaluate`
  delegates to `Analytics.AnalysisFunctions.psiDelta` (§F.5, slice 008) rather
  than re-deriving ρ = r_p/r_s; `LocalRefinement` builds on
  `DesignParameters.applyVector`/`bounds` and `AlglibAdapter.optimize`; wavelength
  reduction goes through the sole `Units.toWaveLength` seam; the engine's closed
  `OpticalFunction` and the Mueller path are untouched.

## Bottom line

One advisory finding this cycle (F1), and it is a re-framing rather than a new
duplication: the `InverseFit` CSV scaffolding is the same Storage-import
duplication the judge already accepted in cycle 1, but its true extent is the
full three-helper trio plus the ~25-line parse body — now a 3-way copy with
already-drifting `unitFromHeader` defaults — not the "two ~4-line helpers" the
note records. The cycle-1 (F2) and cycle-2 (F1) findings are both resolved and
the core fitting seams are reused correctly, so absent a Storage-owning slice to
host a shared seam I read this as a docs/scope note for the judge, not grounds
for a re-spawn; the routing call is the code judge's.

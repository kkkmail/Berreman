# Reuse critique -- 006.slice-md cycle 1

## Coverage

- Helper roots walked: `Berreman/OpticalConstructor/OpticalConstructor.Ui/` and
  `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/` (the only roots the
  diff touches), plus targeted reads of `Schematic.fs`, `ChartView.fs`,
  `Workspace.fs`, `SynthesisFitPage.fs`, `TestApp.fs`.
- Files inspected: ~14/200 (focused walk: the diff is two new files plus three
  one-line registrations; the catalogue centred on the sibling `*View.fs` and
  `*PanelTests.fs` modules the new code parallels).
- Extensions: `.fs` (the diff and every helper it could reuse are F#), `.json`,
  `.md`.

## Findings

### F1: Headless view-tree probe helpers re-copied verbatim into the new test file

- **Worker added:** `mount`, `textBlocks`, and `buttonByContent` in
  `OpticalConstructor.Ui.Tests/ResultsPanelTests.fs:33-48`.
- **Existing helper:** byte-for-byte identical definitions already exist in
  `SourcePanelTests.fs:33-48` (all three), with `mount`/`textBlocks` also in
  `MaterialsPanelTests.fs:34-47` and `buttonByContent` in
  `ChartPanelTests.fs:23-26`. The test project already carries a shared
  test-support module, `HeadlessSession` in `TestApp.fs:42-49`, which is the
  natural home for these probes — and which `ResultsPanelTests` already depends
  on via `HeadlessSession.run`.
- **Why it matters:** `mount` (window + `Component` + `Show` + `RunJobs`),
  `textBlocks` (visual-descendant `TextBlock.Text` projection), and
  `buttonByContent` (`GetVisualDescendants` |> find by `Content`) are now defined
  four times across the suite with this slice's copy being the fourth. They are
  pure visual-tree plumbing with no per-test specialisation; every future panel
  test re-copies them again. A drift in one copy (e.g. a `RunJobs` ordering fix)
  silently leaves the others stale.
- **Suggested action:** extract the three probes into the existing `TestApp.fs`
  (alongside `HeadlessSession`, e.g. a `ViewProbe`/`HeadlessProbe` module) and
  have `ResultsPanelTests` call them. Honest caveat for the judge: this is a
  *pre-existing* duplication pattern the worker followed consistently, not a new
  divergence — the worker matched the slice-002..005 precedent rather than
  inventing a worse one. The cleanup is a suite-wide refactor that arguably
  belongs to its own slice; routing the worker back solely for this would be
  proportionate only if the judge wants the extraction seeded now while the
  pattern is still small (four copies).

### F2: `lift` near-misses `SynthesisFitPage.comparisonOverlay`'s `mkInfo`

- **Worker added:** `lift : SourceSpec -> OpticalSystem -> FixedInfo` in
  `ResultsView.fs:162-163`
  (`{ incidentLightInfo = source.light; opticalSystem = system.dispersive }`).
- **Existing helper:** the private `mkInfo` closure in
  `SynthesisFitPage.fs:228-229`
  (`{ incidentLightInfo = t0.samplePoint; opticalSystem = system.dispersive }`).
- **Why it matters:** both lift an `OpticalSystem` into the `FixedInfo` the
  comparison overlay plots each system under, with the identical
  `opticalSystem = system.dispersive` half. The shape is the same; only the
  incident-light source differs.
- **Suggested action:** **leave as-is.** This is a near-miss, not a duplication
  worth routing back. `Workspace.renderOverlay` (`Workspace.fs:107-114`) is
  *designed* to take the `lift` as a parameter precisely because the source layer
  owns the incident light (`Workspace.fs:84-90`), so the view is obligated to
  supply one. The two lifts diverge on their incident-light input by design
  (`source.light`, the edited source, vs `t0.samplePoint`, a fit-target sample),
  and `mkInfo` is a private closure inside another function — not an exposed,
  reusable helper. There is no shared lift to call, and inventing one to unify a
  two-field record literal across two semantically distinct call sites would be
  over-abstraction the reuse rubric explicitly does not ask for. Recorded only so
  the judge has the near-miss on the table.

### F3: The bordered-host "section" pattern parallels `ChartView.chartRow`

- **Worker added:** the titled, fixed-height bordered host blocks in
  `ResultsView.fs` — `schematicSection` (`Border` wrapping `canvasHost`,
  lines 144-148) and `workspaceSection`
  (`Border.create [ Border.height 220.0; Border.child (overlayHost …) ]`,
  line 218).
- **Existing helper:** `ChartView.chartRow` (`ChartView.fs:163-172`), a
  "titled, fixed-height chart row" — `TextBlock` heading over a
  `Border.create [ Border.height 220.0; Border.child host ]`. The `220.0`
  fixed-height bordered host is the same constant and the same structure.
- **Why it matters:** the two panels render hosted plots with an identical
  layout idiom (heading + 220-high bordered host) but express it independently,
  so the "how a hosted-plot row looks" decision lives in two places.
- **Suggested action:** **leave as-is / document.** `chartRow` is `private` to
  `ChartView`, so it is not reachable without widening a sibling module's surface,
  and `ResultsView`'s sections are not uniform rows (the schematic section sizes
  its `Border` to the stack via `topPad`/`totalHeight`, lines 145-146, rather
  than a fixed 220). Promoting a shared `chartRow` is a design proposal, which the
  rubric puts out of scope for the reuse critic. Flagged for the judge's
  awareness only; no re-spawn is warranted on this alone.

## Bottom line

The diff is largely clean reuse: it correctly routes through the frozen
`Schematic.layout`/`rayGeometry`, `Workspace.update`/`renderOverlay`, and the
slice-002 `ChartHosts` adapters, recomputing no geometry and re-implementing no
reducer. The only substantive finding is F1 — the fourth verbatim copy of the
headless view-tree probes, for which a shared home (`TestApp.fs`) already exists —
and even that is the worker faithfully following an established suite-wide pattern
rather than introducing a new one. F2 and F3 are near-misses I recommend leaving
as-is. My read: not strong enough to compel a re-spawn; F1 is a worthwhile
test-helper consolidation the judge may either seed now or defer to a dedicated
cleanup, but the worker's choices are defensible as written.

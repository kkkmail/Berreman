# Reuse critique -- 016.slice-md cycle 2

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (focused on
  `Berreman/OpticalConstructor/OpticalConstructor.Ui`, `.Storage`, `.Domain`,
  `.Tests`; plus `Berreman/Berreman/Media.fs` for the `Thickness` DU).
- Files inspected: ~16/200 (the full cycle-2 `git diff HEAD` — `Templates.fs`,
  `Sidecar.fs`, `SynthesisFitPage.fs`, `SchematicGeometryTests.fs`, both fsproj —
  plus the three new Ui modules (`Help.fs`, `JobRunner.fs`, `SystemView3D.fs`) and
  the two new test files read in full; `Schematic.fs`, `CurvedElements.fs`,
  `StackEditor.fs`, `Validation.fs` read as the `Thickness`-fold evidence base).
- Extensions: the walk bounds list `.py,.md,.json`, but this is a pure-F# tree —
  the load-bearing helpers live in `.fs`, read directly (the bound is a cross-repo
  default that does not fit this project; the `.fs` sources are the real evidence
  base, as in cycle 1).

## Findings

This is the cycle-2 review of the worker's retry, which targeted the cycle-1
reuse findings. The body below verifies the disposition of each prior finding
against the current diff, then reports whether the consolidation introduced any
new duplication.

### F1 (cycle-1): `Help.fs` cloned `Templates.fs`'s project scaffold — RESOLVED

- **Cycle-1 diagnosis:** `Help.fs` re-implemented `air`/`glass`/`film`/
  `glassPlate`/`defaultLight`/`systemOf`/`projectOf` verbatim, with a divergent
  `defaultLight` (600 nm vs `Templates`' 550 nm).
- **Current state:** `Templates.fs:35-82` promotes those seven helpers from
  `private` to module-public (the diff flips `let private air`→`let air`, …,
  `let private systemOf`→`let systemOf`), and `Help.fs:120-154` now BUILDS every
  gallery sample through them — `Templates.film`, `Templates.glass`,
  `Templates.air`, `Templates.glassPlate`, `Templates.systemOf`,
  `Templates.projectOf`. No scaffold is duplicated in `Help.fs`; only the
  gallery-specific stacks, the `sourceScript` label, and `GalleryEntry` remain
  there. The 600/550 drift is gone: `defaultLight` is the single 550 nm value in
  `Templates.fs:60`, with a comment naming it the one shared default. The gallery
  open path still correctly reuses `Templates.loadTemplate` (`Help.fs:181`). This
  finding is fully discharged — the scaffold now lives in exactly one place.

### F2 (cycle-1): `JobRunner` sidecar policy diverged from the fit page — RESOLVED

- **Cycle-1 diagnosis:** `JobRunner` wrote derived `.binz` under a `.sidecars`
  subfolder via `Sidecar.extension`, while `SynthesisFitPage.fitHistorySidecarPath`
  wrote a flat `workingFolder` + hand-written `".fit-history.binz"` suffix — two
  conventions for the one §J.10-item-6 sidecar obligation.
- **Current state:** the location/extension policy is now a single seam in the
  Storage layer: `Sidecar.sidecarDirectory` and `Sidecar.derivedArtefactPath`
  (`Sidecar.fs:23-38`) decide the `.sidecars` subfolder and append the drift-proof
  `extension` once. `JobRunner.sidecarDirectory`/`derivedArtefactPath`
  (`JobRunner.fs:237-246`) delegate straight to it, and
  `SynthesisFitPage.fitHistorySidecarPath` (`SynthesisFitPage.fs:248-249`) now calls
  `Sidecar.derivedArtefactPath model.workingFolder (name + ".fit-history")` instead
  of hand-writing the `.binz` suffix. Both writers route through one seam; the
  flat-vs-subfolder choice and the extension are decided in a single place. Homing
  the seam in `Storage.Sidecar` (rather than in `JobRunner`, as cycle-1's first
  suggestion read) is the better placement here, since `SynthesisFitPage.fs`
  compiles before `JobRunner.fs` and the slice mandates `JobRunner` be registered
  last — the fit page cannot call `JobRunner`, but both can call Storage. This
  finding is fully discharged.

### F3 (cycle-1): `Thickness`→meters fold shared with `Schematic.fs` — declined; not a reuse finding

- **Cycle-1 diagnosis (lower-confidence):** `SystemView3D.systemThicknessMeters`'
  `match … Thickness.Thickness d -> d | Thickness.Infinity -> …` decomposition
  echoes `Schematic.bandHeight`'s, suggesting a shared `Thickness`→`float<meter>`
  helper.
- **Re-evaluation:** on the rubric ("reuse what exists", not "design what
  doesn't"), this does not qualify. There is no existing canonical `Thickness`→
  meters accessor to cite: `Media.fs` exposes the `Thickness` DU and constructors
  (`nm`/`mm`, `toInfinity`) but no meters-extracting fold. The two-case destructure
  is a pervasive idiom across the tree — `Schematic.fs:114-115`,
  `SystemView3D.fs:73-74`, `CurvedElements.fs:222-223`, `StackEditor.fs:212-213`,
  `Validation.fs:42-43`, and many test files each match it inline — and each site's
  `Infinity` branch carries DIFFERENT semantics (`Schematic` maps it to a fixed
  semi-infinite band height plus a `minFiniteBandHeight` offset; `SystemView3D`
  maps it to `0.0<meter>` and sums finite films; `CurvedElements` maps it back to
  `Infinity`). Collapsing these would mean inventing a new helper, which the rubric
  forbids the critic from proposing. The worker left F3 as-is with a documented
  rationale (state-of-the-world "Architecture" / cycle-2 changelog: "two distinct
  render surfaces with intentionally different scale constants"), which is the
  defensible disposition. No action.

### New duplication introduced by the cycle-2 retry?

None. The retry is a pure reuse-consolidation diff (promote `Templates` helpers,
add the `Sidecar` seam, route two callers through it, plus the AC-J12 test
additions). The new `JobRunner.sidecarDirectory`/`derivedArtefactPath` are one-line
delegations to `Storage.Sidecar` — that is reuse via a thin Ui-facing facade, not
duplication of an existing helper, so it is not a finding (at most a simplification
nit outside this critic's remit). The AC-J12 additions in `SchematicGeometryTests.fs`
build an inline `glass`/`sampleSystem`/`sampleNode` fixture; that matches the
established per-file inline-fixture convention used by `CurvedElementsTests`,
`EnvironmentRoundTripTests`, `RoundTripTests`, and `ProjectJsonRoundtripTests` — no
shared test-fixtures module exists to reuse, so the inline fixture is consistent,
not divergent. The tests otherwise reuse the real engine primitives
(`OpticalSystemSolver`, `BeamTree.solve`, `Help.gallery`, the `Sidecar` seam through
`JobRunner`) rather than re-implementing them.

## Bottom line

Both substantive cycle-1 findings are fully resolved: `Help.fs` now reuses the
single module-public `Templates` scaffold (drift eliminated, one 550 nm default),
and the sidecar-location policy is a single `Storage.Sidecar` seam both `JobRunner`
and `SynthesisFitPage` delegate to. F3 was lower-confidence and, on the
reuse-what-exists rubric, is not a valid finding — no canonical `Thickness`→meters
helper exists to reuse, and the worker documented the deliberate per-surface
divergence. My read: the diff is clean from a reuse standpoint with no new
duplication; routing is the code judge's call.

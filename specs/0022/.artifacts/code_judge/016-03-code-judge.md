# Code judge -- 016.slice-md cycle 2

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0022\.slices\016.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0022\.slices\016-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0022\.slices\016-impl-log.md`
- Gate results: build = pass, unit-tests = pass, constructor-unit-tests = pass
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0022\.artifacts\architecture_critic\016-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0022\.artifacts\reuse_critic\016-02-reuse-critic.md`
- Direct code reads (to adjudicate the retry): `JobRunner.fs`, `JobRunnerCancelTests.fs`,
  `HelpGalleryTests.fs`, `SchematicGeometryTests.fs:110-203`, `Help.fs` (open/glossary
  seams), `Sidecar.fs:20-44`, `git diff HEAD --stat`.

## Rationale

This is cycle 2 of the LAST slice of the arc. Cycle 1 routed back on reuse finding
F1 (verbatim duplication of the `Templates` project scaffold inside `Help.fs`, with a
silent 600-vs-550 nm `defaultLight` drift) with F2 (divergent sidecar-location policy)
as a secondary ask. Both findings are now resolved, both critics independently
recommend shipping, and I verified the resolutions against the working tree rather
than trusting the SoW.

**F1:** `Templates.fs` promotes the seven scaffold helpers (`air`/`glass`/`film`/
`glassPlate`/`defaultLight`/`systemOf`/`projectOf`) from `private` to module-public,
and `Help.fs:120-154` builds every gallery sample through them; the forked block is
gone and the gallery inherits the single 550 nm `defaultLight` transitively via
`Templates.projectOf`, so the drift is eliminated structurally, not papered over.
**F2:** the location/extension policy is homed in `Storage.Sidecar`
(`Sidecar.fs:30-37` — `sidecarDirectory`/`derivedArtefactPath`), and BOTH
`JobRunner.derivedArtefactPath` (`JobRunner.fs:245-246`) and
`SynthesisFitPage.fitHistorySidecarPath` (`SynthesisFitPage.fs:248-249`) delegate to
it. Homing the seam in Storage rather than `JobRunner` is the correct call given
compile order (the fit page compiles before `JobRunner`, which the slice mandates be
registered last); both Ui callers reach downward into Storage, so no new project edge
appears. **F3** (the `Thickness`→meters destructure shared with `Schematic.fs`) is
reasonably declined: no canonical accessor exists to reuse, the two-case match is a
pervasive inline idiom whose `Infinity` branch carries different per-surface
semantics, and inventing a shared helper is outside the reuse rubric.

The slice's three acceptance criteria are met and each load-bearing invariant is
exercised by a test in the diff. **AC-J10:** `JobRunnerCancelTests` proves
`sweepTotalPoints = length + 1` (engine-driven granularity), strictly increasing
completed-over-total progress, a cancel observed between points stopping within one
inter-point check (`CancelledWith` carrying exactly the partial), `committedResults`
surviving a cancel while completion refreshes them, the Start↔Cancel two-state button,
the indeterminate-but-running state, and sidecar-only artefact paths asserted never to
be the repo root. **AC-J11:** `HelpGalleryTests` confirms `Help.openEntry` routes each
sample through `Templates.loadTemplate` (the validate-on-load `ProjectFile.openProject`
path, §A.7), the five named example cases are present as design-reference labels (no
`.fsx` executed), and the eV/cm⁻¹ conversions call `Domain.Units` (`photonEnergyEv`/
`wavenumberPerCm` are thin wrappers over `Units.toMeters`/`fromMeters`), so no second
conversion is introduced. **AC-J12:** the `SchematicGeometryTests` SystemView3D portion
proves `beamDirection`/`beamSegments` take the already-solved `EmFieldSystem` as input
and read each segment direction from `EmField.normal` — the no-re-solve invariant is
structural, not asserted — and that `placeElements` writes no value back into the model
while placing elements to scale in beam-path order. The clone stays unreferenced (the
two grep hits are documentation comments), honouring the §A.6/§A.9 reservation, and no
new C# project is added.

The residual notes are advisory and non-blocking. The architecture critic flags
`JobRunner.sidecarDirectory`/`derivedArtefactPath` as harmless pass-through aliases
(defensible as local discoverability), and carries over that `startBackground` (a
trivial `Async.Start` exception→`RunFailed` wrapper) and the pure `progressText`
formatter are untested. Neither is new externally-observable behavior that an AC-J10
test omits — the progress, cancel, button-toggle, and sidecar commitments are all
covered — so the `done-green` test-coverage criterion is satisfied. The disclosed
fit-history path move (`workingFolder/<name>.fit-history.binz` →
`.sidecars/<name>.fit-history.binz`) is behaviour-preserving for the gates (`*.binz`
gitignored, no test asserted the flat path, still under the working folder). The SoW
and impl-log line up with the tree. All gates pass. With both critics recommending
ship and no unmet slice requirement remaining, this lands as `done-green` and closes
the arc.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "Cycle-2 retry resolves both cycle-1 reuse findings at the correct seams: F1's verbatim Templates scaffold duplication is gone (helpers promoted to module-public, Help.fs reuses them, the 600-vs-550 nm defaultLight drift eliminated to a single 550 nm value reached transitively via Templates.projectOf), and F2's sidecar-location policy is unified behind one Storage.Sidecar seam (Sidecar.fs:30-37 sidecarDirectory/derivedArtefactPath) that both JobRunner.derivedArtefactPath and SynthesisFitPage.fitHistorySidecarPath delegate to, homed in the layer that already owns the .binz IO per compile-order constraints. F3 reasonably declined (no canonical Thickness->meters accessor to reuse; per-surface Infinity semantics differ). All three gates pass (build, unit-tests 84, constructor-unit-tests 204). AC-J10/J11/J12 are each exercised: JobRunnerCancelTests proves engine-driven progress granularity, cooperative cancel-within-one-check leaving committedResults intact, the Start-Cancel toggle, indeterminate state, and repo-root-safe sidecar paths; HelpGalleryTests proves the gallery opens through the schema-validated Templates.loadTemplate path with eV/cm-1 conversions reusing Domain.Units and no .fsx executed; SchematicGeometryTests proves SystemView3D consumes the already-solved EmFieldSystem (no re-solve, structural) and writes no non-SI value back. Clone stays unreferenced; no new C# project. Residual notes (pass-through aliases, untested trivial startBackground/progressText, disclosed gitignored fit-history path move) are advisory and non-blocking. Both critics recommend ship.", "retry_hint": ""}
```

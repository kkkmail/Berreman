# Step 0253 — impl log

## Progress

1. Read the two system prompts, the project prompt, the slice and its `.gates` snapshot, AGENTS.md,
   `BeamLayout.fs`, `BeamLayoutTests.fs`, `TableAndElementRotationView.fs:captureDetectors` /
   `availableCaptures`, and slice 0254 (the consumer of this rule). No operator note is in flight; manual 016
   binds steps 0252, 0254 and 0259–0261, not 0253.
2. Wrote `0253-impl-plan.md`.
3. **Domain rule** (`OpticalConstructor.Domain/BeamLayout.fs`, after `detectedBranches`):
   - `DetectorCapture = { detector : Library.ElementId; branch : BeamTree.BeamBranch }`.
   - `detectorCaptures scene detectors varied objects : DetectorCapture list`. It walks `scene.elements` (scene
     order). A detector counts when it is in `detectors`, LIT, and its path holds `varied` (the filter
     `detectedBranches` applies). It captures the branch of the LAST path step whose element is in `objects`.
     With no object on its path, it captures `TransmittedBeam` when its path leaves `varied` by T, and nothing
     otherwise (the no-sample rule of `availableCaptures`).
   - Each doc is one to three lines and cites spec 0050 S8.1. `detectedBranches` and every UI caller are
     unchanged (step 254 re-points them). No other type or function is added.
4. **Tests** (`OpticalConstructor.Tests/BeamLayoutTests.fs`, two new facts, over `el`, `tilted`,
   `onReflectedOf`, `placedOf` and `layout`; ids bound once at origin):
   - `each detector captures the branch its light leaves the last object by`: src; `s1` (R2 30°, both branches,
     x 0); `s2` (R2 30°, both, x 0.5) behind it; `dT` (x 1.0) behind `s2`; `dR2` on `s2`'s reflected ray;
     `dR1` on `s1`'s reflected ray. Varied `src`, objects [`s1`; `s2`] → [`dT` T; `dR2` R; `dR1` R];
     objects [`s1`] → [`dT` T; `dR2` T; `dR1` R].
   - `a detector whose path holds no object captures only light the varied element transmits`: src; a flat
     mirror `m` (R2 45°); `d` on its reflected main beam; `dUnlit` behind `d` (asserted `Unlit`). No objects:
     varied `src` → [`d` T]; varied `m` → empty; `dUnlit` never appears.
5. Development runs (below), red by mutation, restore, final runs.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/BeamLayout.fs` (+33)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/BeamLayoutTests.fs` (+42)

Both LF: `git diff --numstat` equals `git diff --numstat --ignore-cr-at-eol`.

## Testing state

Development runs only (Invariant 6: the gate engine gives the verdict).

- **build (first):** exit 0, `0 Error(s)`, `2 Warning(s)` — the vendored MathNet C#'s pre-existing `SYSLIB0051`
  pair, printed only when that project recompiles (recorded since step 0001; outside `touches`).
  `0253-build.log`.
- **constructor-unit-tests (first):** `Passed!`, 1403 succeeded of 1403, 68 s. `0253-constructor-unit-tests.log`.
- **Red by mutation:** `List.tryLast` → `List.tryHead` (first object instead of last), and the no-object fallback
  → `Some variedStep.branch` (no T filter). Test-project build 0 warnings; `BeamLayoutTests` 23 total, 2 failed
  (exactly the two new facts), 21 succeeded. `0253-red-mutation-build.log`, `0253-red-mutation-tests.log`.
  The source was restored from a byte copy, then the solution rebuilt.
- **build (final):** exit 0, `0 Warning(s)`, `0 Error(s)`, no lowercase `error` in the log.
  `0253-build-final.log`.
- **constructor-unit-tests (final, the gate's command):** `Passed!`, 1403 succeeded, 0 failed, 0 skipped, 68 s
  (estimate 180 s): checkpoint 252's 1401 plus the two declared facts. `0253-constructor-unit-tests-final.log`.
- **elevation (the gate's command; no base, so all 12 projects):** `ELEV-GATE: OK - 0 new finding(s)`, known
  findings 0, 203 s (estimate 600 s). `0253-elevation.log`.
- **The fourteen s25 invariants,** by their greps: all find nothing. `0253-invariants.log`.

## Artifacts

Under `specs/0050/.artifacts/`:

- `0253-build.log`, `0253-build-final.log`
- `0253-constructor-unit-tests.log`, `0253-constructor-unit-tests-final.log`
- `0253-red-mutation-build.log`, `0253-red-mutation-tests.log`
- `0253-elevation.log`
- `0253-invariants.log`

## Gotchas

- **"With one object on every path this is today's rule"** holds only where every captured path holds the
  object. A detector whose path holds `varied` but not the single object now captures T when `varied` transmits
  to it (the no-object rule), where `detectedBranches` dropped it. Implemented as the slice states;
  `detectedBranches` is untouched here, so no current caller changes behaviour.
- **The new record's `detector` / `branch` labels** can capture an un-annotated `x.branch` lookup in a file that
  opens `BeamLayout` before `x`'s type is inferred. Every existing `.branch` site is piped or typed; the solution
  builds with no change outside this step's two files.
- **Mirror geometry in the no-object fact:** the mirror is tilted 45° (as in the file's existing mirror fact), so
  the detector "behind it" sits on its reflected main beam; an untilted mirror would send the beam back at the
  source.

# Step 0390 — impl plan

## Approach

Stage 2 (and a joint fit of the same shape) becomes `StagedStudy.reconstructLinear`, the reference linear path driven by
the study's own run law.

1. **Projection by the run law.** Each fitted run is bound once at the layout's start vector (`assignmentFor` +
   `boundWith`, its session's dark else the detector's offset). Each capture row is placed on its scan
   (`measuredRowsFor`). The steps before the run's unknown segment are evaluated at the row's own orientation and
   wavelength and fold the emission (`detectorStokes`). The steps after it give the detector's S₀ row
   (`RunStep.productOf`). The segment's rotation is φ. `effectiveSource` / `effectiveAnalyzer` and `correctSignal`
   then give (s, a, corrected intensity) through the projection `projectCaptureRow` is built on.
2. **Session gains.** A free session uses the held detector gain (`ParameterValue.airGain`). An anchored or
   interpolated session builds each AIR run's `AirAnchorGain` from that run's own projected rows. This uses the scalar
   gain arithmetic factored out of `airAnchorGainOf`, then `SessionGainModel.ruleWith` and `gainAt` at each row's time.
3. **P and k from the study's own assignment.** An object's sixteen coefficients, in vec order, are an affine image of
   the fitted vector. The image at the zero vector is k, and its change at each unit vector is P's column. Links,
   owners and fixed leaders therefore follow `assignmentFor` exactly, and no second layout rule is written.
4. **Groups and systems.** Runs are grouped by their unknown-bearing items in light order (first-run order).
   - Single-item groups that share a column merge into one reduced system, with columns in layout order.
   - A lone item with all sixteen free goes through `reconstruct solver`. Any other system is solved as
     `A·P·x = b − A·k` through `solver.solveLinearLeastSquares`.
   - A series group (two or more items) is reconstructed whole by `reconstruct` as its combined matrix, a diagnostic.
5. **Answer.**
   - One `ObjectMatrixFit` per group, through `MuellerMatrix.rowMajorValues`.
   - Single groups fill only their own entries.
   - One converged member, whose χ² is Σ rmse² × group rows.
   - Two new errors, `SessionGainFitErr` and `ObjectSolveFitErr`.

## Files

- `OpticalConstructor.Optimization/StudyEngine.fs` — `ParameterValue.airGain`, `MuellerMatrix.rowMajorValues`,
  `StagedStudy.reconstructLinear`, the two errors, shared private helpers (capture rows, fitted followers, bearers).
- `OpticalConstructor.Tests/StudyEngineTests.fs` — the three new facts and the widened describe fact.
- Required deviations, recorded in the impl-log:
  - `OpticalConstructor.Optimization/StagedMuellerPath.fs`: `airAnchorGainOf` and `projectCaptureRow` are factored into
    `airAnchorGainOver` / `projectCaptureRowFrom`. The brief says "factor and call that scalar gain calculation" and
    "projected by `projectCaptureRow`" without an LP/CPL source model.
  - `OpticalConstructor.Domain/StudyForward.fs`: `readingAt`'s per-row wavelength and orientation rules are moved into
    `BoundExperiment.waveLengthAt` / `orientationAt`, which `readingAt` calls. The brief says the projection "MUST
    come from the same … per-row dial/orientation/wavelength rules used by the run law", and AGENTS.md forbids a copy.

## Risks

- The shipped circular polarizer is LP→retarder in light order, so as an analyzer it does not see S₃. The brief's
  four-family fixture would reach only rank 12. The "CPL" analyzer families therefore put a fixed quarter-wave plate
  ahead of CPL2, recorded as a fixture deviation.
- The constructor suite runs at about 103 s against a 180 s deadline, so the fixture must stay cheap.
- ELEV: no primitives in private module signatures, and unique names.

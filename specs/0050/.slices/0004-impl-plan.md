# Step 0004 — ADD_RED — impl plan

## Approach

Add one failing-first fact, `equal Tauc-Lorentz and Gaussian-oscillator models lower to equal
axis data`, to `OpticalConstructor.Tests.DispersionModelsTests`. Touch only that test file. Write
no production code.

- The fact reuses the Tauc-Lorentz and Gaussian-oscillator coefficient sets of
  `AC-B5 transcendental models lower to the evaluated case and match evaluate`. AGENTS.md says
  "Reuse, never copy" and treats a re-transcribed constant as a defect, so both sets are hoisted
  into two private builders, `taucLorentzCoefficients ()` and `gaussianOscillatorCoefficients ()`.
  Each call builds a fresh record. The AC-B5 fact calls the builders where it wrote the literals;
  its substance does not change.
- A private helper, `assertLowersToEqualData`, runs the checks for one model kind, in this order:
  1. `model.toEpsAxis` twice: the axes are equal and so are their hashes. On the start tree this
     is the first equality that fails.
  2. Two separately built equal models (`build ()` called twice): their axes are equal and so are
     their hashes.
  3. `toEpsValue`, of one model twice and of the two equal models: the values are equal.
  4. A model with a shifted `bandGap` (Tauc-Lorentz) or `energy` (Gaussian) lowers to an unequal
     axis.
- Why it is red: `toEpsAxis` lowers both models to `EpsAxisEvaluated this.evaluate`.
  `evaluate` (`DispersionModels.fs:571-574`) returns a new lambda on every call, and
  `EpsAxisDispersion.Equals` compares the evaluated case with `PhysicalEquality` (`Dispersion.fs`).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`: the only
  file edited.

## Risks

- The fact must not name a case that does not exist yet (`TaucLorentzAxis` and
  `GaussianOscillatorAxis` arrive in step 5). It compares only through `=` and `hash`, so it
  compiles on the start tree.
- `Assert.True(a = b, …)` could parse as a named argument, so each comparison is parenthesised.

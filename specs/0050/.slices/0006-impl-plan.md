# Step 0006 — ADD_RED — impl plan

## Approach

Add one failing-first fact, `equal Forouhi-Bloomer and Brendel-Bormann models lower to equal
axis data`, to `OpticalConstructor.Tests.DispersionModelsTests`. Touch only that test file. Write
no production code.

- The fact reuses step 4's helper `assertLowersToEqualData`. That helper lowers one model twice,
  lowers a separately built twin, compares `toEpsValue`, and checks that a shifted model lowers
  to an unequal axis. It is exactly the check this slice asks for, so it is called, not copied.
- The helper takes a `unit -> DispersionModel` builder, because a twin must be a separate
  allocation down to the coefficient record. `forouhiBloomerASi` and `brendelBormannGold` are
  values today, so their literals move into two private builders, as step 4 did for its sets:
  - `forouhiBloomerASiCoefficients ()`
  - `brendelBormannGoldCoefficients ()`

  Each call builds a fresh record. The two values stay under their names, built from the
  builders, so the AC-B6 facts (and step 7's citations by name) are unchanged.
- The shifted copies:
  - Forouhi-Bloomer: `bandGap` 0.906 → 1.0.
  - Brendel-Bormann: the first `broadening` 0.742 → 0.8, through `List.updateAt 0`, so the
    other four values are not re-transcribed.
- Why it is red: `toEpsAxis` lowers both models to `EpsAxisEvaluated this.evaluate`
  (`DispersionModels.fs:787-788`). `evaluate` (`:607-610`) answers a new lambda on every call,
  and `EpsAxisDispersion.Equals` compares the evaluated case with `PhysicalEquality`
  (`Dispersion.fs:549`). The first assertion, one model lowered twice, therefore fails first.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`: the only
  file edited.

## Risks

- The fact must not name a case that does not exist yet (`ForouhiBloomerAxis` and
  `BrendelBormannAxis` arrive in step 7). It compares only through `=` and `hash`, so it compiles
  on the start tree.
- The slice's line citations (`:403-430`, `Dispersion.fs:386`) have drifted. The names locate
  the code.

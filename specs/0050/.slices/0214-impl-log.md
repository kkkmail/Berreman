# Step 0214 — impl log

## Progress

- [x] (1) `Kit.fs`: appended `ParameterInterval`, `ParameterSigma`, `ParameterPrior`, `UnknownParameterData`,
  `FitRunName`, `CalibratedParameterData`, `ParameterState` and `Parameter` (`knownAtDefault`, `validate`) after
  `ItemModelInstance`, in the brief's order
- [x] (2) `KitItemTests.fs`: the two declared facts appended
- [x] build, constructor-unit-tests, elevation and the fourteen invariants (development runs), plus a mutation check

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs` (+113 lines, appended at the end of the module):
  - `ParameterInterval = { lower : ParameterValue; upper : ParameterValue }`;
  - `ParameterSigma of double` with `.value`;
  - `ParameterPrior = { centre : ParameterValue; sigma : ParameterSigma }`;
  - `UnknownParameterData = { start : ParameterValue; bounds : ParameterInterval; priorOpt : ParameterPrior option }`;
  - `FitRunName of string` with `.value`;
  - `CalibratedParameterData = { value : ParameterValue; sigma : ParameterSigma; fitRun : FitRunName }`;
  - `ParameterState` = `KnownParameter of ParameterValue` | `UnknownParameter of UnknownParameterData` |
    `CalibratedParameter of CalibratedParameterData`;
  - `Parameter = { name : ParameterName; state : ParameterState }`, with
    `static member knownAtDefault (axis : ItemParameterAxis) : Parameter` and
    `member this.validate : Result<Parameter, ItemError>`.

  Each type carries a one-line doc comment citing spec 0050 S7.1. Nothing else changed in the file: no new
  `open` was needed, because `ItemModels` and `System.Globalization` are already open.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs` (+71 lines): the two declared facts,
  appended after `every item error renders its condition and its payload`. The file is already in the test
  project, so the fsproj is untouched.

## Decisions

1. **`validate` answers the first broken rule only, as `InvalidParameterStateErr "<name>: <rule>"`.** The brief
   asks for the error to name "the parameter and the broken rule", in the singular. The rules are checked in a
   fixed order per state:
   - finiteness first (known value; start, lower, upper; prior centre and sigma; calibrated value and sigma);
   - then `lower < upper` (strict, so [1, 1] is refused);
   - then `lower <= start <= upper` (ends included);
   - then sigma `> 0`.

   Finiteness leads, so a NaN reports as itself and never as a misplaced bound. The one comment inside
   `validate` says why.
2. **The prior's rules follow the interval's.** An unknown with a prior reports a broken interval before a broken
   prior. Both are reported; only the order is a choice.
3. **Numbers render as `ToString("R", CultureInfo.InvariantCulture)`.** This gives `NaN`, `Infinity`, `-0.5`, `0`
   and `1`. The reason text is the same on every machine culture, and the test pins it exactly.
4. **No range by kind is checked.** The brief says a kind's range is its model's (S8.1). So, for example, a
   `FractionParameter` known at 2 is valid here.
5. **The record holds the same-typed pairs** (`lower` / `upper`, `value` / `sigma`), as the brief requires (ELEV011).
   The field names are the brief's, `value` in `CalibratedParameterData` included.
6. **The tests check slightly more than the brief lists, inside the two declared identities:**
   - the knownAtDefault fact finds the `retardance` axis in the published list (it does not take the head of the
     list) and compares the whole `Parameter` record;
   - the validate fact compares each refused reason with the exact text `retardance: <rule>`, so it pins both the
     parameter's name and the rule;
   - it adds one refused case the brief's list lacks: an unknown whose upper bound is +∞. This exercises the
     "nor infinite" half of the finiteness rule.

   No new test identity was added.

## Testing state

These are development runs; the gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release`, from `Berreman/`): exit 0, 0 errors.
  - The first run showed 2 `SYSLIB0051` warnings from the vendored `MathNetNumerics/Numerics/Exceptions.cs`. They
    appear whenever that project recompiles (earlier steps' "red" build logs show the same) and are not in this
    step's files.
  - The final build after the mutation check: 0 warnings, 0 errors (`0214-dev-build-final.log`).
- **KitItemTests:** 6 / 6 passed (the four from step 213 and the two new facts).
- **constructor-unit-tests**, whole suite: 1294 passed, 0 failed, 0 skipped, in 1 min 04 s against a 180 s estimate.
  The count rose from 1292 by 2, as the acceptance requires.
- **Mutation check.** Each mutation was rebuilt and `KitItemTests` re-run, then the source was restored and the
  restored diff checked:
  - `lower < upper` → `lower <= upper`: the validate fact fails;
  - `lower <= start` → `lower < start`: the validate fact fails;
  - `knownAtDefault` → `KnownParameter (ParameterValue 0.0)`: the knownAtDefault fact fails.
- **elevation** (`ASG_ARC_BASE_COMMIT` = the `.arc-base-json` origin `f395597`, ELEV004 excluded):
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 193s`.
- **The fourteen `invariant-s22-*` gates:** each `git grep` finds nothing.
- **LF endings:** neither edited file has a CR byte, and `--numstat` equals `--ignore-cr-at-eol --numstat`.

## Artifacts

All in `specs/0050/.artifacts/`:

- `0214-dev-build.log`: the first build;
- `0214-dev-build-final.log`: the build after the mutation check, over the restored source;
- `0214-dev-constructor-unit-tests.log`: the whole-suite run;
- `0214-dev-mutation-build.log`: the last mutation's test-project build;
- `0214-dev-elevation.log`: the ELEV gate run.

## Gotchas

- **Record labels repeat across types:**
  - `sigma` is a field of both `ParameterPrior` and `CalibratedParameterData`;
  - `name` is a field of `Parameter`, `KitFile` and `ItemParameterAxis`;
  - `value` is a field of `CalibratedParameterData`.

  The tests annotate every record value with its type. Later code should do the same.
- **`ParameterValue -0.5` must be written `ParameterValue (-0.5)`** in an application.
- **`grep -c $'\r'` in this Git Bash counted every line as a CR line.** Count CR bytes with `tr -cd '\r' | wc -c`
  instead.

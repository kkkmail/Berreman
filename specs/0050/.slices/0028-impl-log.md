# Step 0028 — IMPLEMENT — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice, its gates snapshot, step 23's slice
  (the converter form to follow), step 27's outputs, the S14-WIRE-TAGS oracle row and .spec-md §4.
- The four sets and their `value` / `tryOfWire` are already in the tree:
  - `LibraryEntryKind`: `OpticalConstructor.Domain/LibraryEntryFiles.fs:23`, tags 1–6;
  - `PublishedIndexLaw`: `Berreman/Dispersion.fs:469`, tags 1–3;
  - `TabulatedInterpolation`: `Berreman/Dispersion.fs:576`, tag 1;
  - `PublishedGyrationLaw`: `Berreman/Dispersion.fs:901`, tag 1.
- `git grep` found none of the 11 case names in a committed `.json`, in a schema or in a JSON literal in a
  test. No stored form changes under anyone, and none of the four is a `Map` key.
- **Red first.** I added the two facts to `CanonicalJsonTests` under the exact slice ids and built. Both
  failed for the right reason (`.artifacts/0028-red-run.log`):
  - (a) `Expected: Number, Actual: String`: each case was still written as its case name;
  - (b) `Assert.Throws() Failure: No exception was thrown`: FSharp.SystemTextJson accepted `9` as a
    `LibraryEntryKind` without complaint.
- **Production.** `OpticalConstructor.Storage/CanonicalJson.fs` gained one private generic
  `ClosedSetTagConverter<'Tag>(tagOf, tryOfWire)`, a `JsonConverter<'Tag>` placed beside step 23's.
  - `Write` writes `tagOf value` as a JSON number.
  - `Read` accepts only a `Number` token whose `TryGetInt32` succeeds and whose int `tryOfWire` knows.
    Anything else throws `JsonException "'<token>' is not a <Type> tag"`: an unknown int, a fraction, a
    string such as the old case name, or any other token.
  - `options` registers four instances (`LibraryEntryKind`, `TabulatedInterpolation`, `PublishedIndexLaw`,
    `PublishedGyrationLaw`), each with its `value` lambda and its `tryOfWire`. They come after the four
    boolean / beam-branch converters and before the `JsonFSharpConverter`.
  - The options' doc names the int-tagged sets, and the file gains `open Berreman.Dispersion`.
- **Green.** The class ran 10/10: the 8 facts from steps 22, 23 and 27, plus the 2 new ones.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs` (+31 / −1): `open
  Berreman.Dispersion`, the `ClosedSetTagConverter<'Tag>` class, four registrations and two doc lines.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs` (+40 / −2):
  - three `open`s: `Microsoft.FSharp.Reflection`, `Berreman.Dispersion`,
    `OpticalConstructor.Domain.LibraryEntryFiles`;
  - the module doc, widened to int tags;
  - two private helpers: `everyCaseOf`, which reads every case off the union type by reflection, and
    `assertWrittenAsTag`;
  - facts (a) and (b).

  No `.fsproj` change: both files were already in their compile lists.

## Testing state

The arc-runner's gate engine is the gate authority. These are local observations only, made so the round
ends on code that compiles and passes:

- `build` command (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0,
  `0 Warning(s)`, `0 Error(s)`, and no lowercase `error` in the output (`.artifacts/0028-build.log`).
- The constructor suite, whole (the gate's command without a selector): `Test run summary: Passed!`, total
  761, succeeded 761, failed 0, skipped 0. That is step 27's 759 plus the two new facts
  (`.artifacts/0028-constructor-unit-tests-whole.log`).
- The ELEV script (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`) reports
  `ELEV-GATE: OK - 0 new finding(s) in 5 project(s), 130s`, with 0 known findings
  (`.artifacts/0028-elevation-local.log`).
  - Storage was expanded to its dependents: App, TestWindows.App, TestWindows and Ui.
  - The test file is out of the analyzers' scope.
  - The private converter's `int` constructor parameters raise nothing.
- The six s04 invariants, run as their `git grep` commands, each find nothing (exit 1).
- Line endings: both source files and both round notes have zero CR bytes, and `git diff --numstat` equals
  `--ignore-cr-at-eol` (31/1 and 40/2).

Every requirement of the slice landed in this round:
- the four converters, each writing `value` and reading through `tryOfWire`;
- the `JsonException` on an unknown tag;
- no other union changes form;
- tests (a) and (b).

## Artifacts

- `.artifacts/0028-red-run.log`: the two new facts, red before the converter.
- `.artifacts/0028-build.log`
- `.artifacts/0028-constructor-unit-tests-whole.log`
- `.artifacts/0028-elevation-local.log`
- `.artifacts/0028-elevation-unexcluded.log`: the same analyzer run without `-ExcludeRules`, to name the
  suppressed findings (see Gotchas).

## Gotchas

- **One generic class, four converters.** The slice asks for "four converters, written as step 23's are".
  Four hand-written classes would repeat one body four times, which AGENTS.md "Reuse, never copy" calls a
  defect. So there is one private generic `JsonConverter<'Tag>` class, as step 23's are, and the options
  register four instances of it. A new int-tagged set is now one registration line.
- **FSharp.SystemTextJson does not refuse a number on its own.** Before this step, `9` read as a
  `LibraryEntryKind` raised nothing (red run). The refusal now lives in the converter, through `tryOfWire`.
- **The old case-name form is refused, not read.** Fact (b) also reads `0` (the unset sentinel) and
  `"MaterialEntryKind"` as a `LibraryEntryKind`, and both throw. No up-converter is needed: no stored file
  holds these sets by name (see Progress), and AGENTS.md "No legacy, no migration" rules one out anyway.
- **`TryGetInt32` through the reader byref.** The out parameter comes back as an F# tuple and needs no
  local `mutable`. The raw token text for the message is bound to a local before the interpolated string,
  so no byref-like span sits inside an interpolation hole.
- **ELEV004, measured and not gated.** Storage's suppressed count went from 38 (step 27) to 40. I ran the
  analyzers once without `-ExcludeRules` (`.artifacts/0028-elevation-unexcluded.log`) to see why. The two
  new findings are both ELEV004, at `CanonicalJson.fs:195` and `:197`: the `.value` lambdas of the two
  single-case sets, `TabulatedInterpolation` and `PublishedGyrationLaw`. The multi-case sets' `.value` is
  not flagged. That `.value` is the one true serialization seam AGENTS.md reserves for it, and ELEV004 is
  excluded in the gate command (spec 0048 `.manual/002` R7). The unexcluded run's `FAILED - 524` is
  exactly the tree's ELEV004 total: the 524 the gated run suppresses.
- **Building the test project alone** (not the `.slnx`) shows two `SYSLIB0051` warnings from the vendored
  `MathNetNumerics/Numerics`. The solution build that the gate runs shows 0 warnings. This is unrelated to
  this step.
- `specs/0050/.manifest.state.json` was already modified when the round started. It is the supervisor's
  file, and this round leaves it alone.

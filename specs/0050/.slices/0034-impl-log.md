# Step 0034 — ADD_RED — impl log

## Progress

- [x] Read the protocol, the ADD_RED role, the project prompt, AGENTS.md, the slice and its `.gates` snapshot, oracle row
  `S15-BUILTIN-PARITY`, 001 §B, `.spec-md` §3.5 and §3.6, steps 33 and 35, and the code the facts call.
- [x] Authored `BuiltInCatalogueParityTests.fs` with the slice's five facts and added its `Compile` line after
  `CatalogueFolderTests.fs`.
- [x] Built the test project and observed all five facts red for the named reason (see Testing state).

## Files modified

| File | Change |
|---|---|
| `Berreman/OpticalConstructor/OpticalConstructor.Tests/BuiltInCatalogueParityTests.fs` | new: `module BuiltInCatalogueParityTests`, five facts |
| `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` | one `Compile` line, no comment |

No production file changed. These are exactly the slice's authorized edits, plus this round's plan, log and SoW.

## Testing state

These are local observations made to record the red. They are not gate results, and the gate engine is the gate authority.
`gates_run` and `gate_results` are empty. `commit_ready: true` means every requirement of the slice is addressed; it does not
mean any gate passed.

**Run:**

- `dotnet build OpticalConstructor.Tests.fsproj -c Release -nologo -v:m`: exit 0, `0 Error(s)`, `2 Warning(s)`. Both warnings
  are the vendored `MathNetNumerics/Numerics/Exceptions.cs` `SYSLIB0051` ones step 0019 already recorded.
- `dotnet test --no-build -c Release --filter-method "OpticalConstructor.Tests.BuiltInCatalogueParityTests.*" --filter-method
  "OpticalConstructor.Tests.BuiltInCatalogueParityTests" --no-ansi`: exit 2, `total: 5`, `failed: 5`, `succeeded: 0`,
  `skipped: 0`.

Actual output, with the stack frames below the first trimmed. All five facts carry the same reason (5 of 5 lines match in the
log), and every stack passes through `readBuiltIn` at `BuiltInCatalogueParityTests.fs:42`:

```
failed OpticalConstructor.Tests.BuiltInCatalogueParityTests.every built-in material and category in the catalogue equals its seed (172ms)
  Xunit.MicrosoftTestingPlatform.XunitException: System.Exception : expected Ok, got CatalogueIoErr
    "catalogue folder 'C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Tests\bin\x64\Release\net10.0\Catalogue\Builtin': Could not find a part of the path 'C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Tests\bin\x64\Release\net10.0\Catalogue\Builtin\.catalogue-json'."
    at ...BuiltInCatalogueParityTests.okOf[Value,Problem](FSharpResult`2 result) in ...\BuiltInCatalogueParityTests.fs:21
    at ...BuiltInCatalogueParityTests.readBuiltIn() in ...\BuiltInCatalogueParityTests.fs:42
failed OpticalConstructor.Tests.BuiltInCatalogueParityTests.every built-in source, detector and polarizer in the catalogue equals its seed (1ms)
  (the same reason)
failed OpticalConstructor.Tests.BuiltInCatalogueParityTests.the built-in catalogue is the Built-in catalogue at version 1 (1ms)
  (the same reason)
failed OpticalConstructor.Tests.BuiltInCatalogueParityTests.every built-in sample in the catalogue equals its seed (65ms)
  (the same reason)
failed OpticalConstructor.Tests.BuiltInCatalogueParityTests.the built-in catalogue is exactly what the seeds seal to, and its manifest hash is stable (0ms)
  (the same reason)
Test run summary: Failed!
  total: 5
  failed: 5
  succeeded: 0
  skipped: 0
```

**Not run:**

- the `build` gate's solution build (only the test project and what it references were built);
- the `elevation` gate (the change is test-only, and the test projects are outside the ELEV scope, so it should report that no
  in-scope project was touched);
- the six s05 invariant commands;
- the rest of `constructor-unit-tests`, and every other suite.

**The green path has never executed, because no catalogue exists yet.** That the facts can go green at step 35 is reasoned, not
measured:

- `every built-in seals, writes, reads and decodes equal to its seed` (`EntryFileJsonTests.fs:439`) round-trips every seed;
- `a catalogue folder written and read back is equal` (`CatalogueFolderTests.fs:96`) covers the write and read of a folder;
- both `CatalogueData.tryCreate` and the folder read order the envelopes by manifest row;
- the three file names fact (a) requires are in step 35's effects list, and the 61 seeds counted from the seed lists (12
  materials, 5 categories, 11 samples, 28 sources, 2 detectors, 3 polarizers) match the 61 entry files listed there.

Step 35's effects list creates no tests, so a defect in these facts would surface there as a conflict with its authority.

**Measured, as edit hygiene:**

- both files have 0 CR bytes and no BOM;
- `git diff --numstat` equals `--ignore-cr-at-eol` for the fsproj: 1 line added, 0 removed;
- the five `[<Fact>]` names equal the slice's five test ids;
- `git status` shows no production file changed;
- `Catalogue/` does not exist in the test output after the run;
- a grep of the new `.fs` for all six s05 invariant patterns finds nothing, and the fsproj's only change is the `Compile` line.

## Artifacts

In `specs/0050/.artifacts/`, which is git-ignored, so these logs stay local and the output above is the durable record:

- `0034-build.log`: the test project's build.
- `0034-constructor-unit-tests-red.log`: the class run, five reds with full stacks.

## Gotchas

- **The ADD_RED role contradicts itself on local runs, and `.manual/` holds no ruling.** It orders "observe them red" and
  "record the actual failing output ... not a claim that it failed". It also says "you run no checks" and "never run,
  self-report, or green-light a gate". AGENTS.md separately says to run the relevant tests after every change.
  - **Reading chosen:** "gate" means the gate engine's run and its verdict. Building the test project and running this one
    class is the observation the role orders, so that alone was run, and nothing is reported as a gate result. This is what the
    earlier ADD_RED steps 0019 and 0022 did, except that this round ran no whole suite, no elevation and no invariant command.
  - **Cost of the reading:** it departs from the literal "no checks". Step 0033 read that sentence literally and ran nothing,
    so the two readings now coexist in this arc. An operator ruling would settle it.
- **A green probe was deliberately not run.** Step 0022 ran one, but no obligation here orders it, so it would have stretched
  the reading above. It would also have needed a throw-away exporter and a catalogue placed where the facts read.
- **`readBuiltIn ()` is the first statement of every fact,** so the red is the missing catalogue and never a fixture fault.
  `expectedCatalogue ()` is a function for the same reason as `CatalogueFolderTests`' fixtures: a failure sinks its fact and
  not the assembly's start-up.
- **The comparison must be by id.** A catalogue's rows sort by kind tag and then ordinally by file name, while the seeds keep
  display order (`standardCategories` starts Glass, Metal; the catalogue's categories start Crystal, Glass). A positional
  comparison could never go green.
- **`assertDecodesToSeeds` compares whole records,** which covers "equal in complexity and metadata" (id, name, category,
  description, complexity) without listing the fields.
- **`okOf` is a fifth private copy of the same small helper** in this test project. The authorized edits allow no shared test
  source, and `CatalogueFolderTests`' copy is private, so it follows the sibling's form rather than touching another file.
- **The slice's `Library.SeedSamples.allSeedSamples` is reached as `SeedSamples.allSeedSamples`** under `open
  OpticalConstructor.Domain.Library`, the form the sibling files use.
- **A stale file in an output will read as `UnlistedEntryFileErr` once the catalogue is copied.** `PreserveNewest` never deletes,
  and the read refuses an entry file no row names, so an entry file later renamed in `Catalogue/Builtin/` stays in an existing
  `bin` until that output is cleaned. This follows from the code as read; nothing here exercises it.

# Step 0456 — impl log

## Progress

1. Read the protocol and its IMPLEMENT delta, the project prompt and AGENTS.md, rulings R0001–R0005 (`.manual/030`,
   `031`, `033`, `035`, `036`, each "Binding ruling") and the slice.

   None of the rulings names this step's files. Each is preserved by leaving its files untouched (StudyEngine,
   ItemEvaluation, StudyForward, TableAndElementRotationView, FittedProblems, BerremanMatrix, Fields, the two server-GC
   project files).
2. Wrote the impl-plan and this log's skeleton.
3. Read the ELEV analyzers' sources (STL `Analyzers/`, read-only) for ELEV001, ELEV003, ELEV006, ELEV009, ELEV010 and
   ELEV011. Two signatures in the brief would break ELEV010 (see Decisions).
4. **Domain.** Wrote `OpticalConstructor.Domain/HenkeCompounds.fs`, in the brief's order:
   - (1) `HenkeCompoundError`;
   - (2) `AtomicNumber`, `ElementSymbol`, `AtomicWeight` and `ChemicalElement` (`all`, `tryOfSymbol`);
   - (3) `AtomCount`, `FormulaCount` and `ChemicalFormula` (`tryCreate`, `text`, `molarMass`);
   - (4) `CompoundDensity`;
   - (5) `CompoundSpec` (`recordId`, `entryName`, `tryOfRecord`, `tryOfQuery`);
   - (6) `AtomicScatteringFactors`;
   - (7) the private `[<Literal>]` r₀ and N_A, then `type CompoundSpec with member this.indexTableWith`.

   Added one `<Compile>` line to `OpticalConstructor.Domain.fsproj`, after `MaterialSources.fs`, with no comment.
5. **Tests.** Wrote `OpticalConstructor.Tests/HenkeCompoundsTests.fs` with the six facts. Its `<Compile>` line is
   appended at the end of the test compile list, with no comment.
6. **Build.** Green on the first attempt.
7. **Development checks.** Ran the filtered class, the whole constructor suite, elevation and the 21 invariants (see
   Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/HenkeCompounds.fs` (new): the Henke compound physics.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one `<Compile>` line.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/HenkeCompoundsTests.fs` (new): the six facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line.

## Decisions

The first three are deviations from the brief's letter. Each is forced by the compiler or the analyzers, and each is
the reading most consistent with the brief.

- **Compile position: directly after `MaterialSources.fs`, not directly before it.**
  - `tryOfQuery` takes `MaterialSourceQuery`, which `MaterialSources.fs` declares (step 443). F# cannot name a type
    from a file compiled later.
  - `MaterialSources.fs` is outside this step's edits, so the query type cannot move.
  - The file now sits between `MaterialSources.fs` and `CatalogueImport.fs`. Nothing in `MaterialSources.fs` refers
    to `HenkeCompounds`, here or in steps 457–459 as the spec writes them.
- **`ChemicalElement.tryOfSymbol` takes an `ElementSymbol`, not a `string`.**
  - ELEV010 (`NoPrimitiveBesideElevated`) also applies to members. It sanctions only the elevation-pattern names
    (`value`, `tryCreate`, `create`, `ofText`, `tryOfWire`, `tryOfDb`, … in STL's `ElevationAnalyzer.fs`).
  - `tryOfSymbol : string -> ChemicalElement option` would mix `string` with `ChemicalElement` and fail the gate. The
    known-findings list must stay empty.
  - The name is kept, and the symbol is elevated where it is read (the parser, the tests).
- **The formula reader is `ChemicalFormula.tryCreate (text : string)`, not `tryParse`.**
  - It has the same ELEV010 cause: `tryParse : string -> Result<ChemicalFormula, _>`.
  - `tryCreate` is AGENTS.md's name for a value parsed from external text, and ELEV010 sanctions it.
  - Its behaviour is exactly the brief's `tryParse`. No later step (457–459) calls either name.
- **Opens are exactly the brief's three** (`Berreman.Dispersion`, `Units`, `LibraryEntryFiles`).
  - `MaterialLibrary.EntryName` and `MaterialSources.MaterialSourceQuery` are written qualified.
  - `System` names (`Double`, `CultureInfo`, `NumberStyles`, `Math.PI`, `StringSplitOptions`) are written in full.
- **r₀ and N_A are private `[<Literal>]` values.** A private module-level `let` of type `float` is ELEV003P; a literal
  is sanctioned. They sit just before the `type CompoundSpec with` that uses them, step (7).
- **`tryOfQuery` reads its two words as a record.** It builds `<formula>@<density>` and calls `tryOfRecord`, so one
  parser reads both forms.
  - A word count other than two is `InvalidCompoundQueryErr` with the brief's advice.
  - A density that does not read is also `InvalidCompoundQueryErr` with that advice, in both forms.
  - A read density that `CompoundDensity.tryCreate` refuses is `InvalidDensityErr`.
  - A formula error passes through as itself (`InvalidFormulaErr` / `UnknownElementErr`).
  - A record without exactly one `@` is `InvalidCompoundQueryErr`.
- **The scanner reads ASCII only.**
  - A symbol is an upper-case `A`–`Z`, plus a following `a`–`z` when there is one. A count is digits, optionally `.`
    and digits.
  - Any other character, an accented letter included, is `InvalidFormulaErr` naming it and its 1-based position.
  - A zero count, or one that overflows to infinity, is `InvalidFormulaErr` naming the count.
- **`indexTableWith` re-checks each table through `AtomicScatteringFactors.tryCreate`.** This reuses the one validation
  rather than copying it.
  - A hand-built record (its fields are public) that is empty, uneven or decreasing answers
    `InvalidScatteringFactorsErr` naming the element. It never throws an index exception.
  - An empty formula gives an empty grid, which `TabulatedNkTable.tryCreate` refuses, so it answers `CompoundTableErr`.
- **Interpolation.** An exact energy reads the element's own first row at that energy. Otherwise it is linear between
  the last row below and the first row above. The grid lies inside each table's range, so the clamped fallbacks are
  never reached; they keep the function total.
- **The element table** is CIAAW 2021's abridged standard atomic weights, written as CIAAW abridges them (`Li` 6.94,
  `S` 32.06, `Mo` 95.95, `Pb` 207.2). Tc, Pm, Po, At, Rn, Fr, Ra and Ac carry the bracketed mass numbers the brief
  lists. Each of the 92 records is written once, with its Z explicit, so the fact's "1 to 92 in order" is a real check.

## Testing state

These are development checks only. The gate engine runs the roster after this session and is the authority. Every
figure is from the final source. Two text-only edits (the `ElementSymbol` doc and the `NoCommonEnergyErr` reason)
came after the first round of checks, so build, class, suite, elevation and invariants were all re-run.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - the first full build: exit 0 in 86 s, 0 errors;
  - the final incremental build, which recompiled Domain and every test project: exit 0 in 19 s, against a 600 s
    deadline.
  - The warnings are only the exempt NU1902 (12). The first full build also recompiled the vendored MathNet C# and
    showed its existing SYSLIB0051 (see Gotchas). There is no FS or MSB warning.
  - Log: `.artifacts/0456-dev-build.log` (the final build).
- **`OpticalConstructor.Tests.HenkeCompoundsTests`, filtered:** `Passed!`, 6 of 6, in 1 s. The TRX's six names equal
  the census's six identities exactly. Log: `.artifacts/0456-dev-henke-class.log`.
- **constructor-unit-tests, the whole suite as the gate selects it** (`all_discovered`, the gate's own command):
  `Passed!`, 1814 of 1814, in 112 s against a 180 s deadline. That is checkpoint 1808 plus the six new facts, the
  "count rising by 6" the acceptance asks for. Log: `.artifacts/0456-dev-constructor-unit-tests.log`.
- **elevation, all 13 projects** (no comparison base standalone):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 230s [excluded rules: ELEV004; ... known findings: 0]`, against a
  600 s deadline. Domain reports `findings=0`. Log: `.artifacts/0456-dev-elevation.log`.
- **The 21 s44 invariants:** each decoded `git grep` finds nothing (exit 1), run both as written and with
  `--untracked`. Log: `.artifacts/0456-dev-invariants.log`.
- **LF:** the numstat equals the CR-ignoring numstat. The new files and this step's markdown hold no CR byte, and no
  file starts with a BOM.
- No development run exceeded its gate's effective deadline.

## Artifacts

All of these are under `specs/0050/.artifacts/`:

- `0456-dev-build.log`
- `0456-dev-henke-class.log`
- `0456-dev-constructor-unit-tests.log`
- `0456-dev-elevation.log`
- `0456-dev-invariants.log`

## Gotchas

- **The brief's worked value "Si's 11.5 at 150 eV" contradicts its own rule.** Si's test table has f1 = 10 at 100 eV
  and 12 at 200 eV, so linear-in-E gives 10 + (12 − 10) · ½ = 11 at 150 eV, and f2 = 1.5. The fact follows the rule:
  its hand-worked row at 150 eV is f1 11, f2 1.5. Its other hand-worked values are:
  - O at 200 eV: 6.5 (the brief's value), f2 2.5;
  - Si at 250 eV: 12.5, f2 0.75;
  - O at 300 eV: 7 + 0.5/3, f2 2 − 1/3.
- **ELEV010 is not member-exempt.** ELEV003 exempts members, but ELEV010 does not: any non-pattern member whose
  signature mixes a bare primitive with one of our types fails the gate. Only the pattern names are sanctioned (see
  Decisions).
- **A full rebuild warns SYSLIB0051** from the vendored MathNet C# (`MathNetNumerics/Numerics/Exceptions.cs`). That
  warning predates this arc's late steps (it appears in `.artifacts/0001-build-tests-project.log` and others), and an
  incremental build does not recompile MathNet. Nothing here touches MathNet.
- **`ChemicalFormula.text` writes counts in G6.** A count of 10⁶ or more, or below 10⁻⁴, renders in exponent form
  (`1E+06`), which the scanner does not read back. The brief fixes G6, and real formulas never reach those counts.

# Step 0456 — impl plan (Henke compound physics, spec 0050 S13.2, obligation S132-HENKE-PHYSICS)

## Approach

1. **New `OpticalConstructor.Domain/HenkeCompounds.fs`**, `module HenkeCompounds`, pure data and functions, in the
   slice's order:
   - `HenkeCompoundError`: eight `*Err` cases, each `of reason : string`, with `describe`.
   - `AtomicNumber`, `ElementSymbol` and `AtomicWeight`; `ChemicalElement` and its 92-element table (H to U, CIAAW 2021
     abridged weights, bracketed mass numbers for the eight elements without a standard weight); `tryOfSymbol`.
   - `AtomCount`, `FormulaCount` and `ChemicalFormula`, with a strict scanner over symbol and count groups, merging
     repeats at the first position; `text` (G6, invariant) and `molarMass`.
   - `CompoundDensity` with `tryCreate`.
   - `CompoundSpec`, with `recordId`, `entryName`, `tryOfRecord` and `tryOfQuery`.
   - `AtomicScatteringFactors`, a `NumericCarrier`, with `tryCreate`.
   - `CompoundSpec.indexTableWith`:
     - the common energy range and the merged grid;
     - per-element linear interpolation in E;
     - N_q = count × ρ × 10⁶ × N_A / M, λ through `Units.toMeters ElectronVolt`;
     - n = 1 − δ, k = β, rows by ascending λ through `TabulatedNkTable.tryCreate`.

   r₀ and N_A are private `[<Literal>]` values, which ELEV003P sanctions.
2. **One `<Compile>` line** in `OpticalConstructor.Domain.fsproj`, no comment.
3. **New `OpticalConstructor.Tests/HenkeCompoundsTests.fs`**, `module HenkeCompoundsTests`, holding the six facts the
   slice names. It is appended at the end of the test compile list.

## Risks and the deviations they force

- **Compile position.** `tryOfQuery` takes `MaterialSources.MaterialSourceQuery`, which `MaterialSources.fs` declares.
  A file compiled *before* `MaterialSources.fs` cannot name it, and `MaterialSources.fs` is outside this step's edits.
  So `HenkeCompounds.fs` compiles directly **after** `MaterialSources.fs`, before `CatalogueImport.fs`. Nothing in
  `MaterialSources.fs` refers to it, now or in steps 457–459.
- **ELEV010** (`NoPrimitiveBesideElevated`) applies to members too. It sanctions only the elevation-pattern names
  (`tryCreate`, `create`, `ofText`, `tryOfWire`, …). Two signatures as the brief writes them would mix a primitive
  with an elevated type:
  - `ChemicalElement.tryOfSymbol (symbol : string) : ChemicalElement option`;
  - `ChemicalFormula.tryParse (text : string) : Result<ChemicalFormula, HenkeCompoundError>`.

  The tree is ELEV-clean and the known-findings list must stay empty, so:
  - `tryOfSymbol` keeps its name and takes an `ElementSymbol` (elevated at origin);
  - the formula's text reader is `ChemicalFormula.tryCreate (text : string)`. That is the doctrine's own name for
    parsing a value from external text (AGENTS.md), and ELEV010 sanctions it.

  No later step (457–459) calls either member.
- **Opens.** The brief lists `Berreman.Dispersion`, `Units` and `LibraryEntryFiles`. `EntryName` (in `MaterialLibrary`)
  and `MaterialSourceQuery` (in `MaterialSources`) are named qualified, so the open list stays as written.

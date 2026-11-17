# Where we are

Step 0456 is the physics of S13.2's Henke compound calculator (spec 0050, obligation S132-HENKE-PHYSICS):

- steps 454 and 455 gave PTB OCDB its reader and its adapter;
- this step adds the pure Domain module that turns a chemical formula, a density and each element's CXRO atomic
  scattering factors into a `TabulatedNkTable`, n = 1 − δ and k = β;
- step 457 builds the `MaterialSourceProxy.createHenkeCompounds` adapter on top of it (`ChemicalElement.all`,
  `AtomicScatteringFactors.tryCreate`, `CompoundSpec.tryOfQuery` / `tryOfRecord` / `recordId` / `entryName`,
  `indexTableWith`).

# What's working

- Added `OpticalConstructor.Domain/HenkeCompounds.fs`:
  - `HenkeCompoundError` (eight `*Err` cases with `describe`);
  - the 92-element table, H to U, at CIAAW 2021 weights;
  - `ChemicalFormula` (a strict symbol/count reader that merges repeats, `text`, `molarMass`);
  - `CompoundDensity`;
  - `CompoundSpec`, with its record id, entry name and query/record readers;
  - `AtomicScatteringFactors`.
- `CompoundSpec.indexTableWith` computes n = 1 − δ, k = β (δ, β = r₀ λ² Σ N_q f1/f2 / 2π):
  - over every element's energy inside their common range;
  - each element's factors linear in E;
  - rows by ascending λ, with no uncertainty.
- Added `HenkeCompoundsTests`'s six facts. constructor-unit-tests went from 1808 to 1814.

# Tests

These are development checks. The gate engine runs the roster after this session and is the authority. Every figure
is from the final source.

- **build:** exit 0, 0 errors; the final incremental build took 19 s, against a 600 s deadline. The warnings are only
  the exempt NU1902; there is no FS or MSB warning.
- **constructor-unit-tests, the whole suite as the gate selects it** (`all_discovered`): `Passed!`, 1814 of 1814, in
  112 s against a 180 s deadline.
  - That is checkpoint 1808 plus the six new facts, the "count rising by 6" the acceptance asks for.
  - Filtered first, `HenkeCompoundsTests` passed 6 of 6, and the TRX names equal the census's six identities:
    - `the element table holds the 92 elements from hydrogen to uranium with their standard atomic weights`;
    - `a chemical formula reads element symbols and counts, merging repeats, and refuses anything else`;
    - `a compound query and record name a formula and a density, and anything else is refused`;
    - `a compound's index is one minus delta plus i beta over its elements' common energies`;
    - `a compound lacking one element's factors, or a common energy, is refused`;
    - `every Henke compound error renders its condition and its payload`.
- **elevation, all 13 projects:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 230s`, against a 600 s
  deadline, with known findings 0.
- **The 21 s44 invariants:** each decoded `git grep` finds nothing, run both as written and with `--untracked`.
- **LF:** the numstat equals the CR-ignoring numstat, and no new file holds a CR byte.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1814
```

# Architecture

- **Pure Domain physics, no IO.** The element table is code because it holds physical constants, not a library entry
  (.spec-md §3.3 concerns library seeds). The factors come in as a `Map<ElementSymbol, AtomicScatteringFactors>`;
  step 457's adapter reads them from its snapshot's `.nff` files.
- **One conversion seam.** λ is `Units.toMeters ElectronVolt (UnitMagnitude E)`, so no photon-energy constant is
  re-transcribed. r₀ (CODATA 2018) and N_A (exact) are private `[<Literal>]`s, each written once.
- **One parser for both forms.** `tryOfQuery` turns its two words into the record `<formula>@<density>` and calls
  `tryOfRecord`. `recordId` and `entryName` write the formula's `text` and the density in G6, invariant culture.
- **Compile order:** `HenkeCompounds.fs` compiles directly after `MaterialSources.fs`, because `tryOfQuery` names its
  `MaterialSourceQuery`.
- **Elevation shapes:**
  - `AtomicScatteringFactors` is a `NumericCarrier`;
  - every scalar is a single-case union (`AtomicNumber`, `ElementSymbol`, `AtomicWeight`, `AtomCount`,
    `CompoundDensity`);
  - every member that reads raw text or numbers is an elevation-pattern `tryCreate`, or takes an elevated argument.

# Deferred

Nothing.

# Gotchas

- **Two names differ from the brief because ELEV010 also applies to members.** Only elevation-pattern names may mix
  a primitive with one of our types, and the tree must stay ELEV-clean:
  - `ChemicalElement.tryOfSymbol` takes an `ElementSymbol`, not a `string`;
  - the formula reader is `ChemicalFormula.tryCreate (text : string)`, not `tryParse`.

  No later step calls either name.
- **The compile position differs from the brief:** directly after `MaterialSources.fs`, not before it, since F#
  cannot name a type from a later file.
- **The brief's worked "Si's 11.5 at 150 eV" contradicts its own linear-in-E rule.** With f1 10 at 100 eV and 12 at
  200 eV, the rule gives 11 at 150 eV. The fact follows the rule, with f1 11 and f2 1.5.
- **A full rebuild shows SYSLIB0051** from the vendored MathNet C#. That warning predates this step, and an
  incremental build does not recompile MathNet.
- **`ChemicalFormula.text` uses G6.** A count of 10⁶ or more, or below 10⁻⁴, would render in exponent form, which the
  reader does not read back.

# Changelog

- 2026-11-17, step 0456: added `HenkeCompounds` with the following:
  - the 92-element table;
  - formula, density, query and record readers;
  - `AtomicScatteringFactors`;
  - `CompoundSpec.indexTableWith`, which computes n = 1 − δ, k = β over the elements' common energies.

  Added `HenkeCompoundsTests`'s six facts; constructor-unit-tests went from 1808 to 1814.

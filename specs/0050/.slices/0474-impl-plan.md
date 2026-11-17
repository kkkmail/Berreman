# Step 0474 — impl-plan

## Approach

A pure MOVE (spec 0050 .spec-md §4, §7; S14.1; S141-BIAXIAL-DESIGN-MOVE), the biaxial twin of step 0473's quartz move.
`BiaxialInverseTests`' measurement design leaves the class and becomes `BerremanTestSupport.BiaxialDesign`, beside its
ground truth `BiaxialSample`, so step 475's Planner facts read the normal-incidence and full designs instead of
transcribing them.

1. New `Berreman/BerremanTestSupport/BiaxialDesign.fs`, declared as `BiaxialSample.fs` and `QuartzSample.fs` are:
   `namespace BerremanTestSupport`, then `module BiaxialDesign =`. The fully qualified name is
   `BerremanTestSupport.BiaxialDesign`.
   - The module doc is one to three lines: spec 0044's biaxial measurement design, shared by `BiaxialInverseTests` and
     the Planner's facts (spec 0050 S14.1).
   - It opens only what the moved bodies read:
     - `Berreman.Constants` (the `mkm` / `deg` units);
     - `Berreman.Media` (`Thickness`);
     - `OpticalConstructor.Domain.MuellerInverse` (`SampleCut`, `Observable`, `MeasurementConfiguration`);
     - `OpticalConstructor.Optimization.MuellerFitting` (`configurationOf`);
     - `BerremanTestSupport.BiaxialSample` (`referenceWaveLength`).
   - It reads no `BerremanTests` name.
   - It receives the measurement-set banner, then `plate`, `configuration`, `principalCuts`, `b1`, `b2`, `b3` and
     `fullConfigurations`, with their docs. The bodies are copied by line range.
   - It also receives the ablation fact's local `normalOn`, as `normalIncidenceOn (cut : SampleCut) :
     MeasurementConfiguration list`. Its body is unchanged and it gets a one-line doc. It sits between `principalCuts`
     and `b1`.
   - Two edits are required:
     - `configuration` passes `referenceWaveLength` where it passed the class alias `waveLength`. That is the same value.
     - `b1` becomes `principalCuts |> List.collect normalIncidenceOn`. It yields the same twelve configurations in the
       same order (cut by cut, then azimuth by azimuth).
2. `BerremanTestSupport.fsproj`: append `<Compile Include="BiaxialDesign.fs" />` at the end of the compile list (after
   `QuartzSample.fs`, so after `BiaxialSample.fs`), with no comment. No reference is added.
3. `BiaxialInverseTests.fs`:
   - delete the banner and the seven moved declarations (lines 122–195);
   - add `open BerremanTestSupport.BiaxialDesign` after `open BerremanTestSupport.BiaxialSample`, the last `open`;
   - in the ablation fact, delete `normalOn` and build `zOnly`, `zAndX` and `everyCutNormal` from `normalIncidenceOn`;
   - keep `degree`, `forward`, `waveLength`, `observe`, `perturbedStart`, `box`, `blindStart`, `wideBox`,
     `recoverFromNoisy`, the other noisy helpers and every fact.
   - The three comments in the class that pointed at the moved banner ("the thickness commentary above", "the
     measurement set above") are re-pointed at `BiaxialDesign`, so they do not dangle. No other comment changes.

## Risks

- **Shadowing.** The class-level `let`s shadowed every earlier `open`. Opening `BiaxialDesign` LAST reproduces that
  resolution.
- **Bit-identity.**
  - `b1`'s rewrite keeps the order: `List.collect` over `principalCuts` concatenates each cut's four azimuths, which is
    the nested comprehension's order.
  - `configuration` reads the same `WaveLength` value.
- **ELEV.** The `BerremanTestSupport` assembly sits outside `inScopePatterns` (`^Softellect\.Berreman\.`), so
  `configuration`'s `double` parameters stay unchanged (as at step 0473).
- **LF line endings** on every file.

## Verification (development only; the gate engine is the authority)

- build (`dotnet build Berreman.slnx -c Release`);
- elevation (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`);
- the 21 s45 invariants' `git grep`s, with `--untracked`;
- unit-tests filtered to `BerremanTests.BiaxialInverseTests` (about 997 s).
- kit-acceptance (`KitAcceptanceTests`) reads none of the moved names and only recompiles against the changed
  `BerremanTestSupport`. It is left to the gate engine, as at step 0473.

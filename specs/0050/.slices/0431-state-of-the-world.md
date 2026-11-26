# Where we are

Step 0431 moves S12.2's recovery-acceptance definitions into shared test support,
so AT-2 (step 0436) and `AbsorbingBiaxialNoisyTests` judge a recovery with one
function and use one seed list. `BerremanTestSupport` now holds the eight shared
seeds, the noisy suite's five seeds and nineteen reported quantities, the typed
band breach and its total band check, and a read-only locator for the
absorbing-biaxial-noisy kit. Implementation is complete; the supervisor has not
yet run the gates.

# What's working

- Added the read-only `AbsorbingBiaxialNoisyKit` locator, which answers None while `data/absorbing-biaxial-noisy` is absent.
- Moved `ensembleSeeds` unchanged to `BiaxialSample`; its four uses now name it in full.
- Moved the noisy suite's five seeds and nineteen reported quantities into `AbsorbingBiaxialSample`.
- Added `RecoveryBandBreach` and total `absorbingBandBreaches`, keeping the original bands and strict comparisons.
- `AbsorbingBiaxialNoisyTests` now checks its bands through the shared function; every other assertion and number is unchanged.

# Tests

No build, test, analyzer or invariant gate was run in this round: IMPLEMENT
Invariant 6 leaves gate execution and verdicts to the supervisor after the worker
exits. The final JSON reports empty gate results and `commit_ready=false` only
because no gate was run locally, as in steps 0428-0430. No requirement or
authority is blocked.

The 0431 roster is unchanged: build; elevation from the immutable arc origin;
kit-acceptance selecting `OpticalConstructor.Ui.Tests.KitAcceptanceTests`;
unit-tests selecting `AbsorbingBiaxialNoisyTests`, `BiaxialInverseTests`,
`MuellerInverseTests` and `MuellerReconstructionTests`; and the twenty-one s42
invariants. No test is added, removed or renamed, so no count change is expected.
No development run took place, so no deadline was exceeded.

The counts below are STATIC source counts, not measured runs. The four selected
BerremanTests classes declare 1 + 11 + 20 + 26 = 58 facts, and
`KitAcceptanceTests` declares 6 facts with `Category=kit-acceptance`. None of
these files declares a theory or a runtime skip. The supervisor's checkpoints stay
authoritative. No checkpoint, floor, census or gate descriptor was authored or
modified.

```yaml
gates:
  berreman_unit_tests: 58
  kit_acceptance_tests: 6
```

# Architecture

All seven source edits are among the slice's authorized effects, and no project
file changes: `BerremanTestSupport` already references Optimization and compiles
`BiaxialSample` before `AbsorbingBiaxialSample`. `CommittedKit.tryFindFolder`
still walks upward from `AppContext.BaseDirectory` to the folder holding
`Berreman/Berreman.slnx`, answers None while the kit folder is absent, and
creates nothing.

`ensembleSeeds` is declared once, last in `BiaxialSample`. `MuellerInverseTests`
and `BiaxialInverseTests` name it as `BerremanTestSupport.BiaxialSample.ensembleSeeds`
and open nothing new. In `AbsorbingBiaxialSample`, after `searchBox`, come
`noisySeeds`, `absorbingReportedQuantities`, `RecoveryBandBreach` and
`absorbingBandBreaches`, in that order. The file opens `ItemModels`, `BenchNoise`
and `RecoveryEnsembles`; none of them shadows a name the file already read. The old
declarations and the inline band loop are deleted in the same change.

`absorbingBandBreaches` asks the caller for each quantity's spread once, in report
order. It accepts only `spread.value < band.value` on the same doubles as before,
so equality and NaN are breaches. A quantity with no band is a breach with
`bandOpt = None` rather than a throw, which makes the function total. The function
holds no `Assert` and no test-framework reference. The noisy fact passes each
statistics row's `scatter` through `rowFor` and makes one `Assert.True` that the
list is empty. Its message is the breaches' `describe` texts joined by `; `, then
`: ` and the unchanged report.

# Deferred

Nothing is deferred. The next lifecycle actions are the supervisor's: review, gate
execution and the measured checkpoint.

# Gotchas

- The first worker on this attempt hit its backend quota after writing the edits.
  This retry reviewed and kept them, rewriting only two doc comments.
- `BerremanTestSupport` is in the elevation run, but its assembly name does not
  match `inScopePatterns` (`^Softellect\.Berreman\.`), which is why its existing
  primitive-bearing helpers report no finding.
- The band check now reports every out-of-band quantity in one message, where the
  old loop failed at the first.
- Leave R0001's series dispatch, R0002's analyzer traversal and R0003's element
  identities as they are; none of their files is touched here.
- `.specrc` and `.manifest.state.json` were already dirty (operator and supervisor
  files) and are untouched.

# Changelog

- 2026-11-15: Moved the absorbing recovery seeds, reported quantities and acceptance bands into shared test support, and added the absorbing committed-kit locator.
- 2026-11-15 (attempt 01, retry 3): Reviewed the inherited edits, rewrote two doc comments to state their contracts, and recorded static selected-test counts.

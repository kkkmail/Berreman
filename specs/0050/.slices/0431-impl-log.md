# Step 0431 implementation log

## Progress

- Read AGENTS.md, the worker base and IMPLEMENT role, project prompt, naming
  conventions, slice, gate snapshot and immutable S122-BANDS-MOVE obligation.
- Read binding rulings R0001/R0002/R0003 and standing manuals 013/015. Their
  repaired behavior and supervisor-owned gate authority remain unchanged.
- Located the seed list, quantity expressions and band loop at their existing
  homes. Test support already references Optimization and compiles BiaxialSample
  before AbsorbingBiaxialSample; no project-file edit is needed.
- Before implementation: move the existing declarations and band expressions,
  add the second read-only kit locator case, and redirect all authorized uses.
- Implemented all seven authorized source edits. The eight-seed body and the
  nineteen quantity expressions moved unchanged; the noisy subset still takes
  the first five seeds. Four transparent-suite uses now name the shared list
  fully, with no new consumer opens.
- Moved the existing five band branches and their observed-scatter comment to
  absorbingBandBreaches. It evaluates each quantity once, returns ordered typed
  breaches for absent bands or failed strict comparisons, and has no assertion
  or test-framework dependency. The noisy fact calls it with each row's scatter
  and joins breach descriptions before the unchanged report.
- Added AbsorbingBiaxialNoisyKit using the existing upward repository search and
  folder-existence filter. Neither kit lookup writes or creates a folder.
- Before handoff: read the final source diff for scope and expression continuity;
  preserve inherited baseline evidence and document supervisor-only validation.
- Read the resulting diff: the seed body and derived expressions are unchanged,
  the five numerical bands retain their original strict comparisons, and all
  assertions outside the authorized band-loop replacement remain intact. The
  source inventory contains exactly the seven authorized files, with no
  CR-at-EOL diff discrepancy. Each moved name has one declaration.
- Wrote 0431-state-of-the-world.md with every required section and historical
  baseline values copied from 0404 and 0430, explicitly distinguished from a
  current result. No scope deviation or additional test identity was needed.

- Attempt 01, retry 3 (Claude): the earlier worker's backend hit its quota after it
  had written all seven edits and this log. Reviewed that diff in place rather
  than redoing it: against the slice's four numbered moves, the S122-BANDS-MOVE
  oracle requirement and rulings R0001-R0003 (their files are untouched here).
- Static compile review, without running a build (Invariant 6): every name the
  moved code reads resolves (ParameterName from ItemModels, NoiseSeed from BenchNoise,
  RecoveredQuantity and rowFor from RecoveryEnsembles, RelativeErr and QuantityValue
  from MuellerFitting, AbsorbingTriclinicParameters.axes from MuellerInverse). The
  added opens in AbsorbingBiaxialSample shadow no name the file already used, and
  BiaxialSample's BenchNoise open shadows nothing. The `quantity` record label
  resolves to RecoveryBandBreach, the latest declared, rather than RecoveryStatistics.
  CommittedKit has no exhaustive match outside its own member, so its second case
  breaks no consumer, and data/absorbing-biaxial-noisy is absent, so it answers None.
- Rewrote two doc comments to state their contracts: absorbingReportedQuantities
  (what it reports and why the isotropic absorption is there) and
  absorbingBandBreaches (strict-below rule, a missing band is a breach, empty when
  every quantity is in band). The spec 0044 §9 citation is right: it is that spec's
  tolerance protocol (specs/0044/.manual/004 §9). No code changed.
- Checked line endings with `git ls-files --eol` and a byte count: zero CRs, every
  touched file w/lf.

## Files modified

- Berreman/BerremanTestSupport/CommittedKits.fs
- Berreman/BerremanTestSupport/BiaxialSample.fs
- Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs
- Berreman/BerremanTests/InverseFitHarness.fs
- Berreman/BerremanTests/MuellerInverseTests.fs
- Berreman/BerremanTests/BiaxialInverseTests.fs
- Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs
- The prescribed 0431 implementation plan, log and state-of-the-world handoff.

## Testing state

No checks run. IMPLEMENT Invariant 6 assigns build, elevation, selected
unit-tests, selected kit-acceptance and all twenty-one s42 invariants to the
supervisor after worker exit. No test identity is added, renamed or removed.
Static source counts (not measured): the four selected BerremanTests classes hold
58 facts (1 + 11 + 20 + 26), and KitAcceptanceTests holds 6 kit-acceptance facts.
None of those files declares a theory or a runtime skip. Implementation is complete. The
structured handoff reports gates_run=[] and gate_results={}, with
commit_ready=false only because no gate was run locally, as in steps 0428-0430.
Nothing blocks the step: every requirement is in place, waiting on the supervisor's
gate run. No development run took place, so no deadline was
exceeded. No baseline or checkpoint was authored or changed.

## Artifacts

No run artifacts produced. All prescribed worker documents remain at the
task's exact paths under specs/0050/.slices/.

## Gotchas

- specs/0050/.manifest.state.json was already dirty and is supervisor-owned.
- Every numeric seed, derived expression and band stays unchanged. The missing
  band branch becomes a typed breach, as expressly required by the slice.
- No gates, budgets, floors, checkpoints, census, versions or external
  repositories may be changed. No Git mutation is authorized or performed.

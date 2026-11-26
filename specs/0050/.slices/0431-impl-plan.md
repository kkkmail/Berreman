# Implementation plan

Move the existing recovery acceptance data into BerremanTestSupport for spec
0050 S12.2. Extend CommittedKit with AbsorbingBiaxialNoisyKit using the existing
root walk and absent-folder behavior. Move ensembleSeeds unchanged to the end
of BiaxialSample and qualify its four uses in MuellerInverseTests and
BiaxialInverseTests without adding opens to either consumer.

Move the noisy suite's five-seed selection and nineteen reported quantities
after AbsorbingBiaxialSample.searchBox. Add the typed RecoveryBandBreach record
and total absorbingBandBreaches function with the existing band constants and
strict double comparison. Replace only the noisy fact's band loop with one
assertion over shared breaches; retain its other assertions, diagnostics and
numbers. Delete the superseded declarations in InverseFitHarness and
AbsorbingBiaxialNoisyTests in the same change.

The seven source files are exactly those authorized by 0431.slice-md. No
project, product, committed-kit, test-identity, registry or gate changes are
needed. Preserve R0001/R0002/R0003 behavior and the already dirty supervisor
manifest. Risks are changed name binding, numerical drift and treating NaN as
an in-band spread; qualify the transparent consumers and accept only the
original strict spread.value < band.value condition.

IMPLEMENT Invariant 6 assigns every build, test, analyzer and invariant run to
the supervisor. Write the implementation log and state-of-the-world handoff
without running checks or claiming gate success. Inherit baseline evidence
without authoring or changing supervisor counts.

## Attempt 01, retry 3 (Claude)

The earlier worker on this attempt left all seven edits in the working tree before
its backend hit its quota. This retry reviews that diff against the slice, the
S122-BANDS-MOVE oracle and the rulings instead of redoing it, makes any correction
in place, and writes the handoff.

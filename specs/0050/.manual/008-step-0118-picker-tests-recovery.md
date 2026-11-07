# Step 0118: picker fixture and test census recovery

The operator approved the four-step plan in `.slices/0118-01.postmortem-review-md`
after committing the failed attempt as `0552847`. This ruling authorizes the
fixture repair and retains the three separate picker facts added during the
coverage retry. That retry and its selected class did not themselves confer
test-identity authority; this decision supplies the missing authority.

Effective at step 118, create these identities under owner/discovery gate
`ui-tests`, class `OpticalConstructor.Ui.Tests.AppContextTests`, source
`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs`:

- `the library folder picker resolves its start folder and delivers only the first selected folder on the UI thread`
- `cancelling the library folder picker leaves the selection unchanged`
- `an unavailable storage provider leaves the library folder selection unchanged`

The declared configured-export-folder fact remains unchanged. The step thus adds
four facts, not one. Preserve every assertion, the production implementation,
gate selection, published requirements and all 988 compiled artifacts. Append
the three exact create rows to `.operator-test-effects-jsonl`, retaining the
step-52 rename. Validate the full 494-step plan and refresh the profile-1 receipt.
Do not write a selected UI count as a whole-suite floor or change supervisor
state, numeric checkpoints, expected-red records or repair budgets.

Repair the fixture's FS0905 binding/member collision by naming its private
instance `providerInstance`. Resolve any measured runtime fixture failure while
preserving the four facts and their behavior. ASG's separate changes narrow auth
classification to provider diagnostics and make test-census authority explicit
in judge and worker instructions (ASG 0090 `.manual/031`).

The first runtime run found one additional fixture mismatch: Avalonia resolves
the starting directory to a URI with a trailing directory separator. The
confirmation assertion now trims that separator before comparing the same
directory path. All four identities and every assertion remain; production
behavior is unchanged. The preflight failure is retained in the artifacts.

Verification evidence belongs in `.artifacts/0118-operator-repair/`. Run all 15
unchanged gates, require all 215 smoke cases and the exact selected UI census,
and review the final implementation against accepted 0117 (`32c7211`). Keep the
arc stopped and wait for the operator to commit. After fresh green verification
and a subsequent relaunch instruction, use `--accept-current-slice
--executability-profile 1`; do not fabricate checkpoint 117 or reset counters.

## Verification outcome

All 15 gates passed: build with zero warnings/errors, elevation, 215 smoke tests,
126 selected UI tests and eleven s14 invariants. Both test selections matched
exact identities, with zero failures/skips and all four new facts present.
Supplementary elevation against accepted 0117 covered four product projects and
found zero new findings. The complete 494-step plan and refreshed receipt passed;
all 988 compiled artifacts, published requirements and supervisor records remain
unchanged. ASG passed 377 relevant tests and strict mypy. See the recovery
artifacts and updated 0118 handoff. Awaiting operator commit; no arc launch.

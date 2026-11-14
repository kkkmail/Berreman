# Step 0407 implementation log

## Progress

- Read the task, worker protocols, project prompt, slice, repository instructions,
  naming conventions, applicable rulings 013/015 and the preceding handoff.
- Before implementation: inspect the existing publishing, opened-material,
  fit-snapshot and bound Mueller evaluation contracts; add the five authorized
  regression facts before the product members.
- Added exactly the five authorized facts and extended error rendering without
  changing the original assertions. Reused the shared fifteen-component crystal
  fixture and shipped circular-polarizer/black-box model versions. Covered stale
  working state, wrong owners, missing snapshots, invalid fitted numbers, stable
  order, and file-stem provenance within those facts.
- Before product implementation: add the prescribed members and error cases using
  `OpenedMaterial.withParameterState`, `ParameterValues.ofParameters`,
  `BoundOpticalElement.ofOpticalForm`/`muellerAt`, and the existing source wavelength.
- Implemented all prescribed conversions, provenance, promotion subjects and
  promotion members. Material metadata is preserved; every component is restored
  through `withParameterState`. Black-box evaluation uses the supplied resolved
  model, zero rotation, normal incidence, transmission and the existing 600 nm
  no-source wavelength. Non-owning fitted entries are rejected before evaluation.
- Preserved study/item ordering and one subject per supplied material/item,
  regardless of the ordering or number of fitted components. Candidate discovery
  follows the brief; promotion performs the scoped-value refusal.
- Completed source edits and adjusted new record expressions to repository style.
- Wrote the state-of-the-world at the designated path. Normalized the five edited
  files to LF; ordinary and CR-ignoring diff statistics match (Domain +193/-3,
  tests +436/-3), with no line-ending churn. Source/status inspection shows only
  the authorized source files, three worker documents and inherited manifest edit.
- Handoff complete. No gate or executable verification was run; the supervisor
  owns the pending verification and measured count update.

## Attempt 02 progress

- Read the retry task and the worker instructions. The supplied supervisor build
  exited 1 after 30.60 s (600 s allowance), with FS0039 at
  `StudyPublishing.fs:144` for the `ElementValueId` type and constructor.
- Before repair: traced `ElementValueId` to `Placement.fs`. The project compiles
  that file before `StudyPublishing.fs`; `ElementId.fs` imports `Placement` for
  its existing `SampleId.toElementValueId` conversion. The new publishing file
  lacks that import. Add only `open OpticalConstructor.Domain.Placement`.
- After repair: added the missing import to `StudyPublishing.fs`. The existing
  `ItemId.toElementValueId` now refers to the declared placement identity type;
  no conversion body, promotion behavior, test identity or assertion changed.
- Updated the plan and state-of-the-world with the supervisor diagnostic and
  repair. No build, tests, analyzers or invariant commands were run in this retry.

## Files modified (both attempts)

- Worker plan and this log created at the supervisor's exact paths.
- `StudyPublishingTests.fs`: five promotion facts and five error-rendering cases.
- `StudyPublishing.fs`: the seven prescribed members, `PromotionSubject`, and the
  five additional `PublishError` cases and descriptions.
- State-of-the-world written at `0407-state-of-the-world.md` beside this log.

## Testing state

No checks run. The IMPLEMENT worker protocol explicitly reserves gates for the
supervisor after worker exit. The inherited measured constructor baseline is
1730; five authorized facts imply an expected 1735, not a measured result.

Attempt 02 consumes the supervisor's measured build failure from
`.artifacts/0407-attempt-02-task.txt`: exit 1, three FS0039 diagnostics for one
missing import, elapsed 30.60 s against 600 s. The import repair is implemented;
the supervisor must run the unchanged gates to establish its result. No new
passing result or verified readiness is claimed.

## Decisions and gotchas

- Preserve the pre-existing modification to `specs/0050/.manifest.state.json`.
- No version changes, git mutations, test-identity changes outside the slice,
  or durable product writes are part of this task.
- A missing non-fitted component in `run.parameters` is malformed evidence, not
  permission to use the current working state. It returns `PromotionStudyErr`
  wrapping `UnknownStudyParameterErr` for a material, or `PromotionEvaluationErr`
  wrapping `MissingParameterValueErr` for a black box. This resolves an unspecified
  failure path using the authorized existing errors; no fallback or extra API.
- The constructor project does not compile `BerremanTests/MatrixComparison.fs`,
  whose helper uses a looser relative tolerance. The required 1e-15 element
  comparison uses xUnit's existing numeric-tolerance assertion directly, without
  introducing epsilon logic or adding a helper/project reference.

## Artifacts

- Supervisor feedback: `specs/0050/.artifacts/0407-attempt-02-task.txt`.
- This retry produced no runtime artifacts; any needed artifacts belong only
  under `specs/0050/.artifacts/`.

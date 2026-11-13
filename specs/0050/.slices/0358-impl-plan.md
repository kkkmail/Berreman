# Step 0358 — impl plan

## Approach

Add the study's Domain shape to `Studies.fs` after `StudySession` (step 357), in the brief's order (1)–(16):
`StudyName`, `StudyNotes`, `ParameterScope` (`SessionBlockScope` for the session case, because STL declares
`SessionScope` and ELEV016 keeps DU case names unique), `StudyParameterRef`, `ScopedParameter`, `ParameterLink`,
`SessionGainSetting`, `FitMode`, `ResidualWeight`, `ExperimentWeightSetting` / `DetectorWeightSetting`,
`StudyFitSettings`, `OpenedMaterial` and `Study` with `create`, `sessionsIn` and `gainModelOf`. `StudyError` gains
`InvalidStudyNameErr`, `DuplicateStudyNameErr`, `UnknownStudyExperimentErr` and `InvalidResidualWeightErr`, each
rendered by `describe`.

Reuse rather than invent:

- `FileStem.ofText` for the study's stem (as `ExperimentName.fileStem` / `ItemLabel.fileStem` do);
- `StudyKitFile.fileNameOf` for the clash message's file name (the item relabel rule's shape, step 216);
- `StudySession.splitAll` for `sessionsIn`;
- the `ExperimentRole` tag/`tryOfWire`/`text`/`all` shape for `ParameterScope` and `FitMode`.

No wire converter (step 359 adds the study file's).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs` — the shape, the errors, the three members.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs` — new `module StudyTests`, five facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — append `StudyTests.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs` — widen the error-render fact to the
  four new cases.

## Risks

- New record labels (`name`, `version`, `experiments`, `weight`, `model`, …) can shadow earlier records in files that
  open `Studies` (`StagedMuellerPath.fs`, `StudySessionTests.fs`). Only the authorized files may change, so record
  literals in Studies and the tests are annotated; the build shows whether Optimization still infers its types.
- ELEV rules: two-word case names, no `bool`, no primitive beside our types in free functions (all new API is members).

# Step 0286 — impl plan

## Approach

Experiment names as pure Domain data (spec 0050 S9.1), plus the template member that writes one from a setup's slots.
Nothing stores a name yet (step 290 does).

1. `OpticalConstructor.Domain/Experiments.fs`
   - Move `ExperimentError` up to directly after `ExperimentNotes` (step 285's last type). Keep its two cases, add
     `InvalidExperimentNameErr of reason : string` and `member this.describe : string`.
   - Add `ExperimentName of string`, with `.value`, `tryCreate` (refuses blank with `InvalidExperimentNameErr`),
     `fileStem` (`FileStem.ofText`), and `nextFreeAmong` (`FileStem.firstFreeText`, mirroring `SetupName`).
   - Add `type ExperimentId with member this.defaultName : ExperimentName` (`E<n>`). This is an intrinsic extension,
     because `ExperimentName` comes after `ExperimentId`.
2. `OpticalConstructor.Domain/NamingTemplates.fs`
   - Open `OpticalConstructor.Domain.Experiments`. `Experiments.fs` compiles before `NamingTemplates.fs`.
   - Add `type NameText with member this.toExperimentName`, which is one-way (ELEV006).
   - Add `type NamingTemplate with member this.experimentNameAmong …`, which calls `nameReadingOf` → `writeName` →
     `toExperimentName` → `nextFreeAmong`, or answers `unnamed`.
3. Tests: add two facts to `ExperimentProxyTests` and one to `NamingTemplateTests`, exactly as the brief names them.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/NamingTemplates.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/NamingTemplateTests.fs`

## Risks

- **`open Experiments` in `NamingTemplates.fs`.** Record-label inference could shift, for example `label`, which
  `VariedElement` also has. Every literal there is annotated or uses labels local to the module, so the build is the
  check.
- **The `E<n>` text.** It would otherwise exist twice: in `ExperimentId.fileStem` and in `defaultName`. To keep one
  rule, `fileStem` is derived from `defaultName.fileStem`. It moves into the same intrinsic extension and keeps the
  same compiled member and value. Step 290 deletes `fileStem`.
- ELEV: no new free function. Every new name is unique (`InvalidExperimentNameErr`, `ExperimentName`, `defaultName`,
  `toExperimentName`, `experimentNameAmong`).

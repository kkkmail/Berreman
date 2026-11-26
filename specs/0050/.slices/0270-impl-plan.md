# Step 0270 — impl plan

## Approach

The reference pipeline stops parsing its labels with regular expressions and reads them through the kit's naming
template (`NamingTemplate.standard.readName`, step 269), spec 0050 S8.3 (001 §J, §R, §T).

1. `OpticalConstructor.Domain/MuellerReconstruction.fs`:
   - delete `open System.Text.RegularExpressions`, `experimentRegex`, `objectRegex`, the `MuellerExperimentLabel`
     extension holding `parsed`, `ParsedExperiment`, `ParsedObject` and `MuellerLabelToken`, with their docs;
   - open `Kit`, `Setups` and `NamingTemplates`;
   - `MuellerExperimentLabel` gains `member this.nameText : NameText`, its doc saying a label reads through
     `NamingTemplate.standard`;
   - after `MatrixKind`, `type NameReading with` holding `family`, `objectRotation`, `matrixKind` and `objectPhi`,
     moved from `ParsedExperiment` with the same rules (CPL in the Prep-1 / Analyzer-1 part; the Obj parts that have
     an angle, in position order);
   - `buildDesign` keeps a row only when `NamingTemplate.standard.readName r.experiment.nameText` reads with the
     requested `matrixKind`; otherwise it filters as before.
2. `BerremanTests/MuellerReconstructionTests.fs`: open `OpticalConstructor.Domain.Kit` (and `NamingTemplates` for
   `readName`); rename the `parseExperiment …` fact to its successor name, reading the label through the template
   (an `Error` fails with `describe`), assertions unchanged; the §7.2 fact's `prepareRow` takes `phi` from the
   reading's `objectPhi`.
3. `OpticalConstructor.Tests/NamingTemplateTests.fs`: open `MuellerReconstruction`; add
   `a lab label's reading names its reference family, matrix kind and object angle` with the five labels.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/NamingTemplateTests.fs`

## Risks

- `Family` / `MatrixKind` cases (`Air`, `Qz`, `Lr`, `CplCpl`, …) opened into `NamingTemplateTests` must not shadow
  anything that file already uses.
- `buildDesign` must keep the same rows in the same order; the darkness labels (`…-(Darkness)-…`) no longer read but
  no design selects `Air`. The §7.2 numbers must be unchanged; the selected class is run to show it.
- ELEV: the new members live on `NameReading` (extension members, unique on that receiver), and `nameText` destructures
  its own union rather than re-wrapping `.value`.

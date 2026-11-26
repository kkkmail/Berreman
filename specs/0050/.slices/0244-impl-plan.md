# Step 0244 — impl plan

Obligation `S81-PARAMETER-NAMES` (IMPLEMENT). Every parameter name an item model publishes is written once, as a
member of `ParameterName`, and the publishers name those members. No name's text, order, kind or default changes.

## Approach

1. `OpticalConstructor.Domain/ItemModels.fs`, on `ParameterName`, each with a one-line doc citing spec 0050 S8.1:
   - fixed names as static properties, texts unchanged: `azimuth`, `ellipticity`, `degreeOfPolarization`,
     `intensityScale`, `extinctionRatio`, `diattenuation`, `axisOffset`, `retardance`, `fastAxisOffset`,
     `blackBoxScale` (`scale`), `gain`, `darkOffset`, `rotationUncertainty`, `elementUncertainty`,
     `throughputUncertainty`, `dialZero`, `substrateThickness` (`substrate.thickness`);
   - positional static members: `muellerElement (row : int) (column : int)` → `m<row><column>`,
     `filmThickness (position : int)` → `film<k>.thickness`,
     `repeatedLayerThickness (position : int) (layer : int)` → `film<k>.layer<j>.thickness`;
   - `member this.inComponent (position : int)` → `c<k>.<name>`.
2. The publishers name the members and change nothing else:
   - `ItemParameterAxis.detectorNoiseAxes` (the three uncertainties);
   - `SourcePreset` gains `static member lightSourceAxes` (the four light-source axes, same kinds and defaults), and
     `parameterAxes` answers it;
   - `Sample.parameterAxes` (`filmThickness`, `repeatedLayerThickness`, `substrateThickness`);
   - `OpticalForm.parameterAxes` (leakage, offsets, retardance, `muellerElement`, `blackBoxScale`, `inComponent`);
   - `ItemModelForm.parameterAxes` (`gain`, `darkOffset`);
   - `Kit.fs` `ItemDial.parameterAxes` (`dialZero`).
3. `OpticalConstructor.Tests/ItemModelsTests.fs`: the fact `every published parameter name is one ParameterName
   member`, exactly as the brief lists its assertions. It is written first and is red by compile error (the members
   do not exist), then green after (1)–(2).

## Files

`ItemModels.fs`, `Kit.fs`, `ItemModelsTests.fs` — the three authorized `edits`. No other file changes.

## Risks

- ELEV rules on the positional members: they take `int`, which AGENTS.md permits on a member of the type
  ("Members may speak their type's own primitives"). The elevation run confirms it.
  - **Outcome:** it did not. ELEV010 flagged all four positional members.
  - The positions are now `ParameterNameIndex`, the one type this step adds. The impl-log's Decisions section
    records why.
- Existing facts of steps 207, 208 and 215 must stay green unchanged; they compare the texts, so any text drift is
  caught by them.

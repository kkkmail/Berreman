# Step 0351 — impl plan

## Approach

This step is a MOVE. Stages 2 and 3 of the section 7.2 fact move into
`OpticalConstructor.Optimization.StagedMuellerPath`, after `calibrateStageOne`. The fact keeps only its assertions and
their formatting.

1. `StagedMuellerPath.fs`:
   - `StagedMuellerError` gains `ObjectReconstructionErr of kind : MatrixKind * error : SolverError`.
   - New record `StagedMuellerData`: `stageOne`, `gains`, `cplCplDay1Gain`, `qz`, `lr`, `qzLr`.
   - New `reconstructStaged solver excluded captures`. It runs `calibrateStageOne`, then the split time from the
     bullshit checks, then the nine AIR blocks in the brief's order, then the five families' projections, then the
     three solves (`Qz`, `Lr`, `QzLrProduct`).
   - Private helpers carry the moved bodies unchanged in their float operations:
     - a private non-empty AIR block record, with members for its rows and its mean capture time;
     - `airBlock`, which finds a block or answers `EmptyCaptureSetErr` naming its label;
     - `fitAirGain`;
     - one helper per gain rule (repeat pair, split, interpolated);
     - the per-row projection.
     No helper has a bare primitive or a single domain-typed parameter (ELEV003P, ELEV010P, ELEV013).
2. `MuellerReference.fs`: add the nine `AirGain` values after the cascade values, and one line on `dataMatrixTol`'s doc.
3. `MuellerReconstructionTests.fs`: the section 7.2 fact calls `reconstructStaged`. It deletes the moved bindings,
   asserts over `staged.qz` / `.lr` / `.qzLr`, and gains the nine-gain assertion. Its leading comment shrinks to one
   to three lines; the §8.4 / §8.5 paragraphs stay word for word.
4. `StagedMuellerPathTests.fs`: two new facts over synthetic captures. The AIR rows are nine readings at 0°…160° with
   `avgTotal` 2 + cos 2α, plus one darkness row and one bullshit-check row.

## Risks

- **Bit-identity.** Every float operation must stay as it was, and in the same order:
  - `fitAirGain`'s `List.map`s and `scalarGain`;
  - `meanTime`'s tick average;
  - `prepareRow`;
  - `buildDesign` over the same families, in the same order.
  Concatenating the design parts with `Array.concat` keeps the same rows in the same order; it does no arithmetic.
- **ELEV.** Private signatures are held to the rules. Every helper has two or more parameters and no bare primitive.
  `DateTimeOffset` is not in the primitive vocabulary.
- **Nesting depth of `Result.bind`.** Splitting the gain rules into helpers keeps it readable.

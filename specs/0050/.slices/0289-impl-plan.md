# Step 0289 — impl plan

## Approach

A dial-mounted item is scanned by its raw dial reading (spec 0050 S9.1; 001 §I, §K).

1. **Domain, `Experiments.fs`.**
   - `VariableParameter` gains `VaryDialReading` after `VaryR2`: code `dial`, label `Dial reading`, unit `°`.
   - `allowedFor` is unchanged.
   - `VariableRange.forVariable VaryDialReading` is 0…360° over 19 points.
2. **Domain, every other match over the variable.** Each gains the case explicitly, with no wildcard:
   - `MeasuredData.validateAgainstExperiment` treats it as `VaryR1`;
   - `OutOfBandDiagnostic.requestedWavelengthsFor` treats it as a fixed wavelength.
3. **Domain, `Setups.fs`.** Two members on `SetupSlot`, after step 277's:
   - `tryFindDialMounted items`: the dial-mounted item of `items` that the slot holds (`content.itemOpt`), with its
     sign. It is the one lookup that both the scan variables and the run read.
   - `scanVariablesWith items`: `allowedFor` of the slot's kind, with `VaryR1` replaced by `VaryDialReading` when
     `tryFindDialMounted` finds an item.
4. **Controls, `ExperimentControls.fs`.**
   - `VariableChoice` gains `VaryDialReading`, with `uiKey` `dial` and caption `Dial reading`.
   - `allVariables` lists it last.
5. **Ui, `TableAndElementRotationView.fs`.**
   - Both mirror maps gain the case.
   - `variablesOfElement` reads `slot.scanVariablesWith model.kitItems`. It serves both `ExpChooseElement` and the
     bay's `variableChoices`.
   - The `ranged` matches in `ExpChooseElement` / `ExpChooseVariable` and `clampSweep` name the case.
   - `chartForRun` sweeps `VaryDialReading` as it sweeps `VaryR1`. Each reading x turns the varied slot at
     `sign.physicalAngleOf item.dialZeroAngle (Angle.degree x)`. It gets its own x label, vary text and title, and an
     angular axis.
6. **Tests.**
   - `SetupTests`: `a dial-mounted item's slot scans its dial reading where a fixed one scans R1`.
   - `ExperimentControlsTests`: `a dial scan draws the run at the physical angle of each reading`. It is a ui-tests
     fact, not smoke.

## Files

The Domain files `Experiments.fs`, `MeasuredData.fs`, `OutOfBandDiagnostic.fs` and `Setups.fs`; the Controls file
`ExperimentControls.fs`; the Ui file `TableAndElementRotationView.fs`; and the tests `SetupTests.fs` and
`ExperimentControlsTests.fs`. The other listed test files name `VariableChoice` cases and should compile as they are.

## Risks

- **ELEV016 (unique DU case names).** `VaryDialReading` sits in both Domain and Controls, as `VaryR1` already
  does. Controls does not reference Domain, and the tree is ELEV-clean with the same pairing, so the case follows
  the existing mirror pattern.
- **A slot that holds no dial-mounted item but is given `VaryDialReading`.** This is possible only for a stored
  experiment whose item was re-mounted. The run reads its reading as its R1, which is the `SetupSlot.turnAngle`
  rule: a fixed item reads no dial, and its zero is `Angle.zero`.

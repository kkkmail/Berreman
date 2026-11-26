# Step 0238 — impl plan (IMPLEMENT: the constructor's flow strip as model state, `S72-FLOW-STATE`)

## Approach

All production edits are in `OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. Before `TableAndElementRotationModel`, add `ConstructorFlowStep`:
   - cases `ItemsFlowStep | SetupsFlowStep | ExperimentsFlowStep`;
   - `static member all` in 001 §O's order;
   - `member this.caption : ControlCaption` returning `Items`, `Setups` or `Experiments`.

   It is window state that is never stored, so it carries no `value` tag. Each doc is one to three lines citing
   spec 0050 S7.2.
2. Add the field `flowStep : ConstructorFlowStep` right after `kitItems`. `initWith` sets it to `SetupsFlowStep`.
3. Add `ShowFlowStep of ConstructorFlowStep` to `TableAndElementRotationMsg`, beside `SelectBay`.
4. Update arms:
   - `ShowFlowStep ItemsFlowStep` sets `flowStep` alone.
   - `ShowFlowStep SetupsFlowStep` sets `flowStep`. If the Experiments bay is open, it moves `ribbon` to `BayNames.add`;
     any other bay stays.
   - `ShowFlowStep ExperimentsFlowStep` sets `flowStep` and opens `BayNames.experiments`.
   - `SelectBay name` keeps setting `ribbon`. It also sets `flowStep` to `ExperimentsFlowStep` for
     `BayNames.experiments` and to `SetupsFlowStep` for any other bay.
   - The bay test is inline in each arm, so there is no free helper of one argument (ELEV013).

The tests go in `OpticalConstructor.Tests/KitWindowModelTests.fs`: the three declared facts, built over
`initMain ()` and closing the scratch kit in a `finally`, as the existing facts do.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Risks

- **Whole-model equality.** Facts that compare whole models still hold, because `SelectBay` and `ShowFlowStep` are
  the only writers of `flowStep`. `switching the constructor…` compares `forward` with `back` after
  `SwitchConstructorMode`, which does not touch `flowStep`.
- **Unique names (ELEV014/016).** `ConstructorFlowStep`, its three cases and `ShowFlowStep` are new across the tree.
  A grep finds nothing for `FlowStep`.
- **Field order.** Record field order matters to the slice ("after `kitItems`"), not to F#.

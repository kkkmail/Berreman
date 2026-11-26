# Step 0318 — impl plan (S9.3 split matching)

## Approach

All of it lands in `OpticalConstructor.Domain/DataSplits.fs` after `splitGroups`, as pure Domain; nothing is
written (the window applies a split in step 320).

1. `type DataRowKey with member this.nameText : NameText`, destructuring the key as
   `MuellerExperimentLabel.nameText` does; no member converts back (ELEV006).
2. `DataSplitError` (`SplitKeyTextErr of ExperimentError`, `SplitKeyNameErr of NamingTemplateError`,
   `SplitKeySetupErr of SetupError`, `SplitKeyTwiceErr of reason : string`) with `describe`.
3. `SplitSetupOffer`, `SplitGroupMatch` (`ExperimentForGroup` / `SetupOfferForGroup` / `GroupWithoutMatch`),
   `SplitGroup`, then `[<ReferenceEquality>] DataSplitParam` (it holds `choiceOf`), then `DataSplit` with
   `matched` / `offered` / `unmatched` and `static member ofGroups`. `DataSplitParam` is declared before
   `DataSplit` so `ofGroups` is an intrinsic member, not an extension.
4. `ofGroups` folds the groups in order; every earlier group's result is the state, so the twice-rule, the
   offered setups (held for `generate`) and the offered experiment names (taken for `experimentNameAmong`) are
   all read from the groups already made — no separate accumulator:
   (a) `key.nameText.toExperimentName`, else `SplitKeyTextErr`;
   (b) first experiment whose `name.fileStem` equals it ignoring case;
   (c) `template.readName`, else `SplitKeyNameErr`; `writeName` of the reading as an experiment name, matched
       the same way;
   (d) the offer: `itemsOf`, a role read that the table lacks is `UnknownSetupSlotErr` naming key and role;
       one-choice axes over every slot in light order whose role a `SlotPlaceholder` names (read item or
       nothing, an object's read angle as R1, through `physicalAngleOf` for a dial); `SetupGrid.generate` with
       held = `param.setups` then the earlier offered setups; experiment by `experimentNameAmong` with the
       reading's note, taken = kit's + earlier offered, unnamed = the key's name;
   (e) a match an earlier group already holds is `SplitKeyTwiceErr` naming both keys and the experiment.

## Tests (DataSplitTests, +4 facts)

Test-local copies of step 271's slots, items (`QZd` dial-negative, zero 10°), `lab` template and a `choiceOf`
over the shipped model entries (`BuiltinCatalogue.itemModel`, the HeNe preset through `BuiltinCatalogue.source`),
as `SetupGridTests` copies them. The four facts are the slice's four names exactly.

## Risks

- `generate` returns a list; one-choice axes make exactly one combination, so the empty answer is an
  unreachable typed refusal, not a throw.
- Record-label inference (`setup`, `items`, `key`, `points` are shared labels): annotate every construction.
- Dial angles are compared with a tolerance helper (test-local, as SetupTests / NamingTemplateTests do).

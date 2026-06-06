# Resolver decision -- 006.slice-md

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0026\.slices\006.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0026\.slices\006-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0026\.slices\006-impl-log.md`
- Judge MD: `C:\GitHub\Berreman\specs\0026\.artifacts\code_judge\006-03-code-judge.md`
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0026\.artifacts\architecture_critic\006-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0026\.artifacts\reuse_critic\006-02-reuse-critic.md`
- Escalation reason: `too many failures (2/2): code-judge route-back-to-worker on cycle 2` (category: `failures-cap`)
- Verified directly in source: `ConstructorView.fs:377-449`, `Ribbon.fs:222-227`, `RibbonTests.fs:243-260`.

## Diagnosis

The cause is concrete, single-sited, and unanimously identified by all three review
artefacts. The cycle-1 retry established the principle "a generated ribbon button with
no visible effect must not be a live, clickable control" and applied it to the nine
*inert-return* commands (`isParameterlessInvokable` returns `false` for the gesture-only
set, so `Ribbon.commandButton` renders them via `Controls.disabledButton`). The cycle-2
judge routed back because that principle was applied to only one subset. Four
element-edit commands routed to the contextual Element tab — `OpenElementDialog`
(`ConstructorView.fs:380`), `ElementContextMenu` (`:381`), `ResetRotation` (`:382-385`),
and `DeleteElement` (`:386-393`) — flip a model flag or arm a `pending`, yet
`isParameterlessInvokable` returns `true` for them through the `| _ -> true` wildcard
(`:449`). I confirmed this in the source. Because `commandButton` (`Ribbon.fs:224`) keys
its enabled/disabled decision entirely off `isParameterlessInvokable`, those four render
as ENABLED buttons on the brand-new *default* landing page that do nothing visible.
`ResetRotation`/`DeleteElement` are sharper: they arm a `pending` confirm state with no
rendered confirm dialog and no front-door `ConfirmPending`/`CancelPending` control, so
the user leaves the model in an armed state clearable only by an unrelated Esc/deselect.
The slice's own SoW Deferred section concedes it: "those buttons change state without a
visible surface."

The fix is narrow, mechanical, and reuses this slice's own just-added pattern: make
`isParameterlessInvokable` return `false` for those four so the ribbon — which already
reads that predicate — renders them disabled in both the ribbon and the collapsed menus.
The judge and reuse-critic (F1) jointly ask that this be done through ONE source-of-truth
list of "commands with no visible front-door effect," collapsing the current three-way
duplication of the disabled set (`isParameterlessInvokable` `:443-449`, the inert
`applyCommand` arms `:431-432`, and `RibbonTests.gestureOnlyCommands` `:247-249`) that the
code's own docstring (`:439-441`) flags as a sync hazard. The one nuance the worker must
respect: this disabled set is *broader* than `applyCommand`'s inert-return arms, because
the four element-edit commands do mutate the model — so the shared seam is a list of
commands-with-no-front-door-surface, not model-equality and not the `-> m` arm set.

The fix touches exactly two code files. `ConstructorView.fs` holds the predicate and the
shared list; `RibbonTests.fs` holds the test list and assertion (the existing test at
`:253-259` asserts every non-disabled registry command IS invokable, so it must be
updated in lockstep or it will fail). `Ribbon.fs` needs no edit — its button already
keys off the predicate (`:224`). `tabOf`/`tabCommands` stay untouched, so the AC-D1/AC-D2
projection invariants hold, and all gates are already green (build/unit-tests/ui-smoke/
ui-tests/structure all pass), so there is no hidden breakage to chase — the worker only
flips four arms, derives one shared list, and extends one test.

This sits squarely in `issue-hint` territory. The escalation is a `failures-cap`
(the route-back the judge wanted would be the third, over the per-launch budget), not a
judge `escalate-to-human` and not a spec-level ambiguity. The cause is named with
file:line, the critics and judge agree (no disagreement to adjudicate), the fix is well
under the 5-sentence / 2-file caps, requires no new design, dependency, or scope change,
and is the same defect class the prior retry already resolved with the same tool. I am
highly confident one more route-back carrying this hint closes the slice. The only minor,
non-blocking item I fold in is reconciling the contradictory CRLF-vs-LF Gotcha note in
the SoW (the SoW is the worker's standing per-round output, not a third directed code
edit). I deliberately exclude reuse F2 (`loadOrEmpty`) and F3 (`commandLabelKey`), which
the judge explicitly marked advisory and not grounds for routing, to keep the final
attempt tightly scoped.

## Verdict

issue-hint

## Hint

In `ConstructorView.fs`, make `isParameterlessInvokable` return `false` for the four
element-edit commands `OpenElementDialog`, `ElementContextMenu`, `ResetRotation`, and
`DeleteElement` — today the `| _ -> true` wildcard (`:449`) renders them as ENABLED
ribbon/menu buttons that flip a model flag with no rendered front-door surface, and
`ResetRotation`/`DeleteElement` additionally arm a `pending` the user can neither see nor
clear. Do this by introducing ONE source-of-truth list of 'commands with no visible
front-door effect' holding both the nine gesture-only commands and these four, and derive
`isParameterlessInvokable` (and the `RibbonTests` disabled-set list) from it, collapsing
today's three-way duplication; note this list is deliberately broader than
`applyCommand`'s inert-return arms, since these four DO mutate the model, so the seam is a
shared list, not model-equality. The ribbon's `commandButton` (`Ribbon.fs:224`) already
keys off `isParameterlessInvokable`, so no `Ribbon.fs` edit is needed — change only
`ConstructorView.fs` and `RibbonTests.fs`. Extend a `ui-test` to assert those four render
disabled, keep `tabOf`/`tabCommands` untouched so AC-D1/AC-D2 hold and every gate stays
green, and reconcile the contradictory CRLF-vs-LF Gotcha note in the SoW.

```json
{"verdict": "issue-hint", "rationale": "failures-cap escalation, not a judge escalate-to-human: the cycle-2 judge route-back is a concrete, single-sited, unanimously-agreed defect. Four element-edit commands (OpenElementDialog, ElementContextMenu, ResetRotation, DeleteElement) render ENABLED on the new default landing page via the isParameterlessInvokable wildcard at ConstructorView.fs:449 but have no rendered front-door surface; ResetRotation/DeleteElement also arm an unresolvable pending. Verified in source. The fix reuses this slice's own disabledButton pattern: flip the predicate for those four via one shared 'no visible front-door effect' list and update the RibbonTests list/assertion -- two files (ConstructorView.fs, RibbonTests.fs); Ribbon.fs:224 already keys off the predicate so needs no edit; tabOf/tabCommands untouched keeps AC-D1/AC-D2; all gates already green. Same defect class the cycle-1 retry already closed with the same tool, no new design/scope, well within 5-sentence/2-file caps. High confidence one more route-back closes the slice.", "operator_reply_text": "In ConstructorView.fs, make isParameterlessInvokable return false for the four element-edit commands OpenElementDialog, ElementContextMenu, ResetRotation, and DeleteElement -- today the | _ -> true wildcard (:449) renders them as ENABLED ribbon/menu buttons that flip a model flag with no rendered front-door surface, and ResetRotation/DeleteElement additionally arm a pending the user can neither see nor clear. Do this by introducing ONE source-of-truth list of 'commands with no visible front-door effect' holding both the nine gesture-only commands and these four, and derive isParameterlessInvokable (and the RibbonTests disabled-set list) from it, collapsing today's three-way duplication; note this list is deliberately broader than applyCommand's inert-return arms, since these four DO mutate the model, so the seam is a shared list, not model-equality. The ribbon's commandButton (Ribbon.fs:224) already keys off isParameterlessInvokable, so no Ribbon.fs edit is needed -- change only ConstructorView.fs and RibbonTests.fs. Extend a ui-test to assert those four render disabled, keep tabOf/tabCommands untouched so AC-D1/AC-D2 hold and every gate stays green, and reconcile the contradictory CRLF-vs-LF Gotcha note in the SoW.", "confidence": "high"}
```

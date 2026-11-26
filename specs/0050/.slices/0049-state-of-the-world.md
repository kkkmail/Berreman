# Step 0049 state of the world

## Where we are

Spec 0050 S1.7 step 0049 is implemented: MaterialEditorWindow requires its
category store from the caller. The production opener continues passing the
shared store, while tests that relied on the removed default now supply the
existing Built-in catalogue fixture. Gate execution belongs to the supervisor;
no successful gate result is claimed.

## What's working

- Require CategoryProxy as MaterialEditorWindow's third constructor parameter.
- Remove the self-seeding fallback and pass categories directly to the context.
- Supply catalogue category stores in all 21 previously implicit test calls.
- Preserve explicit shared-store callers, every fact name and every assertion.

## Tests

No build, test, analyzer or invariant gates were run, per Invariant 6 and the
IMPLEMENT worker protocol. The supervisor's resolved roster is build,
elevation, all discovered ui-smoke cases, ui-tests selected to
MaterialEditorWindowTests, MaterialsWindowTests, SampleEditorWindowTests,
ScrollExtentTests and WireUiFinalCompositionTests, and all seven s07 invariants.

Source review against the immutable S17-EDITOR-CATEGORIES obligation found the
mandatory parameter, direct context assignment and explicit fixture calls in
place. The four-file source diff changes no test identity, attribute or
assertion. Ordinary and CR-insensitive diff counts agree; this source review
is not a gate result.

The smoke count below is inherited from checkpoint 47. That checkpoint has no
ui_tests baseline; zero retains the established absence marker. These are not
measurements from this round, and the supervisor owns the actual baselines.

```yaml
gates:
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

The editor consumes the mandatory CategoryProxy without constructing a store.
MaterialsWindow already passes categories = categories, preserving shared live
category behavior. Tests without a supplied store reuse
BuiltinCatalogue.categoryStore (fun _ -> []), already linked into the UI test
project, through an OpticalConstructor.Tests namespace import. No helper,
seed data, project reference or new type is introduced.

Only an in-process constructor signature changes. No durable or serialized
shape changes, migration, compatibility overload or fallback is introduced.
The class documentation describes the caller-supplied store contract in three
lines. There is no deviation from the slice.

## Deferred

No implementation requirement is deferred. Verification and acceptance await
the supervisor's gate engine, as required by the worker protocol.

## Gotchas

- MaterialsWindow and four named test files already pass explicit stores and
  need no edits; named constructor arguments remain valid when mandatory.
- BuiltinCatalogue is in OpticalConstructor.Tests, not product Storage code.
- The incoming specs/0050/.manifest.state.json modification remains untouched.
- commit_ready is false solely because the worker cannot certify pending
  supervisor gate results; the requested implementation is complete.

## Changelog

- 2026-11-06: Required the Material editor's category store and supplied the
  Built-in catalogue fixture at all previously implicit test call sites.

# Step 0036 - IMPLEMENT - state of the world

## Where we are

Spec 0050, section S1.5: the built-in catalogue was exported in step 35. This
slice retires that one-time export script. The committed catalogue remains the
shipped data, and the existing parity facts retain their comparison with the
code seeds until their separately specified retirement in S1.7.

## What's working

- Deleted the one-time `Catalogue/export-builtin.fsx` exporter without adding a replacement.
- Preserved the committed catalogue, Storage content copy and five parity facts.

## Tests

No gates or acceptance checks were run: the IMPLEMENT worker protocol assigns
them to the supervisor after this worker exits. `build`, `constructor-unit-tests`
selected to `OpticalConstructor.Tests.BuiltInCatalogueParityTests`, and the six
s05 invariant gates remain pending. No passing result is claimed.

The count below is copied from the supervisor's step-34 checkpoint in
`.checkpoints-json`; it is not a new measurement or a changed baseline.

```yaml
gates:
  constructor_unit_tests: 791
```

## Architecture

The durable catalogue is the committed product-writer output, as required by
001 section B and `.spec-md` sections 3.6 and 7. Removing the disposable tool
introduces no new export path, serializer, compatibility layer or data change.
Storage continues copying `Catalogue/Builtin/**` to consumer outputs.

## Deferred

Nothing within this slice's implementation scope. Gate execution belongs to
the supervisor under the worker protocol.

## Gotchas

- Plain `git ls-files` reflects the index, so it will retain the script until
  the supervisor stages its deletion. The worker performed no git mutation.
- There is no regeneration tool after this deletion; seed or serialization
  changes can still affect the existing parity facts.
- Pre-existing supervisor manifest changes were left untouched.

## Changelog

- 2026-11-05: Deleted the one-time built-in catalogue exporter and recorded the completed implementation for supervisor verification.

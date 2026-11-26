# Implementation log — 0420

## Progress

- Read the task, shared worker protocol, IMPLEMENT delta, project prompt,
  repository and naming instructions, applicable manuals 010/013/015, slice,
  gate snapshot and immutable S121-KIT-IDS acceptance obligation.
- Inspected KitOpening, the three KitStartContext factories and existing
  KitStartTests fixtures. KitContext.openOrInitialise already calls a deferred
  metadata factory only on NotAKitFolderErr; reuse that seam unchanged.
- Before test edits: add exactly the two authorized facts, preserve existing
  assertions, and use real folder storage under the test output directory.
- Added both facts before the production change. The first observes the fixed
  ID, exactly one provider call and the canonical metadata read back from disk.
  The second records provider calls across OpenKit, OpenKitReadOnly, CreateKit
  on a stored kit and direct openKitWith in both modes, closing each session
  before the next. Existing assertions and test identities remain untouched.
- Before production edits: add the provider and its system member, move opening
  into openKitWith with deferred minting, then wire the one new context field
  into CreateKit, the real window and the two existing test factories.
- Added KitIdProvider directly after ItemIdProvider and its system extension
  beside ItemIdProvider.system. openKitWith mints inside openOrInitialise's
  ClockTime callback; openKitAt forwards all its unchanged arguments through
  the system provider. The read-only branch still calls openKit directly.
- Wired kitIds directly after clock in KitStartContext and all three context
  factories. Only CreateKit uses the injected opening seam; existing opens,
  zip handling and scratchKit retain their behavior and contracts.
- Reviewed the source diff against S121-KIT-IDS and the existing initializer.
  The six authorized source files are the only product/test edits. Ordinary
  and ignore-CR-at-EOL numstats agree, with no line-ending churn. Corrected the
  new test record expressions to put opening braces on their own lines.
- Wrote the state-of-the-world at the assigned path, preserving inherited
  counts and recording all supervisor verification as pending.

## Files modified

- Worker documents: the assigned 0420 impl-plan, impl-log and state-of-world paths.
- OpticalConstructor.Domain/Kit.fs: reference-equality KitIdProvider record.
- OpticalConstructor.Ui/KitOpening.fs: system provider, injectable openKitWith
  and openKitAt forwarding wrapper with its existing signature.
- OpticalConstructor.Ui/KitStartView.fs: context field and CreateKit wiring.
- OpticalConstructor.Ui/KitStartWindow.fs: system provider composition.
- KitStartTests.fs: the two authorized regression facts and the existing storage
  extension import required for canonical KitProxy reads; default context wiring.
- OpticalConstructor.Ui.Tests/KitStartWindowTests.fs: default context wiring only.

## Testing state

No checks or gates run. IMPLEMENT Invariant 6 assigns execution and verdicts
to the supervisor after worker exit. Expected census effect: two constructor
facts; no UI or smoke identity changes. Verification remains pending.

The unchanged 0420 roster requires build and arc-origin elevation (600 seconds
each), all constructor-unit-tests (180 seconds), all ui-smoke (600 seconds),
ui-tests selecting OpticalConstructor.Ui.Tests.KitStartWindowTests (360 seconds),
and all twenty-one s41 invariants (300 seconds each). No development-run duration
or deadline overrun exists to report. commit_ready=false reflects pending
supervisor verification only; no requested implementation is deferred.

## Architecture

Inject identity generation through a provider record and defer minting through
the existing openOrInitialise metadata factory. Preserve the storage boundary,
canonical serializer, typed errors and session ownership.

## Gotchas

- Preserve the pre-existing specs/0050/.manifest.state.json change.
- No scope deviation is needed. No gate, oracle, census, checkpoint, version,
  analyzer configuration, notebook or external repository edits are authorized.
- Never evaluate mintKitId while constructing the metadata callback: existing
  kits and read-only opens must not consume an ID.

## Artifacts

No runtime artifacts produced. Persistent run output belongs only under
C:/GitHub/Berreman/specs/0050/.artifacts.

# Implementation plan — 0420

Implement S121-KIT-IDS within the six authorized source files. Add the
reference-equality KitIdProvider beside ItemIdProvider, supply its system
implementation at the UI composition boundary, and extract openKitWith while
preserving openKitAt's public signature. Mint inside the existing deferred
openOrInitialise metadata factory, so opening a stored kit never consumes an ID.

Add kitIds directly after clock on KitStartContext. Route CreateKit through
the injected provider and compose the system provider in the window and both
existing test context factories. Preserve the other messages, scratchKit,
storage format, existing tests and all other fields.

Add exactly the two authorized facts to KitStartTests before the production
change. Cover the fixed ID handed over and read back through the real KitProxy,
then existing-kit read-write/read-only opens with a counting provider. Exercise
the deferred initializer through CreateKit on an existing kit and openKitWith
directly within the same authorized fact. Reuse output-folder fixtures and
close each session before reopening.

The main risk is eager evaluation of mintKitId when partially applying
namedKitFile; keep it inside the ClockTime callback. IMPLEMENT Invariant 6
reserves all build/test/analyzer/invariant execution and verdicts to the
supervisor. Record pending verification, inherited checkpoint counts and the
expected +2 constructor / unchanged UI test effects in the required handoff.

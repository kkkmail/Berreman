# Implementation plan

Extend AT-1 in KitAcceptanceTests.fs with the seven final lab files' experiments
using only the existing semantic messages. Reuse the Phone ROI parser, naming
template, data proxy, exclusion reference and real folder KitProxy. Derive the
CPL-LP restart boundary from the earliest BullshitCheck capture in the data.

Keep the existing items/setups fact and all its assertions by retaining the
setup-only construction stage; the full build continues that stage with the
dial draft, split offers, attachment, roles, sessions, two corrections and one
excluded frame. Add only the declared AT-1 data fact, checking independently
read persisted experiments, mapped observations, attachment hashes and unchanged
lab bytes. No product source, gate, oracle, census, version or lab-data edits.

Risks: file-order group identities, one-based mapped-point numbers versus capture
indices, raw dial readings versus physical angles, and preserving the original
setup-only assertions. Inspect the existing domain contracts before editing.
The IMPLEMENT worker's Invariant 6 reserves all build/test/analyzer/invariant
execution and verdicts for the supervisor; this worker writes source and handoff
documents without running gates.

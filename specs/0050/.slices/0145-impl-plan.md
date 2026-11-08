# Implementation plan

Resume step 0145 under the task's operator note and manual 010. The approved
repair is committed at `c5beb60`; accepted 0144 remains `85aa190`. Preserve the
existing S43-OPEN-SEAM implementation, all eight declared concrete test
identities, every existing assertion and manual 009's shared guard.

Read the current naming, picker, opening and scratch-construction code against
the slice and amended construction contract. Keep `KitContext.createInMemory`
in Storage with its two inputs and `Result<KitContext, KitError>` return. It
must initialize one private folder through the canonical factory and propagate
errors without opening, locking, reading the clock or scanning stamps. Review
the existing tests and historical repair evidence; change product code only if
this review identifies a requirement that is still unmet.

Update only the supplied implementation plan, implementation log and
state-of-the-world if the committed repair already satisfies the step. Retain
the inherited YAML constructor baseline of 1,129. Distinguish the operator
repair's recorded 1,137 passing cases from this worker's execution: IMPLEMENT
Invariant 6 assigns every build, test, analyzer and invariant gate to the
supervisor after exit. Run no checks and report no new gate result.

Do not edit supervisor state, counters, checkpoints, baselines, expected-red
records, test effects, frozen contracts, compiled snapshots or the profile-1
receipt. Do not relaunch the supervisor or accept the slice directly. Later
scratch/scene consumers remain at their own steps under manual 010.

# Implementation plan

Implement slice 0426, S121-STUDY-GAIN-VIEW, within the seven authorized source
files. Reuse StudySession.gainModelOffers, Study.gainModelOf, FitMode.all and
the existing StudyCommand messages and kit write guard. Add centralized IDs
and gain captions, the sessions grid after Unknowns, and fit-mode choices
immediately before the existing Run row. No durable format changes are needed.

Add exactly the two declared StudyViewTests facts, capturing semantic messages
and checking selected state. Extend the existing ID, Unknowns scroll extent
and read-only facts without removing assertions. Preserve the existing run
stage commands and all other controls.

Pay attention to FuncUI event closure refresh, initialization selection events,
read-only ComboBox state, accessibility names and scroll reachability. Keep LF
line endings. The existing manifest state edit belongs to the supervisor.

The IMPLEMENT worker's act-only protocol assigns all build, test, analyzer and
invariant execution to the supervisor after exit. Write the implementation log
and state-of-the-world at the supplied paths, with inherited baseline counts
clearly distinguished from unmeasured results. Do not modify gate contracts,
the oracle/census, versions, external repositories or supervisor state.

Attempt 02 repairs the supervisor's compilation diagnostics without changing
acceptance: use the existing experimentCollection.experiments model surface in
the session grid and its test fixture, import Avalonia.Automation in Unknowns,
and import Avalonia and Domain.Studies in Fit. Preserve all seven authorized
file changes, the exact test identities and their assertions. The worker remains
act-only; the supervisor owns the next build and every gate verdict.

Attempt 03 repairs the two ScrollExtentTests forward references reported by the
supervisor. ScrollExtentTests compiles before SlotBarTests, so locate the sessions
grid and gain box by their centralized automation IDs in the existing subjects
collection. Preserve their type and membership assertions, the two screen sizes,
all test identities and the project compile order. Only the authorized scroll
test and these handoff documents need changes for this retry.

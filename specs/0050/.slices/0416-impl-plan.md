# Implementation plan

Implement spec 0050 S11.5's retirement of the obsolete solver handoff. Remove its view, window, window key, automation IDs, collection snapshot types and project entries. Update affected documentation to describe the study's pre-fit checks and the remaining windows.

Move exactly the three authorized view facts into `StudyViewTests`, reusing `studyKitIn`, the existing Fit model preparation and a local attachment-path data proxy. Preserve study order and assert E1's three ready points, E2's missing data and E3's out-of-range data. Delete the eight redundant validation facts with their retired test module; retain the existing domain validation coverage. Update only the authorized scroll, composition and launcher test identities/expectations.

Read the affected source and review the resulting diff for dangling references, compile ordering, fixture semantics and LF preservation. Build, test, elevation and invariant gates belong to the supervisor under IMPLEMENT Invariant 6; report them as pending without inventing results or changing measured baselines. Preserve the entry modification to `.manifest.state.json`, version locks, gate snapshots and external repositories.

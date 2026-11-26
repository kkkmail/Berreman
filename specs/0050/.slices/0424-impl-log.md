# Progress

- Read the supplied task, both worker protocols, project prompt, AGENTS.md,
  naming conventions, slice 0424, gate snapshot, immutable acceptance obligation
  S121-AT1-DATA and applicable operator rulings 013/015.
- Before implementation: inspecting the existing AT-1 builder and the split,
  draft, metadata, correction, parser and exclusion contracts. The only initial
  dirty file is supervisor-owned specs/0050/.manifest.state.json; preserve it.
- Implemented the data continuation using ActivateSetup, experiment draft
  messages, SplitByColumn, ToggleSplitOffer, OfferSplitGroupOver, MatchByTemplate,
  SetExperimentRole/Session, CorrectSetup, ReviewExperimentData and ExcludePoint.
  The full builder and the original setup-only checkpoint share one constructor
  and its failure cleanup. The original fact retains every assertion.
- Added the one authorized fact. A fresh KitProxy reads all 53 experiments and
  checks file/group ordering, ids, setup revisions, dial scans, roles, sessions,
  corrections, attachments and their committed hashes, keyed rows, exclusions,
  mapped captures and kept points. Existing item/setup/snapshot assertions are
  reused. Before/after lab inventories and hashes also guard committed bytes.
- Before final handoff: reviewing declarations and call signatures against their
  existing definitions, scope and line endings; no executable checks or gates.
- Completed source review, reused SessionName.caption, and compared capture
  descriptions with the scan's elevated Angle values to avoid a degree/radian
  round-trip equality assertion. Added an explicit six-session count assertion.
- Wrote 0424-state-of-the-world.md with the required headings and inherited
  floors, clearly separating delivered source from pending supervisor results.
  Ordinary and ignore-CR-at-EOL numstats agree; the only test source edit is the
  authorized KitAcceptanceTests.fs file. No lab file was edited.

# Files modified

- 0424-impl-plan.md and this log created at the exact supplied paths.
- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs.
- 0424-state-of-the-world.md at the exact supplied path.

# Testing state

No checks run. Per the IMPLEMENT worker protocol's Invariant 6, the supervisor
executes all frozen gates after worker exit. No gate outcome is claimed.

# Artifacts

No runtime artifacts produced. Persistent artifacts, if any, belong only in
specs/0050/.artifacts.

# Gotchas

- Preserve the existing setup-only fact, including its empty-experiments assertion.
- New test identities are limited to the one declared by step 0424.
- The original private builder now takes a continuation; buildOpticsMuellerKit
  applies the data continuation, while the original fact uses the setup-only
  checkpoint. This preserves its empty-experiments assertion without copying
  any construction or cleanup body.
- CPL-LP sessions derive their boundary from the earliest BullshitCheck capture;
  no timestamp is transcribed. Exclusion numbers derive from the reference
  capture's one-based position within its group, never from capture_index alone.
- Read the original optics-mueller notes at C:/GitHub/optics-mueller read-only:
  the two +45 final captures were remeasured correctly; only the two specified
  day-1 +90 experiments receive -90 setup corrections. No deviation from 0424.

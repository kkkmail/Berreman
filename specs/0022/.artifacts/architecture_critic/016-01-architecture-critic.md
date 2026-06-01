# Architecture critique -- 016.slice-md cycle 2

## Summary

Clean retry. Cycle 1 routed back on reuse finding F1 (verbatim duplication of the
project-scaffold helpers) with F2 (divergent sidecar-location policy) as a
secondary ask; this cycle resolves both at the right architectural seam and
introduces no new defect. The three slice modules themselves were already found
clean in cycle 1 and are unchanged. The only residual notes are advisory carry-overs
the judge already classified as non-blocking.

## Layering

No violations, and the consolidation respects direction. The shared sidecar-location
seam was added to `OpticalConstructor.Storage.Sidecar` (`Sidecar.fs:30-37`), the
layer that already owns the `.binz` extension and the FsPickler IO — not pushed up
into a Ui module. Both callers reach *downward* into Storage:
`JobRunner.derivedArtefactPath` (Ui → Storage, `JobRunner.fs:245-246`) and
`SynthesisFitPage.fitHistorySidecarPath` (Ui → Storage, `SynthesisFitPage.fs:248-249`).
The F1 fix keeps everything inside the Ui project — `Help.fs` calls
`Templates.*` siblings in the same project — so no new project edge appears. The
clone stays unreferenced (the two grep hits remain documentation comments), so the
arc closes honouring the §A.6/§A.9 reservation.

## Separation of concerns

The F2 placement is the correct call. Homing the seam in `Storage.Sidecar` rather
than in `JobRunner` is forced by compile order — `SynthesisFitPage.fs` (line 33 of
the Ui fsproj) compiles before `JobRunner.fs` (registered last per the slice), so
the fit page cannot depend on `JobRunner`; Storage sits below both. The SoW Gotchas
section records exactly this reasoning, so the choice is deliberate and documented,
not incidental.

One small smell: `JobRunner.sidecarDirectory` and `JobRunner.derivedArtefactPath`
(`JobRunner.fs:237-238,245-246`) are now pure pass-throughs that forward verbatim to
`Sidecar.*`. They add a layer that does no work. Defensible as a discoverability
alias so a reader in `JobRunner` sees the artefact path locally, and the doc-comment
points at the real seam — but a caller could equally call `Sidecar.derivedArtefactPath`
directly. Minor; not worth a re-spawn.

## Consistency

F1 is resolved cleanly and at the source of the drift, not papered over. The seven
scaffold helpers (`air`, `glass`, `film`, `glassPlate`, `defaultLight`, `systemOf`,
`projectOf`) were promoted from `private` to module-public in `Templates.fs`
(`Templates.fs:42-43,54,59,65,72,82`) and `Help.fs` now calls them
(`Help.fs:121-153`); the forked block is gone. Critically, `Help`'s gallery samples
no longer carry their own `projectOf`/`defaultLight` — they route through
`Templates.projectOf`, whose root node uses the single `defaultLight`
(`Templates.fs:74`), so the old 600-vs-550 nm divergence is eliminated *structurally*
(there is now one 550 nm literal, reached transitively). `Help.fs` retains only the
genuinely gallery-specific pieces — the five example stacks, `sourceScript`, and
`GalleryEntry` — which is the right residue. The `Help.fs:111` comment naming
`Templates.defaultLight` is accurate by transitivity (via `projectOf`), not a stale
claim.

## Spec fit

Unchanged from cycle 1 and within scope. The retry was a reuse refactor; AC-J10/J11/J12
remain satisfied by the same 19 tests, and the SystemView3D AC-J12 facts
(`SchematicGeometryTests.fs:131-202`) still derive each segment direction from the
already-solved `EmFieldSystem` via `SystemView3D.beamDirection`/`beamSegments` taking
the solved field as input — the no-re-solve invariant stays structural. The two
advisory cycle-1 spec-fit notes persist unchanged: the active-crystal sample ships a
biaxial plate (not the gyrotropic case of `ActiveCrystal.fsx`, `Help.fs:137-139`) and
the dispersive-glass sample ships plain glass (`Help.fs:152-154`). The judge already
ruled these "corresponding to" rather than "reproducing," and the worker reasonably
left them; re-flagging only so the record shows they were a conscious hold, not an
oversight.

## Risks

- **Fit-history sidecar path moved.** `fitHistorySidecarPath` now writes under
  `workingFolder/.sidecars/<name>.fit-history.binz` instead of the prior flat
  `workingFolder/<name>.fit-history.binz` (`SynthesisFitPage.fs:248-249`). This is a
  real behaviour change, but low-risk: `*.binz` is gitignored regardless of subfolder
  (Part I / slice 012), no test asserted the flat path, and both locations stay under
  the project working folder, never the repo root. The SoW discloses it as
  behaviour-preserving for the gates, which is accurate.
- **`startBackground` still untested** (carry-over from cycle 1). The background
  hand-off (`Async.Start`, `JobRunner.fs:127-134`) and its exception→`RunFailed`
  mapping are exercised by no AC-J10 test; the cooperative loop is driven
  synchronously. Low severity (the function is trivial), unchanged by this retry, and
  the judge already moved past it.

## Bottom line

I would ship this. The retry does exactly what the judge's retry-hint asked: F1's
verbatim duplication is gone with a single 550 nm `defaultLight`, and F2's
sidecar-location policy is unified behind one `Storage.Sidecar` seam that both the
`JobRunner` and the Part G fit page call — homed in the layer that already owns the
`.binz` IO, with the compile-order reasoning recorded. No layering, separation, or
consistency defect was introduced; the only fresh observation is a harmless
pass-through alias in `JobRunner`, and the remaining notes (gallery physics, untested
`startBackground`, the disclosed sidecar path move) are advisory carry-overs already
weighed. The arc closes with the clone unreferenced. My read is ship; the verdict is
the judge's.

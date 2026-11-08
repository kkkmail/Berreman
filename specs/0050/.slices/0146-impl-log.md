# Implementation log

## Progress

- Read the task, worker protocols, project prompt, slice and resolved gate
  snapshot. Read AGENTS.md and naming_conventions.md before edits.
- Initial working tree contains a supervisor-owned change to
  specs/0050/.manifest.state.json; preserve it.
- Before implementation: inspect the canonical serialization/schema stack and
  the existing experiment and kit-file fixtures, then add the four authorized
  tests and production contracts without introducing another parser.
- Added the four declared KitContentFileJsonTests facts and appended their
  compile entry. E1 carries all three binding forms, the version-one glass
  plate, 600 nm source id, attachment/hash and 632.8 nm override; E3 is empty
  with all three optional fields absent.
- The same four facts cover canonical text, naming, unsupported versions and
  invalid envelopes/attachments, plus serializer-owned malformed payloads.
  These are authored assertions, not executed results.
- Before production changes: add the Domain envelope, conditional schema,
  lazy validator and storage extensions by delegating to the existing stack.
- Added ExperimentId.fileStem, the version-1 ExperimentFile envelope and
  Experiment.experimentFile. Added the experiment schema, its shared-common
  lazy loader and validateExperimentFile delegation.
- Added KitContentFiles immediately after KitFolderStore and copied its schema
  with PreserveNewest. Writing delegates to nodeOf/canonicalFileText; reading
  delegates to parseVersionedText with the existing extension/version types.
- Before handoff: review the source edits against the declared effects and
  record implementation status and the inherited baseline at the supplied
  state-of-the-world path. No executable verification is performed here.
- Source review found WaveLength.nm requires the engine's nm unit annotation;
  update the test fixture to 632.8<nm> and import Berreman.Constants before
  completing the handoff. Product diffs are additive and tracked source
  numstats are identical with/without ignoring CR at line ends.
- Applied the engine unit annotation and kept all four fact identities.
- Completed the source review and wrote 0146-state-of-the-world.md with every
  required heading and the inherited 1137-case baseline. All seven authorized
  source/project paths contain the requested implementation; no later slice's
  behavior or additional test identity was added.

## Attempt 02 progress

- Read the retry task and inherited worker protocols. The supervisor's build
  diagnostic identifies FS0597 at the two chained attachment indexer receivers
  in KitContentFileJsonTests.fs, lines 177 and 178.
- Before repair: parenthesize those receivers before AsObject and leave the
  removal of path/contentHash, all assertions and all four fact names intact.
  Preserve the prior implementation and supervisor-owned manifest state.
- Parenthesized both chained receivers. The two fixtures still remove their
  respective required attachment fields and assert SchemaValidationErr.
  No production contract, schema, project entry or test identity changed.

## Files modified

- Worker plan and this log created at the supplied slice paths.
- OpticalConstructor.Tests/KitContentFileJsonTests.fs (new) and its project
  compile list.
- OpticalConstructor.Domain/Experiments.fs.
- OpticalConstructor.Storage/SchemaValidation.fs and its project compile and
  content entries.
- OpticalConstructor.Storage/KitContentFiles.fs (new).
- OpticalConstructor.Storage/schema/experiment.schema.json (new).
- 0146-state-of-the-world.md at the supplied path.

## Testing state

No checks or gates run. IMPLEMENT Invariant 6 assigns build, elevation,
constructor-unit-tests and all eleven s17 invariants to the supervisor.

Attempt 02 addresses the FS0597 diagnostics supplied in the retry task. The
supervisor has not yet rerun the repaired sources; no passing result is claimed.

Implementation is complete and no authority conflict remains. The structured
handoff has empty gates_run and gate_results. commit_ready remains false
because the protocol defines it as including passing gate results; those are
pending supervisor execution, not worker-reported failures or deferred code.

## Artifacts

- Any persistent execution artifacts belong only in specs/0050/.artifacts.
- No run artifacts produced.

## Gotchas

- Exactly four new fact identities are authorized; keep their specified names.
- A foreign positive schema version must pass structural validation before the
  shared parser returns UnsupportedSchemaVersionErr.
- Gate results and baseline publication belong to the supervisor, not this
  worker. No git mutations or version bumps are authorized.
- The live parseVersionedText signature takes JsonText, while the slice's
  shorthand passes a string. KitContentFiles will elevate the text through a
  private source-type conversion member and call the unchanged shared parser.
  KitFolderStore's same-purpose member is private to its own module; it is not
  widened outside the authorized edit surface.
- The immutable S43-EXPERIMENT-FILE oracle matches the slice's envelope, naming,
  validation and typed-error obligations. Checkpoint 144 records the inherited
  constructor_unit_tests baseline as 1137; it remains supervisor-owned.

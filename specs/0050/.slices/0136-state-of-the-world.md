# Where we are

Step 0136 implements S42-OPEN-STATUS after step 0135 introduced kit-relative
measured-data attachments. Opening a saved experiment collection now derives
attachment statuses through the injected data proxy. The production change and
exactly two authorized regression facts are complete; supervisor verification
is pending.

# What's working

- Revalidate unchanged attachments when a saved collection opens.
- Flag changed data and missing files with the required status captions.
- Preserve recorded attachment paths and hashes, including on read failures.
- Reuse one validation helper for attachment and collection loading.
- Add the two authorized regression facts with validation and failure coverage.

# Tests

No worker build, test, analyzer or invariant gate was run, per IMPLEMENT
invariant 6. No gate success or newly measured count is claimed. The supervisor
owns build, elevation, all-discovered ui-smoke, selected InverseConstructorTests
under ui-tests, and all eleven s16 invariant gates in 0136.gates.

The unchanged-data fact saves an attached experiment plus an unattached one,
loads into fresh models sharing the collection store, and asserts inspection,
fresh point counts, parse and range errors, attachment preservation, and no
status for the unattached experiment. The changed/missing fact saves two
attachments at data/a.csv and data/b.csv and checks both status cases, exact
captions, retained experiments and read-error mapping. Its loaders refuse
validation of changed, missing or unreadable data. Both facts refuse any
attempt to re-attach during load.

The expected selected test-count increase is two; it is unmeasured. The YAML
below carries only the existing supervisor baseline from checkpoint 134.
No ui_tests baseline exists in .checkpoints-json, so none is invented.

```yaml
gates:
  ui_smoke_tests: 215
```

# Architecture

CollectionLoad calls ExperimentDataProxy.attachmentStatus for each loaded
attachment. AttachedDataUnchanged uses the same private
validateExperimentAttachment function as AttachDataFileTo, which delegates to
ExperimentDataLoad.loadAndValidate and the existing typed error mapping.
AttachedDataChanged becomes DataFileChangedSinceAttach; AttachedDataMissing
becomes DataFileMissingFromKit through KitRelativePath.toControlCaption. A kit
read failure becomes AttachedFileParseError with the kit error's caption.

The successful load replaces the status map with only the loaded attachments'
statuses. It leaves their metadata intact and omits unattached experiments.
No other message, model field, storage behavior or durable format changes;
there is no migration or compatibility path.

# Deferred

None of the requested implementation is deferred. Gate execution remains
assigned to the supervisor by the worker protocol.

# Gotchas

- The supervisor-owned specs/0050/.manifest.state.json edit existed on entry
  and was left untouched.
- No new test identities beyond the two named in slice 0136 were added.
- Source inspection and the LF-preserving diff do not establish gate success.
- The exit summary cannot claim commit readiness before supervisor verification.

# Changelog

2026-11-07 — Added attachment status checks on collection open, shared validation
with attach, and the two authorized regression facts.

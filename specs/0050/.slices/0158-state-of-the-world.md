# Where we are

Slice 0158 implements S4.4's `S44-EDIT-HISTORY` retirement. The unused whole-project undo/redo module and its two AC-I7 facts are deleted within the three authorized source files. The implementation is complete; executable verification belongs to the supervisor under the IMPLEMENT worker protocol.

# What's working

- Deleted Storage's History module, including EditHistory, ofPresent, push, undo and redo.
- Removed History.fs from the Storage project and removed undo/redo wording from the existing persistence comment.
- Deleted exactly the two authorized AC-I7 facts and their History import.
- Preserved the recent-files and autosave facts and all shared fixtures unchanged; retained only the existing autosave sentence in the module documentation.

# Tests

No build, test, analyzer, acceptance grep or invariant gate was executed by this worker. The supervisor runs build, elevation, constructor-unit-tests (all discovered), and the twelve s18 invariant gates from the resolved snapshot. Source diff review confirms that both surviving facts and all shared fixtures are unchanged. Ordinary and ignore-CR-at-EOL source diff counts match.

The count below is inherited from checkpoint 156, not measured or authored in this round. Removing two non-parameterized facts implies 1146 constructor cases; the supervisor must measure and record the result. No baseline, census, oracle, gate or checkpoint was edited.

```yaml
gates:
  constructor_unit_tests: 1148
```

# Architecture

The retired history stored only obsolete project-aggregate snapshots. It is deleted without replacement APIs, compatibility paths or new undo history, consistent with spec 0050 §8. No durable format changes in this slice. Autosave and recent-files behavior remain intact.

# Deferred

No implementation work was deferred. Supervisor verification is pending as required by Invariant 6; `commit_ready` is false because the worker does not certify gates.

# Gotchas

- Keep `projectWith`, `preEdit` and `edited`: the retained autosave fact still consumes them.
- HistoryTests remains compiled because it still contains the recent-files and autosave facts.
- The supervisor-owned `.manifest.state.json` was already modified on entry and was not edited by this worker.
- No version bumps, git mutations, cross-repository edits, analyzer configuration changes or notebook edits were made.

# Changelog

- 2026-11-08: Retired whole-project edit history and its two AC-I7 facts while preserving autosave, recent-files behavior and their fixtures.

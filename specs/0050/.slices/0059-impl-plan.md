# Step 0059 implementation plan

Retire STORE_XDUO_0007 exactly as specified by slice 0059 and immutable oracle
S21-RETIRE-EF-PROOF. Remove the five C# proof files and the empty Migrations
directory, its sole F# test consumer and JSON fixture. Prune only the associated
test compile/content/package/project references and their comments. Remove the
obsolete Database project introduction while preserving its properties, NU1903
comment and SQLite package reference.

In AppConfig.fs, remove ConfigKey.databaseProvider and
ConfigKey.mssqlConnectionString with their documentation, remove the storage
proof clause from the module documentation, and document the retained SQLite
connection key as the library database setting. Leave other keys, defaults,
comments and appsettings.json unchanged. No replacement model or tests belong
to this retirement slice.

No target-repository Roslyn removal command was found in the available script,
configuration or documentation inventory. Use the worker protocol's manual
fallback: exact file deletions and narrow text patches, preserving LF endings.
The initially modified supervisor manifest is outside the edit scope.

The removed class declares four theory rows and three facts. Relative to the
inherited 801 constructor tests, the intended result is 794, not a newly measured
count. Under RETIRE_CONTRACT Invariant 6, the supervisor alone runs build,
elevation, constructor-unit-tests, selected AppContextTests/LauncherTests and the
nine s08 invariants, including the retirement acceptance search. Do not change
gate definitions, test baselines or checkpoints. Write the implementation log
and state-of-the-world at the exact supplied paths and report verification as
pending.

## Attempt 02: measured count conflict

Preserve the reviewed retirement and diagnose the supplied gate failure using
the immutable S21-RETIRE-EF-PROOF obligation, checkpoint 57, the deleted test
source, the repository inventory and the supervisor's existing results. Read
ASG's count-credit implementation only to identify the discrepancy; ASG and STL
remain read-only, and no gates are run by this worker.

The constructor suite inventory has method granularity. Four deleted method
identities represent seven executed cases: four theory rows and three facts.
ASG credits one per method, so the 801 checkpoint becomes a floor of 797 while
the required seven-case retirement produces 794. No product or test repair can
satisfy both the exact-seven-fewer acceptance and that floor. Do not add tests,
restore the proof, or alter the spec, oracle, inventory, gates or checkpoints.

Update only this plan, the supplied impl-log and state-of-the-world with the
evidence and the missing authority: the supervisor/operator must resolve the
method-versus-case credit conflict in the gate infrastructure or its accepted
inputs outside this slice. Report commit_ready false. Preserve the inherited
baseline in the handoff YAML; it does not authorize a checkpoint change.

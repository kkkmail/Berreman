# Implementation plan

Implement S43-NO-SCENE-SEAM from slice 0151 and its immutable acceptance oracle.
Remove SceneProxy and its in-memory implementation from Domain/Scene.fs, and
remove AppContext.scenes and its composition. Preserve every scene data type
and step 0147 member. Shorten the affected documentation to the kit-file
contract and trim existing project comments without adding new comments.

Move the four authorized SceneName/SnapMode methods unchanged from
SceneProxyTests.fs into SceneRoundTripTests.fs, preserving both blank-name
theory rows under manual 006. Delete the two proxy-only facts, their helpers,
the old file and its compile entry. Remove only the scene-store assertion from
the existing WireUiFinalCompositionTests fact; preserve its identity and other
assertions. Update that module's documentation and existing project comment.

Expected product/test edits: Scene.fs, OpticalConstructor.Domain.fsproj,
AppContext.fs, SceneProxyTests.fs (delete), SceneRoundTripTests.fs,
OpticalConstructor.Tests.fsproj, WireUiFinalCompositionTests.fs and
OpticalConstructor.Ui.Tests.fsproj. ExperimentDataProxy.fs already has no
obsolete scene reference and needs no edit. Out-of-scope historical comments
and the independent OpenedKit.scenes list remain untouched.

The main risk is losing theory rows or changing a test identity beyond the
declared effects. The four migrated methods comprise five cases; deleting two
proxy facts gives an expected constructor total of 1155 from checkpoint 149's
1157, subject to supervisor measurement. Keep the inherited checkpoint counts
in the handoff; never publish or edit a baseline. Under IMPLEMENT Invariant 6,
the supervisor alone runs build, elevation, constructor-unit-tests, ui-smoke,
selected ui-tests and the eleven s17 invariants after this worker exits.

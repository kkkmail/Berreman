# Implementation plan — 0131

Implement S41-KIT-CONTEXT within the four authorized source files. Extend the shared KitProxyContract with the five prescribed facts (ten concrete identities across its two implementations) and widen the existing KitError rendering fact. Reuse the folder adapters, stepping clock, metadata factory and existing lock/change-check operations.

Add KitReadOnlyErr in Domain/Kit.fs. In Domain/KitFolders.fs, append KitProxy.readOnlyView and KitContext after the existing in-memory implementation. Construct the read-only record explicitly so a future proxy field requires an access-policy decision. Open by reading metadata, taking a lock only for read-write access, then recording stamps; release the acquired lock if stamping fails while preserving the original error. Save through the selected proxy with the injected modified time, and close through its release operation.

Tests will cover the required lifecycle, foreign-lock fallback, all denied write verbs, read passthrough, unchanged contents/paths, deterministic metadata times and non-kit rejection. Extend those same authorized facts to cover failure ordering and stamp-cleanup errors without creating extra test identities. No schema, storage serializer, project file or gate/census changes are needed.

The task's IMPLEMENT invariant 6 assigns all build, test, analyzer and invariant execution to the supervisor. This worker will author code and tests, inspect its source changes and write the required log/state files, without claiming gate results or authoring measured baselines. The supervisor-owned manifest change is left untouched.

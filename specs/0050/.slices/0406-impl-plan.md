# Step 0406 — impl plan

Publish calibration as a pure update of the kit's items (spec 0050 S11.5; 001 §I, §L, §P). A fit run's
item-owned fitted values become the items' next revisions, each a `CalibratedParameter` with its σ and its
fit run. A link's follower takes its leader's state.

## Files

1. New `OpticalConstructor.Domain/StudyPublishing.fs`, `module StudyPublishing` in
   `namespace OpticalConstructor.Domain`. It is compiled right after `FitRuns.fs` (one `<Compile>` line, no comment).
   - `PublishError` has six two-word `*Err` cases. `describe` gives the texts the brief lists.
   - `type Study with member this.calibratedItemsWith (run : FitRun) (items : Item list) : Result<Item list, PublishError>`
     checks in the brief's order:
     1. a foreign run;
     2. an unknown item, over every item-owned entry in entry order;
     3. an undetermined σ, read from `run.results.sigmas` by `FitEntryRef`;
     4. no item-owned entry.
   - Then it builds the follower states from `this.links`. Per item, it takes the entries first, then the
     followers. It drops what the item already holds. Nothing left is `CalibrationAlreadyPublishedErr`.
   - Each remaining item goes through `item.publishRevision`, with an `ItemError` mapped to `PublishItemErr`.
   - It writes nothing and reads no clock. It adds no other type or member, only local lets.
2. `OpticalConstructor.Domain.fsproj`: one compile line after `FitRuns.fs`.
3. New `OpticalConstructor.Tests/StudyPublishingTests.fs`, `module StudyPublishingTests`. It is appended to the
   end of the Tests compile list.
   - The fixtures are CPL1, CPL2 and Phone, made through `ItemDraft` as `StudyTests` makes them.
   - The study record is QZ-LR, with the CPL2 → CPL1 retardance link.
   - The FitRun record has the item-owned retardance entry and the session-owned gain entry.
   - It has the four facts the brief names, by their exact identities.

## Risks

- ELEV rules on new names. A grep shows no collision for the type, its six cases or the member.
  Local helper signatures avoid bare primitives beside our types where possible.
- An ambiguous order: are the unknown-item and σ checks done in phases or per entry? I follow the brief's
  sentence order and run them as phases: every entry's item first, then every entry's σ. This is recorded in the
  impl-log.
- Constructor suite time: the step's deadline is 180 s, and step 0405 measured 159 s. Four cheap pure facts add
  little.

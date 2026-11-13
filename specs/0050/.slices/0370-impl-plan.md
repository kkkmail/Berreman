# Step 0370 — impl plan

**Goal.** Add the pre-fit "data changed since attach / since the last fit run" flag (spec 0050 S11.2; 001 §E):
`DataChangeFlag` and `MappedAttachment.changeWith` in `OpticalConstructor.Domain/PrefitChecks.fs`, after
`prefitCheckWith` (step 369), plus one fact in `PrefitCheckTests`.

**Approach.**

1. `DataChangeFlag` — four cases (`DataUnchangedFlag`, `ChangedSinceAttachFlag of current : ContentHash`,
   `ChangedSinceFitFlag of current : ContentHash`, `MissingDataFileFlag`) with a `text` render member. Derived, never
   stored, so no `value` tag.
2. `type MappedAttachment with member this.changeWith (status : DataAttachmentStatus) (fitted : DataAttachment list)`:
   missing → `MissingDataFileFlag`; changed → `ChangedSinceAttachFlag current`; unchanged → `ChangedSinceFitFlag` of the
   attachment's own hash when `fitted` holds the same path at another hash, else `DataUnchangedFlag`.
   Reuses S4.2's `DataAttachmentStatus` (step 132) as the since-attach input; nothing new is computed there.
3. `PrefitChecks.fs` opens `Kit` and `LibraryEntryFiles` for `DataAttachmentStatus`, `DataAttachment`,
   `MappedAttachment` and `ContentHash`.
4. Test `a data file is flagged when it went missing, changed since attach, or changed since the last fit run read it`
   over test-local attachments and hashes, covering every row of the how-to and the four texts.

**Files.** `Berreman/OpticalConstructor/OpticalConstructor.Domain/PrefitChecks.fs`,
`Berreman/OpticalConstructor/OpticalConstructor.Tests/PrefitCheckTests.fs`.

**Risks.** Name collisions under ELEV016/019 (checked: no existing `DataChangeFlag`, `changeWith` or the four case
names); opening `Kit` in `PrefitChecks.fs` shadowing a name already in use there (build will tell).

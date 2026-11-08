# Step 0171 — IMPLEMENT — impl-plan

## Approach

Two history verbs for the start screen (spec 0050 S5.2; 001 §F Locate… / Remove from history, §P StarKit, UnstarKit,
ForgetKit).

1. `KitHistory.relocate (missing : KitFolder) (found : KitFolder) : KitHistory`, an intrinsic member beside `forget`.
   - It finds the first entry matching `missing` through the one private comparison `tryMatchFolder`.
   - None found: the history is answered unchanged.
   - Found: that entry, in its own place, takes `found` as its folder and keeps everything else. Every other entry
     matching `found` is dropped, so a folder is listed once.
   - The moved entry is identified by its index, so the case where `missing` and `found` name the same folder keeps
     it. No disk is touched.
2. `type KitHistoryProxy with member this.changeHistory (change : KitHistory -> KitHistory) : Result<KitHistory, KitHistoryError>`,
   declared after `createInMemory`.
   - It reads with `readHistory ()`, applies `change` and writes with `writeHistory`.
   - It answers the written history, or the write's error.
   - No retry, cache, lock or new proxy field.

## Tests

- `KitHistoryTests` gets two facts over `mixed`:
  - `relocate moves a kit's entry to the found folder and keeps its star, name, time and place`;
  - `relocate onto a folder the history holds keeps one entry for that folder`.
- `KitHistoryProxyContract` gets `changing the history writes the change and answers what was written`. Both derived
  classes run it, which is 2 cases.
- `KitHistoryProxyFolderTests` gets `a change the preferences folder cannot take is KitHistoryWriteErr, never a throw`.
- That is +5 in all.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryProxyTests.fs`

## Risks

- Under invariant 6 nothing compiles locally, so every form copies one already compiling in `KitHistory.fs`.
- **ELEV019.** `changeHistory` must be unique on `KitHistoryProxy`. A grep finds no other `changeHistory` or
  `relocate` in `Berreman/`.
- **The folder test.** With a file at `Preferences`, `readHistory` sees no `kit-history.json` (`File.Exists` is
  false) and answers empty. `writeHistory` then fails in `createPreferences` with a reason naming the preferences
  path, which is the existing write test's mechanism.

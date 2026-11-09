# Step 0198 — impl plan

**Goal.** Send and receive as two Domain members over the `KitArchiveProxy` seam, plus the `KitZipPicker` record
the two dialogs of step 200 will fill (spec 0050 S6; 001 §H, §E, §P).

**Files.**
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitArchive.fs`
  - on the existing `type KitArchiveProxy with` extension, beside `createInMemory`:
    - `sendKit kitFile kit zip observer` — `kit.tryKitPathOf zip` = `Some _` → `KitZipInsideKitErr` naming the zip,
      nothing listed or written; otherwise `listKitTree kit` then `writeKitZip` of `kitFile.sentEntries tree`.
    - `receiveKit zip target observer` — `listZipEntries zip`; no root `KitTreeFile` named `KitFile.fileName`
      (ignoring case) → `KitZipHoldsNoKitErr` naming the zip, nothing written; otherwise `extractKitZip` of the
      entries whose `pathRole` is `KitContentPath`.
  - after it: `[<ReferenceEquality>] KitZipPicker = { pickZipToWrite; pickZipToOpen }` with `static member none`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitArchiveProxyTests.fs` — five new facts on
  `KitArchiveProxyContract` (inherited by `KitArchiveProxyTests`), over `sampleKit` plus `.oc/lock` and
  `.oc/planner/c.json`, with a kit file whose `sendExclude` is `**/*.dng`.

**Risks.** ELEV012 (a two-case union mapped to `bool`): every filter over `KitTreeEntry` / `KitPathRole` is written as
`List.choose` / `List.tryPick`, never a `bool` lambda. Record-literal inference between `KitZipJob` and
`KitUnzipJob` (both have `zip` / `entries`): each job is bound with a type annotation.

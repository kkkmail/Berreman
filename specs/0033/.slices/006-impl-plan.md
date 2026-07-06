# Slice 006 — impl plan

## Goal

IMPLEMENT_CONTRACT STORE_XDUO_0001 `MaterialProxy` (kind proxy → `implemented`
lifecycle): replace the step-003 validate-only mock with the REAL stateful
in-memory store — `MaterialProxy.createInMemory` taking
`samplesReferencing : MaterialId -> Sample list` and closing over a
`ref Map<MaterialId, MaterialEntry>` seeded from
`MaterialLibrary.builtInEntries`, mutation confined to the closure (the IO
boundary) — plus deterministic round-trip / referenced-block tests (fixed
Guids, no IO) in `OpticalConstructor.Tests`.

## Placement constraint (drives the file layout)

The pinned signature takes `MaterialId -> Sample list`, but `Sample` lives in
`ElementId.fs` (`Library` module), which compiles AFTER `MaterialLibrary.fs`.
So the real store CANNOT sit next to the mock it replaces: it lands in
`ElementId.fs` as a type augmentation `type MaterialProxy with static member
createInMemory …` (the step-005 `SampleProxy.createInMemory` precedent —
same anchor, same repo, accepted). The pure pieces the store reuses stay in
`MaterialLibrary.fs`.

## Approach

1. **TDD first** — rewrite `OpticalConstructor.Tests/MaterialProxyTests.fs`
   against the not-yet-existing `MaterialProxy.createInMemory`; the compile
   error is the red state (a missing production symbol counts as red per the
   worker base).
   - Read surface (fresh store, `fun _ -> []` referencing): list / search
     text / category / dispersion-facet partition (silicon + langasite) /
     facet composition / tryGet hit + miss — the step-003 expectations,
     now against the stateful store.
   - Round-trips: add-then-list (store grows, tryGet returns it), stateful
     duplicate-add rejection (same minted id twice), seeded-id duplicate-add
     rejection, update-then-get (updated record read back), unknown-update
     rejection, remove-then-search (gone from list + search; remove again →
     `UnknownMaterialId`), blank-name add/update → `InvalidMaterial` and the
     store unchanged.
   - The referenced-material hard block (the acceptance): compose the store
     with the step-005 `SampleProxy` store via the new `samplesReferencing`
     lookup; `removeMaterial MaterialIds.glass152` (referenced by seeded
     samples) → `Error (MaterialStillReferenced reason)` with the reason
     NAMING the referencing samples; materials store unchanged AND samples
     store unchanged (never cascades). `removeMaterial MaterialIds.glass200`
     (no seeded sample references it) → `Ok`, entry gone.
   - The lookup is LIVE: `glass175` blocked; remove its one referencing
     sample (`SeedSamples.glassFilm600`) through the SampleProxy; remove
     again → `Ok`.
   - `SampleStructure.referencedMaterials` covers every reference position:
     single film layer, `Repeated` cell, substrate plate, lower half-space
     (pure member tests over the seeds).
   - Every test builds a FRESH proxy (the step-005 gotcha: a shared stateful
     proxy is order-dependent under xUnit); the step-003 stub six-function
     acceptance test is kept verbatim.

2. **`OpticalConstructor.Domain/MaterialLibrary.fs`** — the pure pieces:
   - DELETE the step-003 validate-only mock `createInMemory` (grep: its only
     consumers are the step-003 tests).
   - Add `byQuery : MaterialQuery -> MaterialLibrary -> MaterialEntry list` —
     the one pure search seam: reuses `byNameContains`, then `byCategory`,
     then the `DispersionFilter` facet over the existing private
     `hasDispersion`.
   - `validateEntry` loses `private` (the store in `ElementId.fs` reuses it —
     an optional extension cannot reach a module-private binding in another
     file).
   - Doc comments on `MaterialProxy` / `hasDispersion` / `validateEntry`
     updated to the implemented lifecycle and the new producer location.

3. **`OpticalConstructor.Domain/ElementId.fs`** — the store + composition:
   - `SampleStructure.referencedMaterials : Set<MaterialId>` member — films
     (a `Repeated` cell counted once), substrate plate, lower half-space.
   - `type MaterialProxy with static member createInMemory
     (samplesReferencing : MaterialId -> Sample list) : MaterialProxy` after
     the `SampleProxy` augmentation: `ref Map<MaterialId, MaterialEntry>`
     seeded from `builtInEntries` (elevated `MaterialId` is the Map key);
     reads answer from the current map; `searchMaterials` = `byQuery` over
     the current entries; `addMaterial` persists and hard-blocks a held id
     (`DuplicateMaterialId`); `updateMaterial` replaces / rejects unknown;
     `removeMaterial` consults `samplesReferencing` — any referencing sample
     ⇒ `MaterialStillReferenced` naming them, never cascades, never silently
     deletes; blank-name validation kept (`InvalidMaterial`).
   - Module-level `samplesReferencing : SampleProxy -> MaterialId ->
     Sample list` — the composition-root lookup backed by the step-005
     samples store (filters `listSamples` by `referencedMaterials`).

4. **Registry** — `specs/0033/.contracts-json`: set
   `STORE_XDUO_0001.lifecycle` to `implemented`, `implementStep` to 6 (file
   is CRLF — arc-runner-owned, edit in place without re-ending lines).

## Risks

- The augmentation here is an OPTIONAL extension (type declared in another
  file), unlike step 005's intrinsic one — static extension members are
  fine, but call sites need `open …Domain.Library`; it also cannot reach
  `MaterialLibrary`-private bindings, hence de-privatizing `validateEntry`
  and keeping `hasDispersion` private behind the new pure `byQuery`.
- The slice's parenthetical defines dispersive as "epsWithDisp is the func
  case"; the step-003 `hasDispersion` also checks mu/rho func cases. Every
  dispersive entry (silicon, langasite) carries the eps func case, so the two
  definitions coincide on all data; keeping the broader step-003 classifier
  is a compatible superset — recorded in Gotchas.
- `samplesReferencing`'s signature carries no error channel; the in-memory
  `listSamples` is total (always `Ok`), the degenerate `Error → []` branch is
  documented — a future erring store must supply a conservative lookup.
- A shared stateful proxy across tests is order-dependent — fresh proxy per
  test.
- `listMaterials` returns Map (Guid) order, not seed order — no test asserts
  order (same as the step-005 samples store).
- LF-only endings on every touched source file; the registry stays CRLF.

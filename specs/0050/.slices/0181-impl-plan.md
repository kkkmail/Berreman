# Step 0181 — impl plan

## Approach

One spelling per kit folder (spec 0050 S5.3; 001 §E "one window per kit", §F one history entry per folder).

1. `OpticalConstructor.Domain/Kit.fs`: in the `type KitFolder with` block, beside `tryKitPathOf`, add
   `member this.canonical : KitFolder`. It turns every `\` into `/`, trims every trailing `/` and upper-cases the rest
   (`ToUpperInvariant`). It touches no disk and resolves no relative path. The doc is one line citing spec 0050 S5.3 and
   says it is a key, never a path to show.
2. `OpticalConstructor.Domain/KitHistory.fs`: `KitHistory.tryMatchFolder` compares `entry.folder.canonical` with
   `folder.canonical`. The `String.Equals(…, OrdinalIgnoreCase)` comparison is deleted. `star`, `unstar`, `forget`,
   `bump` and `relocate` keep their code. `open System` stays, because `entriesMatching` still uses `StringComparison`.
3. Tests:
   - `KitTests.fs` gains `two spellings of one kit folder share one canonical form`.
   - `KitHistoryTests.fs` gains `a kit is matched by its folder in any spelling`, over `mixed`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs`

## Risks

- ELEV: `canonical` is an extension member on `KitFolder`, and no other `canonical` member is declared on it (ELEV019).
  The wrap `FolderName (… .ToUpperInvariant())` is a fresh elevation at origin, not a `.value` re-wrap (ELEV001/005).
- Existing history facts pass folders as `C:/KITS/A`, `C:\KITS\A`, `C:\kits\f{n}`. Their canonicals keep the same
  matches as the old ordinal-ignore-case rule, so every earlier assertion holds.
- No gate is run by this worker (Invariant 6); the gate engine runs build, elevation, constructor-unit-tests and the
  fourteen s21 invariants.

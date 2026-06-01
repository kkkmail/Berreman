# Architecture critique -- 013.slice-md cycle 2

## Summary

Cycle 2 is a tight, surgical follow-up that discharges exactly the two items
the cycle-1 judge routed back on — the unproven `RecentFiles.bump` MRU
invariant and reuse finding F1 — and touches nothing else. Both fixes landed
cleanly and behaviour-preservingly; the remaining items from cycle 1 are the
same spec-acknowledged interpretation calls the judge already adjudicated as
non-blocking. My read is unchanged: ship.

## Consistency

- **F1 is collapsed correctly.** The hand-rolled `categoryName`/`categoryOfName`
  `MaterialCategory`↔string map is gone; `MaterialEntryDto.category` is now the
  `MaterialCategory` DU itself (`Report.fs:39`), import is a plain typed
  `Deserialize<MaterialEntryDto list>` (`Report.fs:113`), and export boxes the
  DU value straight into the dict (`Report.fs:139`). Both directions now ride the
  single shared `ProjectJson.options` (`.WithUnionUnwrapFieldlessTags()`,
  `ProjectJson.fs:77`), which is precisely the "one JSON stack" mandate the
  reuse critic and judge asked for. No second hand-maintained DU serialization
  survives, so a future `MaterialCategory` case can no longer silently drift.

- **One residual asymmetry, now sanctioned.** Import is a typed deserialize while
  export still hand-builds a `Dictionary<string, obj>` and leans on
  System.Text.Json's runtime-type dispatch for the boxed `category` and on the
  `Some d`/`None` branch to omit a null `description` (`Report.fs:132-143`). This
  is the exact shape the cycle-1 reuse critic endorsed (the boxed-DU path is the
  one stack; the `description`-omit dict has no shared helper to point at and is
  recorded in the SoW gotchas). Worth noting only because the export path's
  correctness now rests on STJ serializing `object`-declared dict values by their
  runtime type — a real but stable STJ guarantee, and proven by the passing
  `exportMaterials` validation test (`ExportImportTests.fs:56-72`).

## Risks

- **The cycle-1 coverage gap is closed.** `RecentFiles.bump` now has a direct,
  IO-free fact (`HistoryTests.fs:67-85`) that exercises all three legs of R-1's
  invariant: case-insensitive dedup of a re-pushed path, most-recent-first
  ordering, and truncation to `maxRecent = 16` via a 20-path fold. This is the
  test the judge's `retry_hint` called for, and it lands at the right seam — the
  pure helper, not the app-data IO boundary `loadRecent`/`pushRecent` reasonably
  leave unexercised. The constructor-unit-tests count moves 147 → 148, matching
  the SoW's baseline block.

## Spec fit

No scope change in cycle 2. The signatures still match R-1…R-4 exactly, and the
two fixes are additive (one test) plus subtractive (deleting the category map) —
no new module, abstraction, or dependency was introduced, consistent with
binding rule 6 (minimum implementation). The SoW's "Files modified" list and
changelog accurately describe the cycle-2 delta against the cycle-1 diff.

## Evolvability

The items I flagged in cycle 1 are unchanged and remain advisory, not defects.
Recording them once more so the judge has them in one place without re-deriving:

- **`diffRevisions` still ignores `beamTree`/`defaultUnit`** (`Report.fs:244-250`),
  so the test's `p1`/`p2` revisions — which differ *only* in the root
  `defaultUnit` — diff to empty `systems`/`sources`. AC-I11 only requires a
  `ProjectDiff` with no VCS call, and the added-system case
  (`ExportImportTests.fs:113-115`) produces a genuine `Added`, so the gate is
  honestly met; the diff is just narrower than "two revisions differ" implies.
  Spec-acknowledged (SoW gotcha; `materials` is hard-wired `[]` because the
  aggregate carries no top-level materials list).

- **Positional "stable identity" and structural `<>` on `OpticalSystem`**
  (`Report.fs:218-237`) remain pragmatic reads: positional indices aren't stable
  under insertion/reorder, and the `<>` comparisons are safe only because
  `OpticalSystem` is closure-free tensor data today. Both are owned by comments
  and were judged out-of-scope for a §I.5–I.8 slice; a future identity-bearing
  slice should revisit them.

- **F2 (re-hardcoded schema path in `materialEntrySchema`, `Report.fs:62-63`) and
  `Report.generate` swallowing `exportChartImage`'s `Result` (`Report.fs:331`)**
  are likewise carried over and were explicitly left as-is by the cycle-1 judge.
  Neither moved in cycle 2, which is the correct call — the route-back hint
  scoped the work to the bump test and F1 only.

## Bottom line

Cycle 2 did exactly what was asked and nothing more: it added the
behaviourally-meaningful `bump` test and collapsed F1 onto the shared JSON stack,
both behaviour-preserving and covered by green gates (build; unit-tests 84;
constructor-unit-tests 148). Every load-bearing binding constraint still holds —
one JSON stack, immutable pattern-matched history with no `.Value`/`.IsSome`, no
rasterizer in Storage, JSON-not-`.binz` revisions, FSharp.Data for external CSV.
The remaining findings are the same spec-acknowledged interpretation calls the
cycle-1 judge already declined to route on. I have no gate authority, but my read
is a clean pass — there is no open item in this slice's scope left to fix.

# 004 — E007 blocked: a duplicate-name finding cannot close bottom-up without rewriting its higher twins' entries

**Batch:** `batch-008`, row **E007** (Controls: `WindowLayout`, `RotationControls`, `ElementPaletteControls`,
`RayPositionControls`, `TablePositionControls`; 97 findings, R2). **State:** `blocked`.
**Done in this batch:** 88 of the row's 97 findings, plus 2 outside it (3262 → 3172), gates green
(the row's note in `002-elevation-ledger.jsonl` has the details).
**Left:** 9 findings. They cannot be closed under the rules as written.

## 1. What blocks

The 9 findings are the row's duplicate-name findings on the three Elmish shapes:

| Rule | Name | Row files | Recorded twin list |
|---|---|---|---|
| ELEV014 | `State` | ElementPalette, RayPosition, TablePosition | every EARLIER `State` |
| ELEV014 | `Handlers` | ElementPalette, RayPosition, TablePosition | every EARLIER `Handlers` |
| ELEV015 | `view` | ElementPalette, RayPosition, TablePosition | the 4 NEAREST earlier `view`s |

The analyzers write the twins into the message, and the gate (`Elevation/Run-ElevationGate.ps1`) keys a known
finding on `rule | file | message`. So any fix of a lower declaration changes the message of every higher one that
lists it. Each of those is then a new finding plus a stale one, and the gate fails.

- **`State` (13 declarations in Controls) and `Handlers` (11).** The messages list all earlier twins.
  - Renaming the row's three `State`s rewrites the `State` entries of all 9 later Controls files (Renderer, Ribbon,
    Library, LayerBands, Experiment, Materials, SampleLibrary, Category, FacetedTree).
  - Renaming the row's three `Handlers` rewrites 7 later `Handlers` entries.
  - Clearing those instead means renaming those types too. That in turn rewrites about 110 more entries in the same
    9 files that quote `State.` / `Handlers.` (their field findings). In effect, rows E008–E012 (573 findings)
    would be done inside E007.
- **`view` (27 declarations: 13 Controls, 8 Ui, 6 TestWindows).** Each message lists the 4 nearest earlier twins.
  - Removing the row's three changes Renderer, Ribbon, Library and LayerBands.
  - Clearing those changes the next four, and so on up the chain: all 23 later `view` entries, into rows E025–E041.

Top-down works without a cascade. Renaming the HIGHEST declaration of a name changes no other message, because a
message lists only earlier twins. The plan drains bottom-up (§4, §5, the task's "in id order, never skip ahead").
§5 also expects "the higher side of a duplicate name, whose finding goes when the lower side is renamed". That
holds only for a name declared twice. With three or more declarations, the higher side's finding stays and its
message changes.

The same defect waits in every later row with a multi-declaration name: ELEV014 `Model` 14, `Msg` 14; ELEV015
`init` 15, `update` 15, `readout` 11; the shared ELEV016 case names. STL's 0229 loop never met it, because its gate
compared per-file counts (`Compare-ElevCensus`, "no other measured file risen") rather than message text.

## 2. What this batch did around it

Everything in the row that does not touch the cascade is closed, and nothing was deferred silently. The R2 rename
was applied where it could be. `RotationControls.Axis` became `RotationAxis`; the lower side was chosen, inside
one assembly, because `TablePositionControls.Axis` is quoted by ExperimentChart's two ELEV016 entries and by
TableAndElementRotationView's ELEV013P `ofPositionAxis` entry. `TablePositionControls.AxisLock` was collapsed onto
`RotationControls.AxisLock`, the same animal, so the lower survives. `readoutText` moved onto each bar's `State`.
The 9 entries stay recorded and unchanged, and the gate is green.

## 3. The question

How should a duplicate-name finding (ELEV014 / 015 / 016) whose message lists several twins be closed?

- **A. Rewrite in place (recommended).** When a change renames a lower twin, the higher entries whose finding
  persists are rewritten to the analyzers' new text in the same change. Only the enumerated twins may differ; the
  rule, file, declaration and count stay. This is ruled a lowering, not an addition.
  - E007 then renames its 9 per R2 (`ElementPaletteControlsState`, `ElementPaletteControlsHandlers`,
    `elementPaletteControlsView`, and the same for RayPosition and TablePosition). It rewrites about 16 ELEV014
    and 4 ELEV015 messages and closes the row.
  - Cost: one sanctioned exception to "never add an entry".
- **B. Key the gate on the declaration.** For ELEV014 / 015 / 016, the gate compares `rule | file | name | here`
  and leaves out the twin list (a change to `Elevation/Run-ElevationGate.ps1`; the analyzers and
  `.elevation-analyzers.json` stay untouched). Then E007 closes its 9 with no list edit beyond deletion.
- **C. Drain duplicate names top-down.** The 9 (and every later multi-twin duplicate) leave their rows. They close
  in new rows that rename each name's declarations highest-first, after the tier rows. This needs new queue rows,
  which only the operator can add, since `002` §5 says never regenerate.
- **D. Do the cascade inside E007.** Rename all 13 Controls `State`s, 11 `Handlers` and 27 `view`s now. Clearing
  the rewritten `State.` / `Handlers.` field findings brings rows E008–E012 into E007. This is not one batch's work,
  and it breaks "close exactly ONE row".

Under A or B, the next batch finishes E007: 9 renames, their callers (Ui `TableAndElementRotationView`, the TestWindows
views, the tests), `elevation-full`, and the three test gates. Under C, E007 closes as soon as the operator moves
the 9 to the new rows. Under D, the loop needs a new batch budget.

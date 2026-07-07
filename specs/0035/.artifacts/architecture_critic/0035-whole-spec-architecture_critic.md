# Architecture critique -- whole-spec cycle 1

## Summary

The worker diff this cycle is a **spec-narrative reconciliation**, not code:
four claim-check fixes to `specs/0035/.spec-md` plus `.history` / `cost.json`
bookkeeping. The edits are architecturally inert (three pointer re-anchors, one
pointer bump), and one — the empty-sample test strategy in Part D — actually
*sharpens* spec-fit. Against the whole spec the design remains clean and
unusually well-cited; every file:line I re-checked against the live tree
(`Report.fs:30-39/96/155`, schema `:84-87`, `MaterialLibrary.fs:67/119/135/146`,
`Dispersion.fs:11-12/306-308`, `ElementId.fs:307/329/337`, `SampleProxyTests.fs:120`)
is accurate. There is **one substantial standing concern, untouched by this
diff**: Part A re-types `MaterialEntry.category` to `CategoryId`, but that field
is serialized and schema-constrained by `OpticalConstructor.Storage`, which no
step touches and which §0.3 disclaims — so step 1 as sequenced cannot green its
own `build` gate.

## Layering

Part E / step 15 makes the strongest architectural call in the spec and makes it
correctly. The core `Berreman.Dispersion` module cannot reference the Domain's
`DispersionModel` without inverting the dependency direction, so the new
evaluated case on `EpsAxisDispersion` carries a `WaveLength ->
ComplexRefractionIndex` closure built *by* `toEpsAxis` from the Domain's
`evaluate` — mirroring the engine's own `EpsWithDisp of (WaveLength -> Eps)` in
the same file (verified at `Dispersion.fs:11-12`). Rejecting the preliminary
spec's `EvaluatedModel of DispersionModel` sketch as unbuildable is right. Part
C's editor-only scope (engine `toRhoWithDisp` / `toMuWithDisp` UNCHANGED) touches
the correct layer. No inward/outward edge is violated.

## Separation of concerns

Part A's four-way split — mutating `CategoryProxy` (Domain IO seam), pure
`CategoryEditor` edit model (Domain), domain-free `CategoryControls` (Controls),
`CategoryEditorWindow` host (TestWindows) — mirrors the existing `MaterialProxy`
/ `MaterialComplexityEditor` / `LibraryControls` / `MaterialEditorWindow`
decomposition. Keeping mutation inside the `createInMemory` closure and pushing
the referencing lookup behind a `materialsReferencingCategory` seam (the
`samplesReferencing` precedent, `ElementId.fs:740`) is the correct shape.

## Consistency

The `CategoryProxy` shape (`[<ReferenceEquality>]` record of camelCase
`Result`-returning functions, `createInMemory` over a private mutable `Map`,
typed `CategoryError` with per-case `reason`) matches `MaterialProxy` /
`SampleProxy`; the elevated-id pattern (`CategoryId` Guid single-case DU with
fixed literal seed Guids) matches `MaterialId` / `MaterialIds`
(`MaterialLibrary.fs:27,51`); the two-case `CategoryVisibility` / `CategoryOrigin`
DUs correctly avoid naked bools. One minor gap: `Ribbon.Bay.name` /
`Ribbon.State.selected` remain bare `string`, and step 8 keys the single content
slot by that string. Pre-existing, but since Part B already re-works `Bay`, a
`BayName` elevation would fit the "elevate every primitive" discipline the rest
of the spec honors. Not blocking.

## Spec fit

**The Part A category re-type collides with the Storage boundary, and no step
closes the gap — this is the one finding I would want resolved before
implementation starts.** Step 1 re-types `MaterialEntry.category`
(`MaterialLibrary.fs:119`) and `MaterialQuery.category:368` to `CategoryId`, with
`touches = [Domain, TestWindows, Tests]`. But that field is serialized in
`Storage/Report.fs`, which I read directly:

- `MaterialEntryDto.category : MaterialCategory` (`Report.fs:39`) binds the DTO
  field to the domain type *by type*, and `dtoToEntry` assigns `category =
  dto.category` (`:96`). Re-typing the domain field to `CategoryId` breaks this
  assignment; the `build` gate compiles the whole `.slnx`, so **step 1 will not
  compile**. The export path `dict.["category"] <- box e.category` (`:155`)
  breaks the same way.
- The module comment `Report.fs:30-34` states outright that `category` *is* the
  `MaterialCategory` DU, round-tripped by `ProjectJson.options`'
  `.WithUnionUnwrapFieldlessTags()` — a mechanic that unwraps a *fieldless* DU to
  a plain string; a record or a Guid does not round-trip that way.
- The `materialEntry` `$def` hard-codes `"category": { "enum":
  ["Glass","Metal","Semiconductor","Crystal","Vacuum"] }`
  (`optical-constructor-project.schema.json:84-87`), enforced by the validation
  path in this same module. A `CategoryId` Guid fails this enum.

§0.3 says the spec "adds NO JSON-schema, versioning, or import/export work" — but
the pre-existing material-export path unavoidably serializes this field, so §0.3
and step 1 are in tension. Part A needs an explicit decision: either (a) scope
`Report.fs` + the `materialEntry` `$def` into step 1 (persist `CategoryId` as a
`uuid`-format string, resolve the display name through the catalogue on import,
and narrow §0.3 to the *new* category catalogue), or (b) drop `category` from the
persisted DTO/schema. Leaving it implicit forces the worker to break the build or
silently do the schema/import-export work §0.3 forbids.

On the diff itself: the four reconciliations correctly execute what the
claim-check asked, and edit #3 is a genuine spec-fit improvement — it drops the
un-anchored "existing tests… are updated" claim and re-anchors the rule to a NEW
negative case in `SampleProxyTests.fs:120` (`addSample` of a films-empty /
substrate-`None` sample → `InvalidSample`). I confirmed the anchor: the blank-name
rejection sits at that line, and every seed uses `filmStructure` / `plateStructure`
(`ElementId.fs:329/337`), so none is structurally empty — the corrected strategy
is accurate. Parts B–E otherwise map cleanly onto their cited seams.

## Evolvability

Step 15's evaluated case adds an unserializable closure to `EpsAxisDispersion`
(`Dispersion.fs:306-308`), whose stated purpose is to be the *serializable*
term-data form. The four transcendental models will now lower to that closure.
This regresses nothing today (the eps segment tree is never written to disk —
`MaterialEntryDto` persists metadata only), but it corners a future "persist the
dispersion tree / §D.9 mapping" slice for exactly those four models. The
constraint is arguably inherent — transcendental models genuinely are not finite
term sums — so this is a note to carry, worth one sentence in the step's `how_to`
acknowledging the evaluated case is deliberately outside the serializable
envelope.

## Risks

- **`CustomEquality` / `NoComparison` propagation (step 15).** `EpsAxisDispersion`
  currently carries no equality attribute (`Dispersion.fs:306`); §E commits to a
  `[<CustomEquality>]`. If `NoComparison` rides along, it silently strips
  auto-generated `comparison` from every containing type (`EpsWithDispValue`,
  `MaterialComplexity`, `DispersionModel`, `MaterialEntry`). A quick search found
  no `Set` / `Map`-key / `sort` use of these types, so it looks benign — but the
  step should state comparison is confirmed-unused, since a later slice that sorts
  or Set-keys any of these would hit an opaque error far from the change site.
- **Part B retires the Details-LAST pin (step 8).** That pin exists because moving
  the Library bay last previously regressed its sample-row layout. Step 7's
  keyed-slot hosting is the correct fix for the documented FuncUI recycling hazard
  (`Ribbon.fs:84-89`), and sequencing the hosting fix strictly before the reorder
  is right — but the `ui-smoke` gate must actually render a frame of the reordered
  full-surface Materials/Library bays; keep that acceptance.
- **Step-18 MSB3277 is conditional.** "If the build emits MSB3277 … resolve it" is
  defensible, but a purely conditional acceptance can pass vacuously. Fine as
  written provided the `build` gate is the real arbiter.

## Bottom line

Architecturally the spec is clean and unusually well-cited, and this cycle's
diff only tightens it — the reconciliations are inert or improving, so nothing in
the diff itself warrants a re-spawn. The one thing I would hold for a spec
revision is the Part A category re-type: as sequenced it breaks
`OpticalConstructor.Storage` (`Report.fs:39/96/155`) and the `materialEntry` JSON
enum, which no step touches and §0.3 disclaims, so step 1 cannot green its `build`
gate without unscoped work. That is a suggestion, not a verdict — the code judge
weighs it against the gate results and the reuse critic. The most defensible
in-flight interpretation is to fold a `Report.fs` DTO + schema `category`-field
update into Part A and narrow §0.3 to the new category catalogue only.

# Architecture critique -- whole-spec cycle 1

## Summary

This is a well-grounded spec: every file:line pointer I spot-checked
(`MaterialLibrary.fs:67/119/146`, `Dispersion.fs:11-12/306`,
`DispersionModels.fs:486/605/666`, `Ribbon.fs:84-103`, `Report.fs`) is
accurate, the layering reasoning in Part E is correct, and the
proxy/edit-model/control decomposition in Part A follows the codebase's
existing seams faithfully. There is **one substantial concern**: Part A
re-types `MaterialEntry.category` from the fieldless `MaterialCategory`
union to `CategoryId`, but that field is persisted and JSON-schema-constrained
by `OpticalConstructor.Storage`, which **no step touches** and which binding
constraint §0.3 explicitly disclaims — so step 1 as sequenced cannot pass its
own `build` gate.

## Layering

Part E / step 15 makes the strongest architectural call in the spec and makes
it correctly. The core `Berreman.Dispersion` module cannot reference the
Domain's `DispersionModel` without inverting the project's dependency
direction, so the new evaluated case on `EpsAxisDispersion` carries a
`WaveLength -> ComplexRefractionIndex` closure built *by* `toEpsAxis` from
`DispersionModels.evaluate` — exactly mirroring the engine's own
`EpsWithDisp of (WaveLength -> Eps)` case in the same file
(`Dispersion.fs:11-12`). The spec is right to reject the preliminary spec's
`EvaluatedModel of DispersionModel` sketch as unbuildable. Part C's
editor-only scope (engine `toRhoWithDisp`/`toMuWithDisp` UNCHANGED) is likewise
the correct layer to touch. No inward/outward edge is violated.

## Separation of concerns

Part A's four-way split — mutating `CategoryProxy` (Domain IO seam), pure
`CategoryEditor` edit model (Domain), domain-free `CategoryControls`
(Controls), and the `CategoryEditorWindow` host (TestWindows) — mirrors the
`MaterialProxy` / `MaterialComplexityEditor` / `LibraryControls` /
`MaterialEditorWindow` decomposition already in the tree. Keeping mutation
inside the `createInMemory` closure and pushing the `materialsReferencingCategory`
lookup behind a seam (the `samplesReferencing` precedent, `ElementId.fs:740`)
is the right shape. Good.

## Consistency

The `CategoryProxy` shape (`[<ReferenceEquality>]` record of camelCase
`Result`-returning functions, `createInMemory` closing over a `ref Map`, typed
`CategoryError` with per-case `reason`) matches `MaterialProxy`/`SampleProxy`
precisely, and the elevated-id pattern (`CategoryId` = Guid single-case DU with
`.value`/`create`, fixed literal seed Guids in a `CategoryIds` module) matches
`MaterialId`/`MaterialIds` (`MaterialLibrary.fs:27,51`). The two-case
`CategoryVisibility`/`CategoryOrigin` DUs correctly avoid naked bools. One
consistency gap: `Ribbon.Bay.name` and `Ribbon.State.selected` remain bare
`string` (`Ribbon.fs:25,32`), and step 8 keys the single content slot by that
string — pre-existing, but since Part B is already re-working `Bay`, a `BayName`
elevation would fit the "elevate every primitive" discipline the rest of the
spec honors. Minor; not blocking.

## Spec fit

**The Part A category re-type collides with the Storage boundary, and no step
closes the gap.** Step 1 re-types `MaterialEntry.category` (`MaterialLibrary.fs:119`)
and `MaterialQuery.category` to `CategoryId`, with `touches = [Domain,
TestWindows, Tests]`. But that field is serialized by
`OpticalConstructor.Storage/Report.fs`:

- `MaterialEntryDto.category : MaterialCategory` (`Report.fs:39`) and
  `dtoToEntry`'s `category = dto.category` (`Report.fs:96`) bind the DTO field
  to the domain field by type. Re-typing the domain field to `CategoryId` (or
  redefining `MaterialCategory` as a record) breaks this assignment — the
  `build` gate compiles the whole `.slnx`, so **step 1 will not compile**.
- The persisted form relies on `ProjectJson.options`'
  `.WithUnionUnwrapFieldlessTags()` to round-trip a *four-case fieldless DU* to
  plain strings (`Report.fs:30-34`); a record or a Guid does not round-trip that
  way.
- The `materialEntry` `$def` hard-codes
  `"category": { "type": "string", "enum": ["Glass","Metal","Semiconductor","Crystal","Vacuum"] }`
  (`schema/optical-constructor-project.schema.json:84-87`), enforced by
  `validateEntryElement`/`importMaterials`. A `CategoryId` Guid string fails this
  enum.

Binding constraint §0.3 says the spec "writes NO catalogue files to disk and
adds NO JSON-schema, versioning, or import/export work" — but the material
*export* (a pre-existing path, distinct from the new category catalogue)
unavoidably serializes this field, so §0.3 and step 1 are in tension. The spec
needs an explicit decision, added to Part A: either (a) scope `Report.fs` +
the `materialEntry` `$def` into step 1 (persist `CategoryId` as a `uuid`-format
string, resolve the display name through the catalogue on import — and relax
§0.3's blanket disclaimer accordingly), or (b) drop `category` from the
persisted DTO/schema entirely. Leaving it implicit forces the worker to either
break the build or silently do the schema/import-export work §0.3 forbids.
This is the one finding I would want resolved before implementation starts.

Everything else fits: Parts B–E each map cleanly onto their cited seams, the
`.spec-jsonl` faithfully expands the `.spec-md` narrative, and the contract
table (`STORE:CategoryProxy`, `UICOMP:CategoryControls`,
`UICOMP:CategoryEditorWindow`) is consistently declared and later
implemented/consumed.

## Evolvability

Step 15's evaluated case adds a closure to `EpsAxisDispersion`, whose stated
purpose (`Dispersion.fs:303-308`, `DispersionModels.fs:492-498`) is to be the
*serializable* term-data form "so catalogue materials become serializable and
editable." The four transcendental models will now lower to an unserializable
closure. This regresses nothing today (I verified the eps segment tree is never
written to disk — `MaterialEntryDto` persists metadata only, `complexity` is
dropped on import, `Report.fs:98-102`), but it does corner a future "persist the
dispersion tree / §D.9 mapping" slice for exactly those four models. The
constraint is arguably inherent — transcendental models genuinely are not finite
term sums — so this is a note to carry, not a redesign. Worth one sentence in the
step's `how_to` acknowledging that the evaluated case is deliberately outside the
serializable envelope.

## Risks

- **`[<CustomEquality; NoComparison>]` propagation (step 15).** Adding
  `NoComparison` to `EpsAxisDispersion` silently strips auto-generated
  `comparison` from every containing type (`IsotropicEpsSegment`,
  `EpsWithDispValue`, `MaterialComplexity`, `DispersionModel`, `MaterialEntry`).
  I searched for `Set`/`Map`-key/`List.sort`/`compare` use of these types and
  found none, so this looks benign — but the step should state that comparison
  is confirmed-unused, since a later slice that puts any of these in a `Set` or
  sorts them would hit an opaque error far from the change site.
- **Part B retires the Details-LAST pin (step 8).** The pin exists because
  moving the Library bay last previously regressed its sample-row layout
  (`TableAndElementRotationView.fs:235-239`). Step 7's keyed-slot hosting is the
  correct fix for the documented recycling hazard (`Ribbon.fs:84-89`), and
  sequencing the hosting fix strictly before the reorder is right — but the
  ui-smoke gate must actually exercise a rendered frame of the reordered
  full-surface Materials/Library bays (step 8/19 acceptance covers this; keep it).
- **Step-18 MSB3277 is conditional.** "If the build emits MSB3277 … resolve it"
  is defensible (the authors correctly declined to commit an unverified
  WindowsBase claim), but a purely conditional acceptance can pass vacuously.
  Fine as written provided the `build` gate is the real arbiter.

## Bottom line

Architecturally this spec is clean and unusually well-cited; Parts B–E are
ready. The one thing I would hold for a spec revision is the Part A category
re-type: as sequenced it breaks `OpticalConstructor.Storage` and the
`materialEntry` JSON schema, which no step touches and §0.3 disclaims, so step 1
cannot green its `build` gate without unscoped work. That is a suggestion, not a
verdict — the code judge weighs it against the gate results and the reuse
critic. If the judge accepts an in-flight interpretation, the most defensible
one is to fold a `Report.fs` DTO + schema `category`-field update into Part A and
narrow §0.3 to the *new* category catalogue only.

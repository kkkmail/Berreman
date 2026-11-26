# Step 0008 — IMPLEMENT — impl plan

## Approach

Silicon and Langasite are the last two built-ins whose dispersion is a closure
(`EpsAxisEvaluated`). Their indices are the engine's published closed forms in
`OpticalProperties/Dispersive.fs`. Those closed forms move into the core as the closed set
`PublishedIndexLaw`. The engine presets and the library complexities then both read the law, so
each closed form exists once.

1. **Record the references first.** Before any edit, record the start tree's closures as G17 at
   400, 500, 600, 800 and 1000 nm, from the built output. Two sources: the engine preset's own
   private lets in `Dispersive.fs`, read by reflection, and the library complexities' axes.
   They must agree. The script and its output go to `.artifacts/0008-reference-literals*`.
2. **Core (`Berreman/Berreman/Dispersion.fs`).** Add `PublishedIndexLaw` (`SiliconIndexLaw` /
   `LangasiteOrdinaryIndexLaw` / `LangasiteExtraordinaryIndexLaw`) with `value : int` (1, 2, 3),
   `tryOfWire` and `complexIndex`. Move the three closed forms into `complexIndex` verbatim
   (`numberE = exp 1.0` included). Add `PublishedLawAxis of PublishedIndexLaw` to
   `EpsAxisDispersion`, with `complexIndex`, `Equals` and hash tag 7, and update its doc.
3. **Engine presets (`OpticalProperties/Dispersive.fs`).** `Silicon`'s eps reads
   `SiliconIndexLaw`; delete `nSi`, `xiSiFinal` and `refrIndexSi`. `Langasite`'s
   `epsLa3Ga5SiO14` reads the two laws. `g11` / `g33` read the real parts of the laws' indices
   where they re-state those expressions. Delete the two index lets and `numberE`.
4. **Library (`OpticalConstructor.Domain/MaterialLibrary.fs`).** The complexities carry
   `PublishedLawAxis`. Delete the `WaveLength` extension with the three re-statements, and
   rewrite the comment block and the docs so `EpsAxisEvaluated` is no longer named.
5. **Matches.** `parameters` → `[]`, `facetKey` → `"Evaluated (transcendental)"`,
   `isEditable` → view-only (no term data). Update their docs.
6. **Facts (`BerremanTests/EpsWithDispValueTests.fs`).** Add the Silicon fact, the Langasite
   fact and the wire-tag fact.

## Files

- `Berreman/Berreman/Dispersion.fs`
- `Berreman/OpticalProperties/Dispersive.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs`
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`

## Risks

- **Bit-identity.** The presets' numbers must not change: the moved expressions must evaluate in
  the same order with the same operations. `g11` / `g33` must read the same `Complex` they
  re-stated. Two guards: the exact-literal facts, and the unchanged constructor reproduction
  facts.
- **The pipe after a match.** A `|> ComplexRefractionIndex` placed after the last arm binds to
  that arm only, so each arm pipes its own value.
- **ELEV.** The new union's tag is an `int` (ELEV017). Its case names are unique and have two or
  more words (ELEV016). No free function over our types is added (ELEV013).

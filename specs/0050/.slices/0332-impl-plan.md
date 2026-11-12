# 0332 — impl plan

Step 0332 (IMPLEMENT, S101-UNIAXIAL-BUILDER) moves the uniaxial gyrotropic crystal builder out of
`BerremanTests` and onto its product parameter record (`.spec-md` §3.16).

## Approach

1. `OpticalConstructor.Domain/MuellerInverse.fs`
   - `open OpticalProperties.Active` (Domain already reaches it through `Analytics`; `MaterialLibrary.fs`
     opens it the same way).
   - `UniaxialParameters` gains, after `axes`, `member this.opticalProperties : OpticalProperties` whose
     body is the deleted builders' expression:
     `OpticalProperties.type_3_4_6_Crystal (EpsValue.fromRefractionIndex this.ordinaryIndex)
     (EpsValue.fromRefractionIndex this.extraordinaryIndex) this.g11 this.g33`.
   - The module doc's "Domain does not reference OpticalProperties" sentence (lines 16-20) is rewritten to
     the step's prescribed text. No other line changes.
2. `BerremanTests/MuellerInverseTests.fs` — delete `buildUniaxialGyrotropic` with its doc; `forward` reads
   the member; the `orientForCut` fact's `props` is `quartz.opticalProperties`.
3. `BerremanTests/LangasiteInverseTests.fs` — delete `buildLangasite` with its doc; `forward` reads the member.
4. `BerremanTests/BiaxialInverseTests.fs` — delete the fact-local `buildUniaxial`; `uniaxialForward` reads
   the member.
5. Each of the three test files opened `OpticalProperties.Active` only for the deleted builder; the open
   becomes dead and is removed with it.

## Risks

- Bit-identity: the member evaluates exactly the expression the three builders did, so every number is
  unchanged.
- Name resolution: `OpticalProperties` is both a namespace and the engine type; the same `open` +
  `OpticalProperties.type_3_4_6_Crystal` shape already compiles in `MaterialLibrary.fs` and the tests.
- ELEV: a member on its own type is exempt from the primitive rules; no `.value`, no call-site wrap.

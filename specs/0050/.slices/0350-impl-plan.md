# Step 0350 — impl plan

`buildDesign` (`OpticalConstructor.Domain/MuellerReconstruction.fs`) already reads each row's label through
`NamingTemplate.standard.readName` to decide whether the row is of the requested kind, then throws the reading away;
the section 7.2 fact's `prepareRow` reads the same label again (`standardReading`) and fails on an `Error` no kept row
can give. The step hands the reading over instead.

1. `buildDesign`'s first parameter becomes
   `prepareRow : MuellerRawRow -> NameReading -> StokesVector * RealVector4 * DetectorIntensity`. One
   `List.choose` reads each label once; a row whose reading's `matrixKind` is `kind`, whose label carries no
   `(AIR#0)` and which is not an excluded point is projected by `prepareRow row reading`. Rows kept, their order and
   the assembled system are unchanged. The doc says so in one to three lines (spec 0050 S10.3).
2. `MuellerReconstructionTests.fs`: the section 7.2 fact's `prepareRow` takes `(reading : NameReading)` after its row
   and its φ is `reading.objectPhi` (its `standardReading` call goes; the helper stays, the naming-template fact uses
   it). The `families` annotation follows the new projection type. The always-run `buildDesign` fact's stub takes an
   ignored `NameReading`; rows, assertions and numbers unchanged.

Risks: none numerical — the reading handed over is the one the fact made itself, so every φ is the same. ELEV: the
signature carries only our types, a three-tuple of distinct types (ELEV011 clean).

Files: `MuellerReconstruction.fs`, `MuellerReconstructionTests.fs`. No test added or removed.

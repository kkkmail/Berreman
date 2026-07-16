# Impl-plan — spec 0042, slice 002 (IMPLEMENT) — attempt 02

## Goal

Same slice as attempt 01: add the column-major linear-algebra core for Mueller
reconstruction to `OpticalConstructor.Domain.MuellerReconstruction` (seeded by
slice 001), plus BerremanTests facts. Attempt 01 failed **only** the `build`
gate — a single F# compile error in the *test* file. This attempt fixes that
compile error; the Domain production code and the test *logic* are already
correct and unchanged.

## Root cause of the attempt-01 build failure

```
BerremanTests\MuellerReconstructionTests.fs(66,5): error FS0960:
'let' and 'do' bindings must come before member and interface definitions in
type definitions
```

`MuellerReconstructionTests` is an F# **class type** (`type … () =`). In a class
type EVERY `let` binding (a private field) must precede the FIRST `member`.
Attempt 01 placed the shared `let asymmetricM = …` fixture at line 66, *after*
the `[<Fact>] member` methods — illegal. The Domain file
(`MuellerReconstruction.fs`) compiled cleanly; the error was entirely this
ordering.

## Fix

Move the `let asymmetricM = …` fixture (and its comment) up to sit immediately
after the existing `let assertMuellerEqual …` binding — i.e. before the first
`[<Fact>]` member. No other change: the four production primitives
(`kron4`, `vecColumnMajor`, `muellerOfVecColumnMajor`, `MatrixDiff` +
`frobeniusDiff`) and every test body are kept verbatim.

## Files to modify

- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — reorder the
  `asymmetricM` fixture ahead of the members.

No change to `MuellerReconstruction.fs` (it already compiled). No `.fsproj`
changes.

## Risks

- **None new.** The reorder is mechanical. The transpose-guard and round-trip
  algebra were re-derived by hand this round and hold (see impl-log).
- **LF endings.** Keep the file LF-only.

# Reuse critique — 006.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` — focused on `Berreman/OpticalConstructor/OpticalConstructor.Domain/` and `.../OpticalConstructor.Tests/`, plus the named engine seams (`BeamTree.fs`, `Solvers.fs`, `Fields.fs`, `BerremanMatrix.fs`, `Media.fs`).
- Files inspected: ~11/200 (well under cap; cap not tripped).
- Extensions: the configured walk bounds list `.py,.md,.json`, which do not match this F#/.NET repo — the reusable helpers live in `.fs`. I deviated and catalogued the in-scope `.fs` sources (the only files that can carry a reusable optics helper); the `.md`/`.json` files carry no callable helper to cite.

## Findings

### F1: Test-local `eNorm` field-norm helper duplicated verbatim

- **Worker added:** `let private eNorm (f : EmField) : float = f.e.value.norm` at `CurvedElementsTests.fs:60`.
- **Existing helper:** the byte-identical `let private eNorm (f : EmField) : float = f.e.value.norm` at `BeamRoutingTests.fs:42`.
- **Why it matters:** this is direct duplication of a one-line field-magnitude utility across two sibling test modules in the same project. It is the canonical "compare two solver outputs by field norm" idiom used by the engine-seam tests, and the curved-element tests reach for exactly the same idiom. If the comparison basis ever changes (e.g. `f.e.value.norm` → a Stokes/energy measure), both copies must be edited in lockstep, and a missed copy would silently weaken one suite's assertions.
- **Suggested action:** advisory — either extract a single shared test helper (a small `OpticalConstructor.Tests` utility module exposing `eNorm`) and have both modules call it, or leave as-is and note that this project's test suites deliberately keep per-module-private helpers (see F-note below). The judge decides; I lean toward "leave as-is" because the prevailing convention in this test project is per-module-private fixtures (each of `BeamTreeTests`, `BeamRoutingTests`, `StackEditTests`, `RoundTripTests`, `GradientDiscretizeTests` rolls its own `glass`/`vacuumSystem`/`light`), so a lone shared helper would itself diverge from that pattern. This finding is low-severity.

### F2: AC-C7 exercises the raw `EmField.propagate` seam instead of the existing `childIncidentField` wrapper

- **Worker added:** in `CurvedElementsTests.fs:202-216` (AC-C7) the inter-element gap is propagated with `outgoing.propagate gap`, where `outgoing = (solveZone ...).emSys.transmitted`.
- **Existing helper:** `BeamTree.childIncidentField (branch) (gap) (parentEms) = (branchEmField branch parentEms).propagate gap` at `BeamTree.fs:107`. Part A already wrapped "take a branch's outgoing field and advance it across a gap `Layer`" as a first-class Domain helper, and the sibling AC-B4 test demonstrates the canonical way to use it: `BeamRoutingTests.fs:68` calls `childIncidentField BeamBranch.Transmitted gap ems` and proves it equals the manual `(branchEmField …).propagate gap`.
- **Why it matters:** §C.7 / R-7 is about routing the curved-element beam across the inter-element gap through the *project's* reused propagation seam. The AC literally names `EmField.propagate`, which the test satisfies — but Part A's intent (BeamTree.fs:10-16, BeamTree.fs:103-108) is that downstream consumers route through `childIncidentField`, not re-derive the branch-field-then-propagate step inline. The curved test demonstrates the seam works but not that the curved path uses the *same wrapper* every other element uses, so a future regression in `childIncidentField` (the seam Part C's hand-off claims to reuse) would not be caught by this suite. Note this is divergence in the test only — the production `CurvedElements.fs` correctly adds no second propagation routine (good reuse).
- **Suggested action:** advisory — consider routing AC-C7 through `childIncidentField BeamBranch.Transmitted gap (solveZone …).emSys` (mirroring `BeamRoutingTests.fs:68`) so the curved path is pinned to the shared wrapper, or document why the raw `EmField.propagate` seam is exercised directly here. Low-to-medium severity; the AC's literal wording is met.

## Bottom line

The production `CurvedElements.fs` is clean on reuse: it calls the unforked `OpticalSystemSolver` and `IncidentLightInfo.rotateY`, reuses `BeamNode.attach` (with its mirror reflected-only rule) rather than re-deriving the branch logic, reuses `OpticalSystem`/`Layer`/`Thickness` verbatim, adds no second propagator or ABCD module, and mirrors the existing `discretize` count-validation idiom (`invalidArg` + uniform `[ for i in 0 .. n-1 ]`) in `sampleZones` — all consistent with the engine seams the slice names. The two findings are both in the test layer and both low severity: a one-line `eNorm` duplicate (F1) and an AC-C7 propagation step that bypasses the `childIncidentField` wrapper its sibling test uses (F2). Neither is substantive enough on its own to justify a re-spawn; the judge may reasonably resolve both as "leave as-is, note convention" given the project's per-module-private test-helper pattern.

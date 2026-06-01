# Fidelity-judge verdict for round 1

**Verdict:** fail
**Blocker count:** 2
**Info count:** 1

## Summary

Parent spec: `C:\GitHub\Berreman\specs\0022\.spec-md` (Spec 0022, Berreman
Optical Constructor) — a cross-cutting architecture spec with ten parts
(A–J), 7 binding constraints (`## 0`), 85 labeled acceptance criteria
(AC-A1..A8, AC-B1..B11, AC-C1..C8, AC-D1..D8, AC-E1..E10, AC-F1..F9,
AC-G1..G9, AC-H1..H10, AC-I1..I11, AC-J1..J12), a Files-in-scope block,
and a Test-artifacts block. Slices: 10 (`001.slice-md`..`010.slice-md`),
one per part (001→A, 002→D, 003→B, 004→C, 005→E, 006→F, 007→G, 008→H,
009→I, 010→J). Contractful-item count enumerated: 7 binding constraints +
~111 part directives (A.1–J.12) + 85 acceptance criteria + the
files-in-scope / test-artifact / reference path set. Coverage is otherwise
faithful — every binding constraint is reproduced verbatim in every slice,
every part directive (A.1–J.12) is reproduced verbatim under `Requirements`,
and slices 002–010 reproduce their parent "Part X acceptance" criteria
byte-for-byte — but slice 001 (which owns Part A) replaced the parent's
Part-A acceptance criteria (AC-A1..AC-A8) with a different, directive-derived
list under the same identifiers plus an invented AC-A9, which is a
contractful-identifier meaning-drift and an invention.

## Findings

### F-001  kind=meaning-drift  severity=blocker

**Parent-spec excerpt:**

> - AC-A1: After the five new projects are added and `Berreman.slnx` is built with the documented MSBuild invocation at Configuration=Release Platform=x64, the solution MUST compile with all five `OpticalConstructor.*` projects ... with no new C# project introduced.
> - AC-A2: After loading any canonical project file, the storage layer MUST validate it against `optical-constructor-project.schema.json` (anchors opticalSystem/layer/opticalProperties/beamNode/beamBranch/constructorElement) and MUST reject an invalid file before the domain model is admitted, within the load call.
> - AC-A5: After any physical quantity is stored in the domain model or serialized to canonical JSON, the stored value MUST be in the SI base ...
> - AC-A6: After a derived/bulk artefact ... is persisted, it MUST be written via Softellect.Sys.Core.serialize with BinaryZippedFormat to a `.binz` sidecar ...
> - AC-A7: After an eV or cm⁻¹ value is entered at the UI/IO boundary, the boundary conversion MUST round-trip to wavelength-in-meters ...

(Parent `## Acceptance criteria` → `### Part A acceptance`, AC-A1..AC-A8 — eight labeled criteria.)

**Slices considered:** `001.slice-md`

**Slice excerpt:** `C:\GitHub\Berreman\specs\0022\.slices\001.slice-md`:

> - [ ] **AC-A1 (assembly layout).** The solution `Berreman.slnx` contains the five new projects ... each with `<Platform Project="x64" />`, in the dependency order engine → orchestration → UI → tooling; `OpticalModel.fsproj` is unchanged. *(Maps: A.1)*
> - [ ] **AC-A2 (engine reuse, no fork).** No new project copies, re-derives, or shadows `OpticalSystemSolver` ... *(Maps: A.2, A.3)*
> - [ ] **AC-A5 (UI architecture).** `OpticalConstructor.Ui` is pure F# Avalonia + FuncUI MVU ... *(Maps: A.6)*
> - [ ] **AC-A6 (storage split).** The canonical project serializes to JSON validated against `optical-constructor-project.schema.json`; `.binz` is used only for derived/bulk sidecars ... *(Maps: A.7)*
> - [ ] **AC-A7 (numerics boundary).** Linear algebra stays on `MathNetNumericsMath`; optimization is ALGLIB-only ... *(Maps: A.8)*

**Reason:** Slice 001 owns Part A, but instead of reproducing the parent's
eight Part-A acceptance criteria it authored a *different* set of
criteria — restatements of the Part A directives A.1–A.10 — and labelled
them with the parent's contractful `AC-A1..AC-A8` identifiers. The same
identifier therefore carries a different substantive claim than the parent:
parent `AC-A2` is the schema-validation-on-load criterion, whereas slice
`AC-A2` is "engine reuse, no fork"; parent `AC-A5` is the SI-storage
criterion, whereas slice `AC-A5` is "UI architecture"; parent `AC-A7` is the
eV/cm⁻¹ round-trip, whereas slice `AC-A7` is "numerics boundary". This is a
live collision, not a harmless re-statement: sibling slices reference the
parent's meanings under the same labels (e.g. `002.slice-md` cites
`AC-A7` for the eV/cm⁻¹ round-trip; `003.slice-md` cites `AC-A2`/`AC-A5`
for schema-validation and SI-unchanged round-trips; `009.slice-md` cites
`AC-A6` for the `.binz` sidecar identity), so the same `AC-A2`/`AC-A5`/
`AC-A6`/`AC-A7` token now denotes two different acceptance criteria across
the slice set. Reassigning a contractful acceptance-criterion identifier to
a different claim is a substantive meaning-drift (rubric Axis B). Every
other part (B–J) reproduced its parent acceptance criteria verbatim, so this
is an isolated mishandling of Part A's acceptance block rather than a
uniform style choice.

### F-002  kind=invented  severity=blocker

**Parent-spec excerpt:**

> ### Part A acceptance
> - AC-A1: ...
> - AC-A8: Before `OpticalConstructor.Ui` is linked against `C:\GitHub\Avalonia.FuncUI.Clone\`, the audit gate MUST be recorded as passed; until then the UI project MUST contain no reference to the clone and the linking-mechanism choice MUST remain unresolved.

(The parent's Part A acceptance list ends at AC-A8; there is no AC-A9 anywhere in the parent spec.)

**Slices considered:** `001.slice-md`

**Slice excerpt:** `C:\GitHub\Berreman\specs\0022\.slices\001.slice-md`:

> - [ ] **AC-A9 (audit gate).** No build links the FuncUI clone before the §A.9 audit passes; the linking mechanism remains undecided. *(Maps: A.6, A.9)*

**Reason:** Slice 001 introduces an acceptance criterion `AC-A9`, but the
parent's Part A acceptance block contains only AC-A1 through AC-A8 — there is
no AC-A9 to trace to. The slice's AC-A9 (audit-gate) restates the substance
the parent expresses in its AC-A8, but it does so under a net-new
`AC-A9` identifier that has no parent counterpart. Carrying an
acceptance-criterion label absent from the parent is an invention
(rubric Axis B), and it compounds the off-by-one renumbering in F-001 (the
slice has nine AC-A* entries where the parent has eight).

### F-003  kind=invented  severity=info

**Parent-spec excerpt:**

> - `C:/GitHub/Berreman/Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (new) — orchestration domain project; references `Berreman/Berreman.fsproj` and `Analytics/Analytics.fsproj`; registers BeamTree/Units/CurvedElements/SourceSpec/SourceCombination/DispersionModels/MaterialLibrary in compile order

(The parent's Files-in-scope Domain entry enumerates the registered modules and does not list an `OpticalConstructorProject.fs` file.)

**Slices considered:** `001.slice-md`

**Slice excerpt:** `C:\GitHub\Berreman\specs\0022\.slices\001.slice-md`:

> - `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructorProject.fs` (new) — the root aggregate `OpticalConstructorProject` (skeleton; per-element default-unit plumbing filled by slice 002 / D.4 and slice 003 / B.8). *(A.7)*

**Reason:** Slice 001 adds a new file `OpticalConstructor.Domain/
OpticalConstructorProject.fs` that is not named in the parent's
Files-in-scope list. This is recorded as `info` only, not a blocker,
because it is traceable to a named parent directive: §A.7 requires the
Domain project to "Define the root aggregate `OpticalConstructorProject`",
and the parent's Domain fsproj bullet enumerates registered modules without
claiming to be an exhaustive filename list. The splitter's choice to host
the §A.7-mandated aggregate in a dedicated file is a reasonable realization
rather than a manufactured artefact; it is flagged for the operator's audit
trail only.

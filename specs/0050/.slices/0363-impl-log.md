# Step 0363 — impl log

## Progress

- Read the protocol, project prompt, AGENTS.md, slice 0363 and slice 0362, `Studies.fs`, `StudyTests.fs`,
  `StudySessionTests.fs`, 001 §L's scope table and `Item.tryFindParameter` (`Kit.fs`).
- Grep: `StudyError` is matched exhaustively nowhere outside `describe`; `ScopeNotAllowedErr` and
  `withParameterScope` are new names here and in STL.
- `Studies.fs`: added `ScopeNotAllowedErr of reason : string` to `StudyError`, rendered
  `the parameter cannot take that scope: <reason>`.
- `Studies.fs`: added `Study.withParameterScope` to the `type Study with` block that holds `scopeOf`, just after it.
  - The checks run in the step's order: the parameter, then the scope's kind, then the link.
  - The parameter check: an item parameter needs an item of `used` with its id that holds the name
    (`Item.tryFindParameter`). A material component needs an opened copy of its version that holds the name.
    Otherwise it answers `UnknownStudyParameterErr`, whose reason starts with the reference's `describe`.
  - The kind check: an item parameter takes `ItemScope`, `SessionBlockScope` or `ExperimentScope`, and a material
    component `MaterialScope` only. Otherwise it answers `ScopeNotAllowedErr`
    `"<describe> takes <allowed texts>, not <scope text>"`.
  - The link check: a parameter that is any link's follower or leader answers `ScopeNotAllowedErr`
    `"<describe> is linked, <follower describe> = <leader describe>, and a link joins parameters of one scope"`.
  - Otherwise `scopes` takes the entry by `List.tryFindIndex` + `List.updateAt` in place, or appends it. Every other
    field is left as it was.
- `StudyTests`: added a `scopedAs` helper and the two facts the step names.
- `StudySessionTests`' error-render fact: added the `ScopeNotAllowedErr` row (index 12).
- Built, ran constructor-unit-tests, elevation and the 20 invariants (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`: `StudyError.ScopeNotAllowedErr` and
  `Study.withParameterScope`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`: the two new facts, a `scopedAs` helper, and
  the module doc now names SetParameterScope.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs`:
  `every study error renders its condition and its payload` covers the new case.

## Testing state

These are development runs of each gate's own command from `0363.gates`, over the final tree.

- **build:** exit 0, `0 Error(s)`, no lowercase `error`, in 75 s (deadline 600 s).
  - The only warnings are the exempt NU1902, plus the vendored MathNet SYSLIB0051 pair that has been there since
    step 0001.
- **constructor-unit-tests:** `Passed!`, 1599 of 1599 (1597 + 2), in 72 s (deadline 180 s).
  - In the TRX, both new `StudyTests` ids and the widened `StudySessionTests` fact are `Passed`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 220s`, with 1518 ELEV004 findings suppressed, as at step 0362 (deadline 600 s).
- **invariant-s36-\* (20):** I decoded each `--find-nothing` grep and ran it over the final tree. All 20 find
  nothing.

## Artifacts

- `specs/0050/.artifacts/0363-dev-build.log`
- `specs/0050/.artifacts/0363-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0363-dev-elevation.log`

## Gotchas

- **The link check matches the whole `StudyParameterRef`.** It compares the case, the id and the name, as
  `unknownsWith`'s follower filter does (step 362). A parameter that is any link's leader is refused as well as its
  follower, so a link's two ends can never be given different scopes from here. The fact asserts both ends.
- **The order of the refusals is the step's.** The parameter check comes first, then the kind, then the link. A
  linked material component under `ItemScope` therefore answers the kind refusal. Links between material components
  arrive with LinkParameters (step 364).
- **A known parameter can take a scope.** The step refuses only parameters the study lacks, not a parameter's state.
  A scope set on a known parameter applies once it is freed. `unknownsWith` already reads `scopeOf` for every
  unknown.
- **The "study unchanged" half of the refusal fact holds by construction.** A refusal is an `Error` and the record is
  immutable. The fact ends by showing that a linked study still scopes an unlinked parameter (Phone's `gain`), with
  every other field equal.

# Reuse critique -- .spec-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (repo root).
- Files inspected: 4/200 (the diffed file plus the sibling contract-id registry, lock, and spec inputs; `.py`/`.md`/`.json` extensions catch none of the F# solver source, and the diff touches none of it).
- Extensions: `.py`, `.md`, `.json`. Cap not tripped.
- Diff scope: `git diff HEAD --numstat` reports a single changed file, `.contract-ids/XDUO-json` (+24 / -0). No `.fs`/`.fsproj`/test files changed; `specs/0035/` and `.claude/` are untracked metadata, not product code.

## Findings

No reuse findings; the diff introduces no duplication of existing helpers within the walked roots.

Rationale (not a finding — recorded so the judge sees the read): this cycle's diff is not an implementation slice. It is a contract-id registration stub — the task file itself states "(Stub template: full content lands in slice 004-08.)" The entire change adds three entries to the `.contract-ids/XDUO-json` registry:

- `STORE_XDUO_0003` → `CategoryProxy` (proxy, `OpticalConstructor.Domain`)
- `UICOMP_XDUO_0005` → `CategoryControls` (component, `OpticalConstructor.Controls`)
- `UICOMP_XDUO_0006` → `CategoryEditorWindow` (component, `OpticalConstructor.TestWindows`)

These are exactly the three placeholders the spec's contract table mints (`.spec-md` lines 78–80). The reuse critic evaluates whether a diff reinvents functions, helper modules, fixtures, or test scaffolding that already exist. This diff adds none of those — no F# symbol, no fixture, no test helper. There is no existing helper to point at, and the rubric is explicit that a finding without a citable existing helper is noise, not a finding.

Where the diff *could* have diverged from an existing pattern, it does the opposite — it reuses one. Each new registry entry mirrors the schema and field ordering of the six pre-existing entries in the same file (`STORE_XDUO_0001` `MaterialProxy`, `STORE_XDUO_0002` `SampleProxy`, `UICOMP_XDUO_0001`–`0004`): the same `name` / `kind` / `language` / `declaringProject` / `status` / `formerNames` shape, the same `STORE_`/`UICOMP_` id-prefix convention, the same `declaringProject` values already used by the sibling proxies and components. The two new `STORE_` entries declare into `OpticalConstructor.Domain` alongside `MaterialProxy`/`SampleProxy`, and the two new `UICOMP_` entries into `OpticalConstructor.Controls` / `OpticalConstructor.TestWindows` alongside the existing controls/windows — consistent with the spec's own instruction that `CategoryProxy` follow the `SampleProxy.createInMemory` / `MaterialProxy.createInMemory` shape and `CategoryControls` follow the `LibraryControls` shape (`.spec-md` §0.3, §A.2). Pattern-consistent registration is the correct outcome, not a divergence to flag.

There is consequently no direct duplication, no near-miss duplication, and no pattern divergence to report for this cycle. The substantive reuse questions the spec raises — will `CategoryProxy.createInMemory` actually reuse the `MaterialProxy`/`SampleProxy` in-memory closure pattern rather than hand-rolling a parallel one; will `CategoryControls` reuse `LibraryControls` rather than clone it; will the dispersive-ρ/μ editor reuse `DispersionModels.modelParameters` + the raw `SumOfTerms` surface (§C) rather than re-implement a coefficient editor; will the n/k preview reuse the shared `ExperimentChart`/`ChartWindow` control rather than keep the hand-rolled `NkDispersionChart.inlineCanvas` (§E) — are all deferred to the implementation slices (004-08 onward) where real F# code lands. They cannot be assessed against a contract-id-only diff, and inventing findings against code that does not yet exist would violate the "reuse what exists, do not design what doesn't" constraint.

## Bottom line

The diff is a three-line contract-id registration stub carrying no F# helpers, fixtures, or test scaffolding, so there is nothing that could duplicate an existing helper, and the new registry entries in fact reuse the established `XDUO-json` entry schema and sibling `declaringProject` conventions verbatim. My read is that this cycle is clean and gives the judge no reuse basis to re-spawn. The real reuse surface (proxy `createInMemory` reuse, `LibraryControls` reuse, `DispersionModels`/`ExperimentChart` reuse) arrives with the implementation slices and should be critiqued then.

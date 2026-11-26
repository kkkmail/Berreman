# Step 0275 — impl plan

## Approach

The experiments half of the grid (spec 0050 S8.3; 001 §J "all setups and experiments created at once"):
`Experiments.commitOverSetups (references : SetupReference list) (c : ExperimentCollection) : ExperimentCollection`,
placed directly after `commit` in `OpticalConstructor.Domain/Experiments.fs`.

- When `c.canCommit` is false (no element or no variable chosen) the collection is answered unchanged, as `commit`
  appends nothing for an incomplete draft.
- Otherwise the draft is cleared of its editing cursor (`editingId = None`), and `commit` is folded over the
  references in order, each fold step starting from that fresh draft. `commit` appends one experiment per reference
  (next id, the draft's varied element, capture, range and wavelength, no data file) and moves `nextId` forward. The
  answer's draft is the fresh draft, editing nothing, so a later commit appends.
- No second experiment builder is written; the only new code is the fold (AGENTS.md "Reuse, never copy").

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs` — `commitOverSetups`, with a one-to-three-line
  doc citing spec 0050 S8.3.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs` — the fact
  `committing the draft over setups appends one experiment over each, and an incomplete draft appends none`, in the
  "committing over the active setup revision" section.

## Risks

- ELEV rules: two parameters, both our types, return our type — no ELEV003/010/013 exposure. `commitOverSetups` is a
  new, unique public name (ELEV015; grep finds no namesake).
- Equality of `Experiment` / `ExperimentCollection` in the test: both are records of equality-capable fields; the
  existing renumbering test already compares `Experiment list`.

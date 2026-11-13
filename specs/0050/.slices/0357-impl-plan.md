# Step 0357 — impl plan

## Approach

Spec 0050 S11.1 sessions as data. In `Studies.fs`, after `SessionGainModel.ruleWith` (step 356), add the record
`StudySession = { name : SessionName; experiments : ExperimentName list }`. It has two members:

- `static member splitAll (experiments : Experiment list) : StudySession list` — one fold over the experiments in
  the order given. The state is the sessions in order of first appearance, plus the name of the open automatic block.
  - An experiment with `sessionOpt = Some s` joins (or opens) the session `s` and leaves the open block alone.
  - A `DiagnosticRun` with no hand-set session closes the open block.
  - Any other experiment joins the open block. With none open, it opens a block named `Session <k>`, `k` the least
    positive integer whose `SessionName` neither a hand-set session of the list nor an earlier block holds.

  Naming a block when it opens is exact, because every hand-set name is collected before the fold. Since a block
  only opens on an experiment that joins it, there is never an empty session. Order by first experiment follows from
  appending each new session as it first appears.
- `member this.darkRunsIn (experiments : Experiment list) : ExperimentName list` — the session's own `DarkRun`
  experiments in its order; with none, every `DarkRun` of `experiments` in list order.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs` — the new record and its two members.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs` — two new facts over test-local
  experiments. They are built like `ExperimentProxyTests`' `e1` (a full record literal over a `Bench` revision-1
  setup, one rotating-polarizer axis) and differ only in name, role and session.

## Risks

- Record-label inference: `name` / `experiments` are shared with `Experiment`, `SetupReference` and
  `ExperimentCollection`. Every construction of a `StudySession` is annotated.
- ELEV: a new type name (`StudySession`, unique — grep finds nothing). Members only, no free function. The one
  int is the local numbering loop, which mirrors `SetupName.firstFree`.
- The constructor-unit-tests count must rise by exactly 2 (1572 → 1574).

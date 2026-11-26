# Step 0491 — impl plan

Spec 0050 S14.2: the Plan view's ranked candidates, each with Add to kit (001 §O; .spec-md §10.4).

## Approach

1. `OpticalConstructor.Controls/UiIds.fs`: add `planCandidateList` (`"PlanCandidateList"`) at the end of the Plan step's
   group.
2. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: one private extension member on `MeasurementPlanner.PlanCandidate`
   writing `<plate item GUID>_<t|r>_<wavelength nm, F1, invariant>`, and the two static members `planCandidateRow`
   (`PlanCandidate_<key>`) and `addPlanCandidateButton` (`AddPlanCandidate_<key>`) beside `planMaterialChoice`.
3. `OpticalConstructor.Ui/PlanView.fs`: after the instrument lines, the heading `Next experiments, in the order the plan
   adds them:` and a vertical `StackPanel` carrying `planCandidateList`:
   - one line per report `inKit` candidate, `Already in the kit: <caption>.`;
   - per ranked step, a horizontal row carrying `planCandidateRow`: `<k>. <caption> — rank <r>`, the report criterion's
     measure (`, log det <F3>` or `, Σσ² <G4>` when the trace exists), `; σ: <name G3>, …` when bounds exist; at its end
     `Add to kit` (writes offered and `CandidateNotInKit`) or `In the kit as <name>.` (`CandidateInKit name`);
   - with no ranked step, `No candidate is open: …`.
   Coverage is computed in the view over `model.kitSetups` and the collection's experiments, so a candidate added after
   the plan shows as in the kit without a new run. The row text does not wrap (horizontal StackPanel rule, AGENTS.md).
   The renderings are private extension members (ELEV013P).
4. Tests:
   - `PlanViewTests`: new `the Plan step ranks the candidates and offers Add to kit for each the kit does not hold`
     (no trait) and `pressing Add to kit records a planned experiment of the candidate in the kit` (ui-smoke); the ids
     fact pins `PlanCandidateList` and the two derived ids of a test-local candidate.
   - `ScrollExtentTests`' Plan fact asserts the completed report ranks candidates, so their rows are in the window
     checked.
   - `KitWindowTests`' read-only fact runs the fixture's plan and finds the candidate rows but no Add to kit.

## Risks

- A real plan run per fact costs seconds; the read-only fact gains one plan run.
- ELEV013P on one-parameter helpers: written as private extension members from the start.
- `actionButton` keeps its first handler per key: the button key is the candidate's id, unique per candidate.

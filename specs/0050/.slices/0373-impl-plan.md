# Step 0373 — impl plan

## Approach

Add, in `StudyForward.fs` after `Study.parameterLayoutWith` (step 372), the numbers one experiment is evaluated with
at one parameter vector (spec 0050 S11.2; 001 §I, §L):

1. `ExperimentAssignment = { itemValues : Map<ItemId, ParameterValues>; crystals : Map<MaterialVersionId, CrystalParameters> }`
   with `valuesOf` (the item's entry, else `ParameterValues.ofParameters item.parameters`), `crystalOf` (the opened
   copy's entry, else its `currentCrystal`) and `materialProxyOver` (the store with `resolveVersion` answering an
   assigned version's entry with its physics replaced by `CrystalParameterPhysics` of the assigned crystal; every other
   version and every other field passes through).
2. `type StudyParameterLayout with member this.assignmentFor study items experiment vector`: `VectorLengthErr` naming
   the vector's length and the entry count when they differ; otherwise the entries indexed from 0, kept when their
   owner holds the experiment (`ItemOwner` / `MaterialOwner` always, a `SessionOwner` whose session of `this.sessions`
   lists it, the `ExperimentOwner` of it), then (a) each item's own values with its held entries set by `withValue`,
   (b) each opened material's `currentCrystal` with its held entries written through the generated axis of that name,
   (c) each link's follower set to its leader's value in this assignment (item `valueOf`, or a component's
   `parameterValueOf`), written as in (a) / (b).
3. `StudyForwardError` gains `VectorLengthErr of reason : string`, rendered
   `the parameter vector does not fit the study: <reason>`.

## Tests (`StudyForwardTests`, over step 372's fixture)

Four new facts named exactly as the effects list them, plus `VectorLengthErr` in
`every forward error renders its condition and its payload`. CPL2 (a second circular polarizer item, `c2.retardance`
known at 1.2) is added to a bench slot so the study can link it to CPL1's retardance. The working-copy fact uses a
fresh Built-in material store with the test crystal saved into it (v1), comparing another version (glass 1.52 v1)
with the store's own answer.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs`

## Risks

- ELEV rules: every new public signature carries elevated types only; the vector place is `StudyVectorIndex`.
- A link whose leader holds no value in the assignment (its item not among `items`) cannot be read: the follower then
  keeps its own value (the brief names no error for it; recorded in the impl-log).

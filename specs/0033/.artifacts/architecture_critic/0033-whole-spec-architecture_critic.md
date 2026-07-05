# Architecture critique -- .spec-md cycle 1

## Summary

This is a whole-spec review: no code has landed (the `specs/0033/` folder is
untracked and `git diff HEAD` is empty), so the artifact under critique is the
spec itself — `.spec-md` plus the 26-step `.spec-jsonl`. The spec is
architecturally strong: I spot-checked roughly twenty of its file:line
citations across nine files (`Propagation.fs`, `Dispersion.fs`,
`MaterialProperties.fs`, `Active.fs`, `ElementId.fs`, `MaterialLibrary.fs`,
`DispersionModels.fs`, `MaterialImport.fs`, `TableAndElementRotationView.fs`,
`Validation.fs`, `MaterialPreview.fs`, `Program.fs`, `RepeatBuilder.fs`) and
every one is accurate, and the layering claims check out against the actual
`.fsproj` references. The most important finding is a fidelity gap: Part F
promises the `Ui/Validation.fs` helpers "move with the editors … never
duplicated," but no step schedules that move, and step 23's `touches` omits
`OpticalConstructor.Ui`, leaving the implementing worker to either duplicate
rules (forbidden) or improvise an unauthorized move.

## Layering

Verified correct. The core `Berreman.fsproj` references only the vendored
MathNet projects, so placing `RhoWithDispValue.toRhoWithDisp` as a type
extension in `OpticalProperties/Active.fs` (step 9) is not a stylistic choice
but forced — the spec's stated reason is real. `EpsAxisDispersion` declared in
core (step 8) and consumed by Domain (step 11) and Storage (step 25) is a
clean downward dependency. Step 19's claim that TestWindows "today references
only Domain and Controls" matches `OpticalConstructor.TestWindows.fsproj`;
note the Analytics reference it adds is already transitively present (Domain
references `Analytics.fsproj`), so the explicit reference is correct hygiene
for a direct dependency rather than a new edge.

One soft spot: step 13's `MaterialComplexity.toProperties` needs the Active.fs
type extension, meaning Domain consumes `OpticalProperties` — but Domain has
no direct reference to it; visibility flows transitively through Analytics (a
charting project). This is existing practice (`Propagation.fs` already uses
`OpticalProperties.transparentGlass` etc.), but step 13 deepens the reliance.
The smallest hardening is a direct `OpticalProperties` ProjectReference on
Domain when step 13 lands, so a future cleanup of the odd Domain→Analytics
edge cannot break material assembly at a distance.

## Separation of concerns

The proxy split is well shaped: `MaterialProxy.createInMemory` taking an
injected `samplesReferencing : MaterialId -> Sample list` (step 6) keeps the
referential-integrity policy inside the material seam without a bidirectional
proxy dependency, and composition (step 26) wires it to the SampleProxy store
one-directionally. The decision to build `SampleStackEditor` as a new pure
Domain edit model rather than growing `StackEditor.StackMsg` (step 21) is
correctly reasoned — the existing editor transforms a resolved engine
`OpticalSystem` that has no material ids to select by — and it honors the
CLAUDE.md rule that editor state be testable without a window.

## Consistency

The spec goes with the grain of the codebase. `ExperimentChart.fs`'s own
module doc says the model is "Domain-neutral (no Berreman types) so it crosses
the Controls/Ui seam freely" — Part D's real-move into Controls completes an
intention the code already recorded, and I confirmed `ChartWindow.fs` opens
only Avalonia/ScottPlot, so the move needs no new Domain edge on Controls.
Step 17 does give Controls (currently a zero-reference leaf) its first chart
package, but both hosts already pin ScottPlot.Avalonia 5.1.59, so no version
skew is possible. The new controls copy the `LibraryControls` shape
(`Row`/`State`/`Handlers`/`UiIds`) verbatim, and relabeling
`MaterialError.UnknownMaterialId` to carry `reason : string` (step 2) brings
the one existing bare-payload error case up to the CLAUDE.md error-DU rule.
The Part A reversal of the spec-010 "only expanded films exist" decision is
honestly recorded and matches what `RepeatBuilder.fs`'s module doc actually
says.

## Spec fit

Three internal-coherence findings:

1. **Constraint 0.1 contradicts Part B as written.** 0.1 ends "No solver file
   changes in this spec," yet steps 7–10 add types to
   `Berreman/Berreman/Dispersion.fs` and new `Rho` builders to
   `OpticalProperties/Active.fs`. The intent (no changes to existing solver
   types or algorithms; additions only) is clear from context, but a
   literal-minded worker or judge can read steps 7–10 as violating a binding
   constraint. Reword 0.1 to say the engine changes are strictly additive
   `…Value` data plus the named Active.fs builders.

2. **Constraint 0.3 contradicts step 2.** 0.3 says "The only
   `OpticalConstructor.Storage` change is Part G's import-parser breadth," but
   step 2's `touches` includes Storage (the `MaterialImport` entry builders at
   `MaterialImport.fs:80,109,144` are re-typed to `MaterialId`). Mechanical
   fallout of the id elevation, but "only" is falsified; qualify it.

3. **Part F's validation-helper move is never scheduled.** Part F says
   `validateThickness` / `validateRepeatCount` / `validateWavelengthRange` /
   `imaginaryIndexGainWarning` (all in `OpticalConstructor.Ui/Validation.fs`)
   "move with the editors where project layering requires, never duplicated."
   TestWindows cannot reference Ui (verified), step 23 requires the
   `imaginaryIndexGainWarning` rule and step 21 re-states the
   `validateRepeatCount` rule, yet no step's `how_to` moves the helpers and
   step 23's `touches` omits `OpticalConstructor.Ui` (which a move must
   re-point). As written the worker must either duplicate the rules (Part F
   forbids it) or perform an unauthorized cross-project move. Add the move
   (Ui → Domain, re-point Ui) to step 21 or 23 explicitly, with Ui in
   `touches`.

Otherwise fit is good: Part G's typed unsupported-formula error for RII
formulas 8/9 is an honest negative scope, and the contract table matches the
six ADD_CONTRACT/ADD_COMPONENT steps and `.contracts-json` exactly.

## Evolvability

The in-memory-only stores (0.3) are the right seam: `Program.fs:125-127`
already documents that disk-backed proxies would replace the mocks at the
composition root, so persistence in a later spec is a one-site change. Step 1
keeping `materialId` a string until step 2 elevates it means the seeds are
rewritten twice in consecutive steps — accepted churn that keeps each step
landable, worth the cost. Step 24 re-minting `BayNames.library` with a new
meaning (samples workbench) after step 14 frees the label is deliberate per
0.5; between the two steps the identifier does not exist, so the compiler
guides any stragglers.

## Risks

Step 12's Brendel–Bormann model requires the Voigt profile (complex error
function); the step names reference values to reproduce but gives no guidance
on the numerical kernel. The vendored MathNet.Numerics ships special
functions — the worker should reach for those rather than hand-rolling
Faddeeva, and the spec could say so in one clause. Step 1 is by far the
largest step (new structure type, delete the string branching and glass
fallback, re-seed all samples, rebuild `sampleToSystem`, re-point the sweep
builders); its equal-to-hand-built-system acceptance is a strong pin, but it
is the step most likely to need a re-spawn. The 0.4 "first covering segment
wins / topmost extrapolates, no validation" semantics are surprise-prone for
users, but they are an explicit operator decision and the step 8 acceptance
tests pin them — no action needed beyond keeping those tests.

## Bottom line

I would ship this spec into implementation. The citations are trustworthy,
the layering decisions are verified against the real project graph rather
than asserted, and the recorded decisions genuinely reflect what the code
says. Before or during Part E/F, the resolver should patch the three spec-fit
items — reword constraints 0.1 and 0.3 so the binding text stops contradicting
the steps it binds, and schedule the Validation.fs helper move with Ui in the
touching step — since the third one otherwise forces a worker into an
unrecorded architectural choice. None of the three blocks Parts A–D from
starting.

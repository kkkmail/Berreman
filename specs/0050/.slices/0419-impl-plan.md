# Implementation plan

Wire `ConstructorScene.mount` in `OpticalConstructor.App/Program.fs` to the existing
`StudyFitJobHost.onUiThread`, sending progress and completion through the Elmish
dispatch captured by its init command. Keep the other shell seams unchanged and
state the off-UI-thread fit contract briefly on both constructor window types.

Add the one authorized `KitWindowTests` smoke identity. Show the real
`InverseConstructorWindow` over `TestAppScope.freshAppContext` and
`TestAppScope.openedKitIn`; use the existing ID/centre-click helpers to create
`Empty`, open Fit and run it. Pump dispatcher jobs until the fit error has text,
bounded by ten seconds with no sleep. Assert the required no-stage-run explanation,
`Empty.study-json` persistence and absence of fit-run files, then close the window.
Retain all existing consumer assertions and test identities.

Insert only the required six-line study-flow gotcha after the lab import bullet
in `AGENTS.md`. Write the implementation log and state-of-the-world only to the
task's assigned paths. Risks are asynchronous completion and delayed UI rendering;
reuse the real host and a condition-based dispatcher wait. The WIRE_UI worker's
Invariant 6 assigns every gate execution and verdict to the supervisor, so this
worker will run no build, test, analyzer or invariant command and report verification
as pending. Preserve the supervisor's pre-existing manifest state change.

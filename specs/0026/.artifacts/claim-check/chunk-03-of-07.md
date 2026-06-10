{
  "claims": [
    {
      "id": 1,
      "phrase": "When both the reflected and transmitted groups are emitted ... The reflected/transmitted groups the constructor draws MUST be the same two branches the engine beam tree routes and solves (BeamTree.fs:98, `:117`).",
      "spec_location": "§B.7.1",
      "evidence": "Inferred assumption-shape phrase ('the engine beam tree routes and solves' = already-wired engine behaviour); not a literal trigger word. Pointer-anchored. Read Berreman/OpticalConstructor/OpticalConstructor.Domain/BeamTree.fs:98 -> `let branchEmField (branch: BeamBranch) ... = match branch with Reflected -> ems.reflected | Transmitted -> ems.transmitted` (selects the reflected/transmitted branch); Read :117 -> `let routeAndSolve (branch) (gap) (parentEms) (childSystem) = BaseOpticalSystemSolver(...).emSys` (routes parent branch to child and solves). Both lines exist and match the described routing/solving of the two branches.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "The existing `Schematic.fs` module (Schematic.fs:18) draws a *vertical cross‑section of a single `OpticalSystem`*; it is a different view and MUST NOT be repurposed or removed.",
      "spec_location": "§C.0",
      "evidence": "Pointer-anchored ('existing'). Read Berreman/OpticalConstructor/OpticalConstructor.Ui/Schematic.fs:18 -> `module OpticalConstructor.Ui.Schematic`; module docstring (lines 10-17) describes the pure cross-section projection geometry. File and module exist at the cited line.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "Colours MUST be ... expressed as pure colour values the FuncUI binding maps to Avalonia, in the same style as `SchematicColor` (Schematic.fs:35).",
      "spec_location": "§C.5.2",
      "evidence": "Inferred assumption-shape phrase ('in the same style as' an existing symbol); pointer-anchored. Read Berreman/OpticalConstructor/OpticalConstructor.Ui/Schematic.fs:35 -> `type SchematicColor =` (record red/green/blue : byte). Cited symbol exists at the cited line.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "Reuse `SchematicColor` (Schematic.fs:35) as the colour value shape. Do not fold the top‑down table geometry into `Schematic.fs` (a different, cross‑section view); add the new `Table.fs` / `Drawer.fs` / top‑down layout modules instead.",
      "spec_location": "§C.8",
      "evidence": "Inferred assumption-shape phrase ('Reuse <existing>' / 'a different, cross-section view'); pointer-anchored. Read Schematic.fs:35 -> `type SchematicColor =` exists; Schematic.fs:18 module is the cross-section view as described in §C.0. Cited symbol and file confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "The existing shell has a flat two‑page nav bar (`Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs:73` `Page`; `:510` `navBar`; `:500` `navButton`); the ribbon shell is net‑new and is added as the new front door.",
      "spec_location": "§D.0",
      "evidence": "Pointer-anchored ('existing'). Read Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs:73 -> `type Page = | Construction | SynthesisFit` (two pages); :500 -> `let private navButton (dispatch) (label) (target: Page) (active: Page) : IView`; :510 -> `let private navBar (model) (dispatch) : IView` building a horizontal StackPanel of two navButtons. All three cited symbols exist at the cited lines and the 'flat two-page nav bar' description matches.",
      "verdict": "CONFIRMED"
    }
  ]
}

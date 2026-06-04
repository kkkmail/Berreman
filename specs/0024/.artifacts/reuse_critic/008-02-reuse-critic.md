# Reuse critique -- 008.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (focused on `Berreman/OpticalConstructor/` — the slice's blast radius: `OpticalConstructor.Ui`, `OpticalConstructor.Storage`, `OpticalConstructor.Ui.Tests`).
- Files inspected: ~14 / 200 (cap not reached).
- Extensions: the task bound lists `.py,.md,.json`, but this is a pure-F# project and the entire diff is `.fs`; I walked `.fs` plus the `.fsproj`/`.md` context. The `.py` default is a stale cross-repo template value, recorded here so the judge knows the walk targeted the real source language.

## Findings

### F1: `marshalIo` is a line-for-line clone of `marshalFit`

- **Worker added:** `marshalIo` (`Shell.fs:328`):
  `Dispatcher.UIThread.Post(fun () -> dispatch (RootMsg.Io msg))`.
- **Existing helper:** `marshalFit` (`Shell.fs:248-251`):
  `Dispatcher.UIThread.Post(fun () -> dispatch (RootMsg.Fit msg))`. Same module,
  added one slice earlier (007).
- **Why it matters:** The two functions are identical except for the `RootMsg`
  wrapper constructor. This is the §0.4 UI-thread-marshal seam — the single most
  safety-critical helper in the file (a missed marshal is "a defect" per §0.4).
  Having two copies means the §0.4 contract is now expressed in two places; a
  future fix to the marshal call (e.g. swapping `Post` for `InvokeAsync`, adding
  priority, or instrumenting it) has to be made twice or it silently diverges.
  Both copies live in `Shell.fs`, which is the composition root the worker already
  edits this slice — so unifying them is fully in-scope and touches no frozen
  module (§0.1 freezes the *logic* modules, not the shell).
- **Suggested action:** Extract one generic
  `marshal (wrap : 'm -> RootMsg) (dispatch) (msg : 'm) = Dispatcher.UIThread.Post(fun () -> dispatch (wrap msg))`
  and define `marshalFit = marshal RootMsg.Fit` / `marshalIo = marshal RootMsg.Io`,
  or call `marshal RootMsg.Io dispatch …` directly. Judge decides.

### F2: `preorderPaths` re-implements `ConstructionPage.walk` (and shadows `placeElements`' own DFS)

- **Worker added:** `preorderPaths` (`SystemView3DView.fs:52-53`) — a recursive
  pre-order DFS over `BeamNode.children` (`Map.toList |> List.collect`).
- **Existing helper:** `ConstructionPage.walk` (`ConstructionPage.fs:57-62`), a
  recursive pre-order DFS yielding `(NodePath * BeamNode) seq` in the **same**
  child order (`for KeyValue (b, c) in node.children`). It already underpins
  `subtreePaths` (`:64`) and `solveSubtree` (`:84`). A *third* copy of this exact
  walk is `placeElements`' inner `go` (`SystemView3D.fs:97-114`), which
  `preorderPaths` is deliberately written to mirror so the `List.zip placed paths`
  (`SystemView3DView.fs:99`) lines up.
- **Why it matters:** There are now three independent hand-rolled DFS traversals
  over `BeamNode` children that MUST agree on child ordering, and two of them
  (`preorderPaths` ↔ `placeElements.go`) are coupled by a positional `List.zip` —
  if either ever changes its child-iteration order, the zip silently pairs each
  element box with the *wrong* solved node's field and the beams render off the
  wrong elements with no error. The canonical enumerator (`walk`) already returns
  exactly the `(NodePath * BeamNode)` pairs this view wants.
- **Suggested action:** Ideally consume `ConstructionPage.walk` (it returns paths
  *and* nodes, eliminating both the separate traversal and the fragile zip).
  **But** `walk` is `private` and lives in `ConstructionPage.fs`, which §0.1
  freezes — exposing it is an edit to a frozen module, so direct reuse is blocked
  this arc. Given that, re-implementing in the new view is the defensible call
  (and the SoW documents the lockstep requirement). Flagging so the judge sees the
  duplication is freeze-forced, not careless; a follow-up that lifts a public
  `paths`/`enumerate` onto the construction page would retire all three copies.

### F3: `projectNameOfPath` hard-codes `".ocproj"` instead of `ProjectFile.extension`

- **Worker added:** `projectNameOfPath` (`Shell.fs:355` region) with the literal
  `".ocproj"` written twice (the `EndsWith` test and the `Substring` trim).
- **Existing helper:** `ProjectFile.extension = ".ocproj"` (`ProjectFile.fs:17`) —
  introduced precisely as "the canonical project file extension (§I.3)". The
  module is `open`ed in `Shell.fs` already (the diff `open OpticalConstructor.Storage`).
- **Why it matters:** The canonical extension constant exists to be the single
  source of truth; hand-coding the literal re-introduces the magic string the
  constant was meant to eliminate. If the project extension is ever renamed, the
  save/open seams update via the constant but this string-trim is missed, so
  reopened files would lose their name derivation. Low blast radius, but it is a
  textbook "reuse the existing constant" miss.
- **Suggested action:** Replace the `".ocproj"` literals with
  `ProjectFile.extension`. Judge decides.

### F4: `mount` / `buttonByContent` test scaffolding copied a 5th/6th time

- **Worker added:** private `mount` (`LifecycleTests.fs:36`, `SystemView3DTests.fs:57`)
  and private `buttonByContent` (`LifecycleTests.fs:43`).
- **Existing helper:** byte-identical private copies already exist in
  `FitPanelTests.fs:67,74`, `ChartPanelTests.fs:23`, `ConstructionEditTests.fs:31`,
  and `MaterialsPanelTests.fs:42`.
- **Why it matters:** It is the same `Window()`/`Component`/`Show()`/`RunJobs()`
  mount and the same `GetVisualDescendants |> choose Button |> find by Content`
  lookup, now copied into two more files. A shared test helper would be the reuse
  target — **but none exists**: every UI test module keeps its own private copy, so
  the worker is *consistent with established repo precedent*, not diverging from it.
  This is duplication of an existing pattern, not a fresh anti-pattern.
- **Suggested action:** Leave as-is for this arc (matches precedent; the
  `OpticalConstructor.Ui.Tests` project has no shared `TestHost`/`ViewProbe`
  module). A future cleanup could hoist `mount`/`buttonByContent` into one shared
  test module and retire all six copies — out of scope for U8.

### F5: `runUntil` generalizes FitPanelTests' inline pump-until loop

- **Worker added:** `runUntil` (`LifecycleTests.fs:51-60`) — run a `Cmd`'s effects
  with a collecting dispatch, then `RunJobs()`/`Sleep 10` spin (budget 500) until a
  predicate holds.
- **Existing helper:** the identical pump-until-marshaled loop inlined at
  `FitPanelTests.fs:116-121` (`while not (completed ()) && spins < 500 do
  Dispatcher.UIThread.RunJobs(); Thread.Sleep 10; …`).
- **Why it matters:** Same off-thread-Cmd test idiom, expressed twice. The worker
  actually *improved* it (named + parameterized by predicate), which is the right
  instinct — but FitPanelTests still carries the inline copy. Pure test code, no
  production risk; noting it so the judge has the full duplication picture.
- **Suggested action:** Acceptable as-is (sharing it would require editing the
  existing `FitPanelTests.fs`). If a shared test-helper module is ever created
  (see F4), `runUntil` belongs there.

## Bottom line

F1 (`marshalIo`/`marshalFit`) and F3 (`".ocproj"` vs `ProjectFile.extension`) are
clean, in-scope reuse misses that the worker could fix without touching any frozen
module, and F1 sits on the safety-critical §0.4 seam, so they are worth a fix; F2 is
a genuine and somewhat risky duplication (three coupled DFS walks) but reuse is
blocked by the §0.1 freeze on `ConstructionPage.fs`, so it is defensible-as-is with
a documented follow-up. F4/F5 are test-scaffolding duplications that merely follow
existing repo precedent (no shared test helper exists) and carry no production risk.
My read: the findings are real but low-to-moderate — F1 and F3 are quick polish
rather than a re-architecture, so this is closer to a "fix-and-proceed" than a
"re-spawn"; the judge owns that call.

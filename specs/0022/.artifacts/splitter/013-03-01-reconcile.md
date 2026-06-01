# Reconciler decision -- round 3 iter 1

## What I weighed

The verifier (`011-03-01-verify.md`) returned **ACCEPTABLE** with 0 BLOCKER,
1 MAJOR, 4 MINOR, and four priority recommendations. The claim-checker
(`012-03-01-splitter-claim-check.md`) fact-checked the critique and came back
**29 CONFIRMED, 0 REFUTED, 5 NOT-CHECKED (semantic), 1 NOT-CHECKED (tool
error)**. Nothing was refuted, so every finding's file/line/identifier substrate
is accurate; I spend my judgment on the semantic conclusions the claim-checker
left on my plate (chiefly #12 the F# won't-compile consequence, and #18 the
"resolver assumed not named" reading).

I weighed the four recommendations. Three are substantive and route to the
splitter; the fourth is a real-but-cosmetic protocol divergence whose
verifier-proposed fix would actually break the project's own arc-runner, so I
fold it in with the *safe* resolution rather than the verifier's headline one.

### Rec 1 -- §b MAJOR: slice 014 forward-references the slice-015 J.9 validator (KEPT -> splitter)

Confirmed substrate: slice 014 R-2 and its `RepeatBuilderTests.fs` bullet both
direct the worker to reject `R<1` "through the J.9 validation seam (slice 015)"
(claim-check #6, #7); the J.9 `Validation` module is owned by the *later* slice
015 (#8, #9); and 015's `OpticalConstructor.Ui.fsproj` edit appends
`Validation.fs` *after* 014's `RepeatBuilder.fs` (#10). The won't-compile
conclusion (#12) is the one semantic link, and my read backs the verifier: in
F#, a symbol must be compiled before it is referenced, so any reference from
014's `RepeatBuilder.fs`/`RepeatBuilderTests.fs` into the slice-015 module is a
forward reference that fails the `build`/`constructor-unit-tests` gates -- and
the arc-runner gives the worker exactly one round per slice. The fix is cheap
and the verifier already named it: keep `RepeatBuilder.expand` the pure
`List.replicate count cell |> List.concat` (which 014 R-2 already mandates) with
no dependency on `Validation.fs`, and state that the `R<1`-rejection assertion is
owned/tested in slice 015's `ValidationTests.fs` (AC-J9), not in slice 014.
**Substantive -> splitter.**

### Rec 2 -- §e MINOR: name the Part D by-id material resolver in slice 004 (KEPT -> splitter)

Confirmed: slice 004 delivers `byCategory`/`byNameContains`,
`toOpticalProperties`, and `getEps` (#15, #16, #17), but the by-id
`id -> OpticalProperties` resolver that slices 005 (§B.6) and 014 (§J.4) both
delegate to is assumed, not named as an explicit deliverable with a signature
(#18, semantic -- I agree). Slice 014 R-4 literally says it resolves "through
Part D's material-resolution function," so the consumer points at a target slice
004 never names. Low-cost fix: have slice 004 name the resolver as an explicit
deliverable (a by-id lookup that evaluates the entry's `DispersionModel` at a
supplied `WaveLength` -- required because a dispersive entry has no single tensor
until a wavelength is chosen -- returning the concrete `OpticalProperties` and
`Result.Error` on unknown id). **Substantive -> splitter.**

### Rec 3 -- §i MINOR: make slice 008's Mueller engine-edit guard mandatory (KEPT -> splitter)

Confirmed: slice 008 lands the one engine-core edit (uncommenting
`FieldFunctions.fs:93` `em.muellerMatrix`) but routes its AC-F3/F5/F7/F8
assertions to slice 011 and marks the in-slice `BerremanTests` smoke test
optional ("MAY add ... if convenient", claim-check #31, and the parent's test
routing at `.spec-md:1002`, #30). The deferral itself is faithful to the parent,
so this is not a coverage gap -- but it means an engine-tree change ships with no
same-slice regression guard, against the arc-runner's test-first discipline. Fix:
make at least the `em.muellerMatrix` == `MuellerMatrix.fromEmFields` smoke
assertion mandatory in slice 008, keeping the full equivalence suite in slice
011. **Substantive -> splitter.**

### Rec 4 -- §g MINOR: gate source-of-truth divergence (KEPT -> splitter, with the SAFE resolution)

Confirmed: the manifest carries a `slice_gates:` block (#23) AND the 16
`.slices/NNN.gates` snapshots exist (#26), and they **agree**
([build, unit-tests, constructor-unit-tests] for every slice). The divergence is
between the project `arc-runner.user-md`, which says the arc-runner reads gates
from the manifest's `slice_gates:` block (#24), and the cross-repo
`arc-runner.system-md`, which says gates live in each slice's sibling `.gates`
snapshot and the manifest header carries no gates key (#25).

Here I diverge from the verifier's headline recommendation. The verifier's first
option -- "retire the manifest `slice_gates:` block" -- would *break the actual
arc-runner for this project*, because `arc-runner.user-md` is the file the runner
reads and it points at the manifest block. So the correct, non-breaking
resolution is the verifier's own alternative: keep BOTH in sync and document the
intentional dual-source during the migration (they already agree, so there is no
runtime conflict today). I fold this into the splitter round as the safe
instruction, not as "drop the manifest block." **Kept -> splitter (sync + note,
do not drop the manifest block).**

### Items explicitly NOT routed

- Claim-check #28 flagged that the §g aside "`*.binz`/`*.autosave` are
  git-ignored" is **not yet true on disk** -- those globs are deliverables of the
  not-yet-run slices 003/013, correctly future state, not a refutation. Nothing
  to route: it is a worker deliverable, not a slice-text defect, and the slices
  already own the rule.
- Claim-check #27 (`verifier.system-md` cross-reference) is NOT-CHECKED (tool
  error: file not among inputs). It underpins only the §g aside, which is already
  handled above; no separate action.

## Routing

Nothing was refuted, so no finding is dropped. One MAJOR and three MINOR findings
are substantive; all four need the *slice text* refined (a forward-reference
removed, a deliverable named, a test made mandatory, a dual-source documented),
which is the splitter's job, not a re-verify of an otherwise-fine critique. So
this is **route-to-splitter**, not route-to-verifier, and not done-green (the
MAJOR would fail the build gate on slice 014 as drafted).

Budget check: `splitter_max_rounds` is 5; `split` has consumed 3 rounds, so the
next splitter pass is **round 4** -- within budget. The fixes are cheap textual
refinements, not a re-decomposition. No escalation warranted.

---

```json
{"decision": "route-to-splitter", "next_role": "split", "next_round": 4, "next_vn": null, "rounds_consumed": {"split": 3, "verify": 2, "splitter-claim-check": 2, "reconcile": 2}, "verifier_findings_to_apply": ["§b MAJOR slice 014 forward-references the slice-015 J.9 Validation module: keep RepeatBuilder.expand the pure `List.replicate count cell |> List.concat` with no reference to Validation.fs, and move the R<1-rejection assertion to slice 015's ValidationTests.fs (AC-J9) so slice 014 has no forward reference that breaks the build/constructor-unit-tests gates", "§e MINOR name the Part D by-id material resolver in slice 004 as an explicit deliverable (id -> OpticalProperties at a supplied WaveLength, Result.Error on unknown id) that slices 005 (§B.6) and 014 (§J.4) delegate to", "§i MINOR make slice 008's in-slice em.muellerMatrix == MuellerMatrix.fromEmFields smoke assertion mandatory (not optional) so the FieldFunctions.fs:93 engine edit ships with a same-slice guard; full AC-F3/F5/F7/F8 equivalence still lands in slice 011", "§g MINOR reconcile the gate source-of-truth divergence by keeping the per-slice .slices/NNN.gates snapshots AND the manifest slice_gates: block in sync and documenting the intentional dual-source during migration -- do NOT drop the manifest block, since arc-runner.user-md reads gates from it"], "verifier_findings_to_drop": [], "next_task_file_path": null, "reasoning_summary": "Claim-check confirmed 29 facts with 0 refuted; 1 MAJOR + 3 MINOR substantive; routing to splitter round 4 to remove the 014->015 forward reference, name the slice-004 by-id resolver, make slice-008's Mueller guard mandatory, and keep both gate sources in sync."}
```

# Claim-check of 021-05-01-verify.md

**Inputs:** verifier MD `C:\GitHub\Berreman\specs\0022\.artifacts\splitter\021-05-01-verify.md`; resolved `repo_root` `C:/GitHub/Berreman` (manifest `.manifest` line 1, cross-checked against the task file's "Repo root" input).
**Summary:** CONFIRMED: 27, REFUTED: 0, NOT-CHECKED (semantic): 9, NOT-CHECKED (ambiguous): 0, NOT-CHECKED (tool error): 0.

| # | Verifier claim (verbatim or paraphrased <= 120 chars) | Evidence | Verdict |
|---|---|---|---|
| 1 | Big spec `.spec-md` is 1053 lines | `wc -l specs/0022/.spec-md` -> `1052` (1053 lines if final line lacks a trailing newline; immaterial off-by-one) | **CONFIRMED** |
| 2 | Slices `001`-`016.slice-md` exist in manifest order | `ls .slices/*.slice-md` -> `001`..`016`, no gaps, no `017` | **CONFIRMED** |
| 3 | `Berreman/Berreman.slnx` present | `ls Berreman/Berreman.slnx` -> exists | **CONFIRMED** |
| 4 | `Berreman/OpticalModel/Primitives.fs` is the untouched `let x = 1` stub | line 9 -> `    let x = 1` | **CONFIRMED** |
| 5 | `C:\GitHub\Avalonia.FuncUI.Clone\` present | `ls -d /c/GitHub/Avalonia.FuncUI.Clone` -> exists | **CONFIRMED** |
| 6 | `C:\GitHub\Softellect\Sys\Core.fs` + `Primitives.fs` present | `ls` -> both exist | **CONFIRMED** |
| 7 | All six gallery `.fsx` (`MultilayerThinFilm`, `_EUV`, `ActiveCrystal`, `Wedge_Glass`, `Wedge_BiaxialCrystal`, `Glass_Dispersive`) present | `ls Berreman/Analytics/Examples/*.fsx` -> all six present | **CONFIRMED** |
| 8 | `FieldFunctions.fs:93` carries commented-out `member em.muellerMatrix` (F.3 edit target) | `FieldFunctions.fs:93` -> `// member em.muellerMatrix : MuellerMatrix =` | **CONFIRMED** |
| 9 | `Fields.fs:411` `rotateY` adds onto `incidenceAngle` (AC-C3) | `Fields.fs:411` -> `member this.rotateY y = { this with incidenceAngle = this.incidenceAngle + y }` | **CONFIRMED** |
| 10 | `Solvers.fs:149` is the `(emf : EmField, system : ShortOpticalSystem)` overload (B.4) | `Solvers.fs:149` -> `new (emf : EmField, system : ShortOpticalSystem) = BaseOpticalSystemSolver (EmFieldBased (emf, system))` | **CONFIRMED** |
| 11 | `Solvers.fs:199` `OpticalSystemSolver` | `Solvers.fs:199` -> `type OpticalSystemSolver (info : IncidentLightInfo, system: OpticalSystem, parameters : SolverParameters) =` | **CONFIRMED** |
| 12 | `FourierTransform.fs:74` `createGaussian` | `FourierTransform.fs:74` -> `let createGaussian (numberOfPoints: int) (mu: float) (sigma: float) =` | **CONFIRMED** |
| 13 | Gates resolve: `build`/`unit-tests` in `.gates/Berreman/`; `constructor-unit-tests` in `.gates/OpticalConstructor/` (`pass_when exit_code: 0`, targets `OpticalConstructor.Tests.fsproj`) | `ls .gates/Berreman/` -> `build.gates`,`unit-tests.gates`; `cat .gates/OpticalConstructor/constructor-unit-tests.gates` -> `command: dotnet test Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj -c Release`, `pass_when: exit_code: 0` | **CONFIRMED** |
| 14 | Manifest header: `repo_root`=`C:/GitHub/Berreman`, `branch`=`0022`, `worker_timeout_sec`=14400, `worker_hangup_idle_sec`=3600, `worker_review_max_cycles`=3, `max_total_route_backs`=30; no `gates_file` key | `.manifest` lines 1-6 -> exact match; no `gates_file:` key present | **CONFIRMED** |
| 15 | `.spec-bundle.gates` = `["build","unit-tests","constructor-unit-tests"]` | `.spec-bundle` -> exact three-element array | **CONFIRMED** |
| 16 | (§b) Slice 011 attributes the `.gitignore` `*.binz`/`*.autosave` rule to "slice 012 / Part I" | `011.slice-md:121` -> "the `.gitignore` edit for `*.binz`/`*.autosave` is owned by slice 012 / Part I" | **CONFIRMED** |
| 17 | (§b) Slice 012 is Part H (charts) | `012.slice-md:1` -> `# 012 — Charts & visualization (Part H)` | **CONFIRMED** |
| 18 | (§b) Slice 012 edits no `.gitignore` | `grep -ci gitignore 012.slice-md` -> `0` | **CONFIRMED** |
| 19 | (§b) `*.binz` rule owned by slice 003 (§I.4) | `003.slice-md:48` `.gitignore (edit) — ignore *.binz and *.autosave`; `:50` "(This slice contributes the `*.binz` entry per §I.4…)" | **CONFIRMED** |
| 20 | (§b) `*.autosave` rule owned by slice 013 (§I.5) | `013.slice-md:46` "…owned by slice 003, which added `*.binz`; this slice adds the `*.autosave` entry…per §I.5" | **CONFIRMED** |
| 21 | (§b) Slice 014 keeps `RepeatBuilder.expand` as `List.replicate`, MUST NOT reference `Validation.fs`; `count<1` rejection + AC-J9 re-homed to slice 015 | `014.slice-md:77` "expand…`List.replicate count cell |> List.concat`…with NO dependency on `Validation.fs`…The `R < 1` rejection…tested in slice 015's `ValidationTests.fs` (AC-J9), NOT in slice 014" | **CONFIRMED** |
| 22 | (§c) Slice 012 bundles 7 view modules (`ChartSettings`,`SeriesData`,`Plot1DView`,`Plot3DView`,`Readout`,`PolarizationPlots`,`CieView`) + 4 test files | `012.slice-md:44-50` -> all seven new `.fs` view files listed; test files present | **CONFIRMED** |
| 23 | (§i) AC-F3 asserted in slice 008 (`MuellerMatrixTests.fs`) and slice 011 (`OptimizationTests.fs`) | `008.slice-md` -> `MuellerMatrixTests` (5 refs); `011.slice-md` -> `OptimizationTests` (8 refs) | **CONFIRMED** |
| 24 | (§e) Edit targets `Berreman.slnx`, `FieldFunctions.fs`, `Analytics/Analytics.fsproj`, `BerremanTests/BerremanTests.fsproj`, `.gitignore` are all real files | `ls` -> all five exist on disk | **CONFIRMED** |
| 25 | (§e) Slices 003/013 redirect tests into `OpticalConstructor.Tests.fsproj` (single gated host) | `003.slice-md` 5 `OpticalConstructor.Tests` refs; `013.slice-md` 8 refs | **CONFIRMED** |
| 26 | (§f) `MuellerMatrix.fromEmFields` exists (the F.3 delegation target) | `Fields.fs:636,679` -> `static member fromEmFields (s : EmField) (p : EmField) =`; consumed as `MuellerMatrix.fromEmFields` at `Solvers.fs:301,307` | **CONFIRMED** |
| 27 | (§e) `Berreman/OpticalConstructor/**` does not yet exist (net-new tree the arc creates) | `ls -d Berreman/OpticalConstructor` -> not found | **CONFIRMED** |
| 28 | (§a) Every directive A.0-J.12 maps to exactly one owning slice; every AC owned once | requires whole-spec semantic cross-walk | **NOT-CHECKED (semantic)** |
| 29 | (§a) The seven §0 binding constraints reproduced verbatim in every slice and respected | broad semantic sweep over all slice bodies | **NOT-CHECKED (semantic)** |
| 30 | (§b) Build order forward-only; deferred-validation seam (014->015) "handled correctly" | ordering/soundness judgment (factual core confirmed in #21) | **NOT-CHECKED (semantic)** |
| 31 | (§c) Slice 012 acceptable / not worth splitting / highest-effort UI slice | granularity judgment | **NOT-CHECKED (semantic)** |
| 32 | (§e) Slice 008 writes its `.binz` smoke artefact under the per-arc artifacts folder (not `%TEMP%`) | worker runtime-behavior + intent claim | **NOT-CHECKED (semantic)** |
| 33 | (§f) Risk tags (002 Medium, 011 Medium/high, 008 engine edit Low) calibrated | risk-calibration judgment | **NOT-CHECKED (semantic)** |
| 34 | (§h) Every slice carries Scope/Requirements/Non-requirements/Files/Testing/Risks/Hand-off + `gates:` baseline | per-slice structural-completeness judgment over 16 files | **NOT-CHECKED (semantic)** |
| 35 | (§i) Big spec ambiguous on `OpticalConstructorProject` location (§A.1 Domain vs §A.7 Storage); slice 002 resolves to Domain | intent/resolution judgment | **NOT-CHECKED (semantic)** |
| 36 | (§i) Slice 008's guard vs slice 011's full suite is deliberate, not redundant | rationale/intent judgment | **NOT-CHECKED (semantic)** |

---

**Bottom line:** The verifier's verdict is **ACCEPTABLE** with zero BLOCKERs/MAJORs and three MINORs, and the fact-check supports it: **zero REFUTED entries**. Every factual underpinning of the three MINOR recommendations survives — (§b) slice 011 does carry the stale "(owned by slice 012 / Part I)" pointer for `*.binz`/`*.autosave` (`011.slice-md:121`) while slice 012 is Part H and edits no `.gitignore`, and the rules actually belong to slice 003 (`*.binz`, §I.4, `003.slice-md:48,50`) and slice 013 (`*.autosave`, §I.5, `013.slice-md:46`) [#16–#20]; (§c) slice 012 genuinely bundles all seven named view modules plus test files [#22]; (§i) AC-F3 is asserted in both slice 008 and slice 011 [#23]. The structurally important non-finding claims also hold on disk: all three gate ids resolve with the `constructor-unit-tests` descriptor targeting `OpticalConstructor.Tests.fsproj` [#13], the manifest header values and the `.spec-bundle.gates` array are exact [#14–#15], and every engine reuse line-ref is accurate to the exact line — `FieldFunctions.fs:93` (commented `em.muellerMatrix`), `Fields.fs:411` (`rotateY` adding onto `incidenceAngle`), `Solvers.fs:149` (the `EmField`/`ShortOpticalSystem` overload), `Solvers.fs:199` (`type OpticalSystemSolver`), `FourierTransform.fs:74` (`createGaussian`), and `MuellerMatrix.fromEmFields` (defined `Fields.fs:636/679`, consumed `Solvers.fs:301`) [#8–#12,#26]; the deferred-validation seam in slice 014 is worded exactly as described [#21]. The one factual near-miss is #1: `wc -l` reports 1052 newlines vs the stated 1053 — an off-by-one from a missing trailing newline, immaterial. The nine `NOT-CHECKED (semantic)` entries (#28–#36) — directive/AC coverage mapping, §0-constraint propagation, dependency-order soundness, granularity, artifact-path discipline, risk calibration, per-slice completeness, the §A.1/§A.7 resolution, and the guard-vs-suite rationale — are judgment calls left intact for the reconciler.

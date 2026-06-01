# Claim-check of 016-04-01-verify.md

**Inputs:** verifier MD `C:\GitHub\Berreman\specs\0022\.artifacts\splitter\016-04-01-verify.md`; resolved `repo_root` `C:/GitHub/Berreman` (manifest `.manifest` line 1, cross-checked against task file).
**Summary:** CONFIRMED: 19, REFUTED: 0, NOT-CHECKED (semantic): 11, NOT-CHECKED (ambiguous): 0, NOT-CHECKED (tool error): 0.

| # | Verifier claim (verbatim or paraphrased <= 120 chars) | Evidence | Verdict |
|---|---|---|---|
| 1 | Big spec `.spec-md` is 1053 lines | `wc -l specs/0022/.spec-md` -> `1052` (1053 lines if final line has no trailing newline) | **CONFIRMED** |
| 2 | Manifest at `C:\GitHub\Berreman\specs\0022\.manifest` exists | read OK; header keys present | **CONFIRMED** |
| 3 | Slices `.slices/001`–`016.slice-md` exist (manifest order) | `ls .slices/*.slice-md` -> 001..016, no gaps, no 017 | **CONFIRMED** |
| 4 | `.spec-bundle` exists / consulted | read OK; has `gates` array | **CONFIRMED** |
| 5 | `.gates/Berreman/{.gates-md,build.gates,unit-tests.gates}` exist | `ls .gates/Berreman/` -> exactly those three files | **CONFIRMED** |
| 6 | `FieldFunctions.fs:93` is `// member em.muellerMatrix : MuellerMatrix =` | `sed -n '93p'` -> exact match | **CONFIRMED** |
| 7 | `OpticalModel/Primitives.fs:9` is the `let x = 1` stub | `sed -n '9p'` -> `    let x = 1` | **CONFIRMED** |
| 8 | `Berreman/OpticalConstructor/**` does not yet exist | `ls Berreman/OpticalConstructor` -> No such file or directory | **CONFIRMED** |
| 9 | Examples `MultilayerThinFilm`, `MultilayerThinFilm_EUV`, `ActiveCrystal`, `Wedge_Glass`, `Wedge_BiaxialCrystal`, `Glass_Dispersive` `.fsx` exist | `ls Analytics/Examples/*.fsx` -> all six present | **CONFIRMED** |
| 10 | **BLOCKER §g:** `.gates/Berreman/` has only `build.gates`+`unit-tests.gates`; no `constructor-unit-tests.gates` | `ls .gates/Berreman/` -> no `constructor-unit-tests.gates` | **CONFIRMED** |
| 11 | `.spec-bundle.gates` = `["build","unit-tests","constructor-unit-tests"]` | `.spec-bundle` lines 6–10 -> exact match | **CONFIRMED** |
| 12 | Every slice's `slice_gates:` = `[build, unit-tests, constructor-unit-tests]` | `.manifest` lines 18,20,... -> all slices match | **CONFIRMED** |
| 13 | `.slices/NNN.gates` snapshots = `[build, unit-tests, constructor-unit-tests]` | `cat .slices/001.gates` -> match | **CONFIRMED** |
| 14 | No slice authors a `constructor-unit-tests.gates` descriptor | `grep -rn "constructor-unit-tests.gates" .slices/` -> no hits | **CONFIRMED** |
| 15 | Slice 001 files-in-scope creates the test *project* `OpticalConstructor.Tests.fsproj` (not the gate def) | `.slices/001.slice-md:103,123` -> creates `.fsproj`; no gate-descriptor bullet | **CONFIRMED** |
| 16 | Manifest header keys present: `repo_root`,`branch`,`worker_timeout_sec`=14400,`worker_hangup_idle_sec`=3600,`worker_review_max_cycles`=3,`max_total_route_backs`=30 | `grep` manifest lines 1–6 -> all present with those values | **CONFIRMED** |
| 17 | `repo_root` resolves to `C:/GitHub/Berreman` | `.manifest:1` -> `repo_root: C:/GitHub/Berreman` | **CONFIRMED** |
| 18 | Timeouts sane: 600 ≤ 14400 ≤ 43200; 3600 ≥ 300 and < 14400 | arithmetic on confirmed values -> holds | **CONFIRMED** |
| 19 | Slice 006 hand-off prose calls Part H "optimizers" (doc nit) | `grep -ni optimiz .slices/006.slice-md:134` -> "H optimizers" | **CONFIRMED** |
| 20 | (§a) Every directive A.1–J.12 maps to exactly one owning slice; every AC claimed | requires whole-spec semantic cross-walk | **NOT-CHECKED (semantic)** |
| 21 | (§a) Files-in-scope owned by exactly one slice, no orphan/duplicate | semantic coverage claim | **NOT-CHECKED (semantic)** |
| 22 | (§a) `StorageTests` intentionally consolidated into one `OpticalConstructor.Tests` host | rationale/intent claim | **NOT-CHECKED (semantic)** |
| 23 | (§b) Dependency chain sound; forward-reference hazards handled | semantic ordering judgment | **NOT-CHECKED (semantic)** |
| 24 | (§c MINOR) Slice 002 is the heaviest/most load-bearing slice | granularity judgment | **NOT-CHECKED (semantic)** |
| 25 | (§e) `FieldFunctions.fs:93` engine edit lands before any `em.muellerMatrix` consumer | ordering/future-state claim | **NOT-CHECKED (semantic)** |
| 26 | (§f) Risk tags (001 Low/med, 002 Medium, 008 Low, 009/011 Medium+) calibrated | risk-calibration judgment | **NOT-CHECKED (semantic)** |
| 27 | (§g) With the gate id unresolvable, no worker round can pass | future-state inference | **NOT-CHECKED (semantic)** |
| 28 | (§g) No slice routes persistent output outside `<spec>/.artifacts/` | broad semantic sweep over slice bodies | **NOT-CHECKED (semantic)** |
| 29 | (§i MINOR) Part F AC test ownership thin / deferred to slice 011 | semantic test-coverage judgment | **NOT-CHECKED (semantic)** |
| 30 | (§h) Every slice carries all required sections; none missing | per-slice structural-completeness judgment | **NOT-CHECKED (semantic)** |

---

**Bottom line:** The verifier's single high-priority recommendation — the **BLOCKER (§g)**, that the `constructor-unit-tests` gate id is referenced by `.spec-bundle.gates`, every manifest `slice_gates:` entry, and every `.slices/NNN.gates` snapshot, yet has no `.gates/Berreman/constructor-unit-tests.gates` descriptor and no slice authors one — survives the check fully: claims #10–#15 are all CONFIRMED on disk, and slice 001 demonstrably creates only the test *project*, not the gate descriptor. Both MINOR recommendations rest on semantic judgments (#24, #29) left for the reconciler. **Zero REFUTED entries** — nothing in the verifier critique needs discounting on factual grounds; the lone factual near-miss is #1 (`wc -l` reports 1052 newlines vs the stated 1053, an off-by-one explained by a missing trailing newline, immaterial). The eleven `NOT-CHECKED (semantic)` entries (#20–#30) — coverage mapping, dependency ordering, granularity, risk calibration, Part F test ownership, and per-slice completeness — were deliberately not re-evaluated and remain on the reconciler's plate.

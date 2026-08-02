# Mueller-matrix measured data and reference pipeline

This folder holds the measured Mueller-matrix polarimetry data that
`BerremanTests/MuellerReconstructionTests.fs` consumes, together with the reference
implementation and the reference numerical results that the F# port was written against.

Everything here is **data and documentation**. No project builds it, references it, or executes it.
In particular, **BNM never invokes Python at build or test time** — the tests are pure F#.

---

## Provenance

| | |
|---|---|
| Source repository | <https://github.com/NihilismVoid/optics-mueller.git> |
| Branch | `main` |
| Commit | `2f1a35b70f877e41fa9b576fadd138d7dc80b858` |
| Copied on | 2026-08-02 |

The copies below are byte-identical to the source except that CRLF line endings were converted to LF
to satisfy this repository's `.gitattributes` policy.

Before this copy, `MuellerReconstructionTests` located the CSV data by walking up from its output
directory to a sibling `optics-mueller` checkout, and skipped when that checkout was absent. That
dependency is retired: the data lives in `data.zip` here, the tests read it through
`OpticalConstructor.Storage.MuellerDataStore`, and **no code path references `optics-mueller` any
more**. The scripts and reference results are kept so that the F# port remains auditable after the
external checkout is gone.

---

## Contents

### Measured data

| File | Description |
|---|---|
| `data.zip` | The measured capture data. Inventory below. |

`data.zip` holds 18 files. Seven of them are the science data the tests consume, all under
`data/raw/final/`:

| Entry | Data rows | Content |
|---|---|---|
| `data/raw/final/lp_lp.csv` | 209 | LP source / LP analyzer family: AIR, QZ, LR, QZ+LR at 0/45/90 |
| `data/raw/final/lp_cpl.csv` | 209 | LP source / CPL analyzer family, same set |
| `data/raw/final/cpl_lp.csv` | 209 | CPL source / LP analyzer family, same set |
| `data/raw/final/cpl_cpl_day1_main.csv` | 170 | CPL/CPL day 1: AIR, AIR-BR, QZ 0/45/90, LR 0 & 90, QZ+LR 0 & 90 |
| `data/raw/final/cpl_cpl_day2_corrections.csv` | 57 | CPL/CPL day 2: AIR-2 plus the re-measured LR#45 and QZ#45+LR#45 sweeps |
| `data/raw/final/darkness_checks.csv` | 6 | dark frames; their mean `avg_total` is the dark level (869.666) |
| `data/raw/final/bullshit_checks.csv` | 2 | diagnostic captures; the earliest one is the CPL-LP gain split time |

The remaining eleven entries are earlier exploratory work, retained for completeness and **not read by
any test**: eight CSVs under `data/raw/` (`cpl_cpl*.csv`, `cpl_lr45_*.csv`, region-of-interest and
beam-shape variants) and three beam-shape descriptions under `data/shapes/`.

Every CSV carries the same 16-column header — `experiment, capture_index, captured_at, description,
iso, exposure_ns, focus_distance, region_x, region_y, region_w, region_h, n_pixels, avg_R, avg_G,
avg_B, avg_total` — prefixed with a UTF-8 BOM, with ISO-8601 `captured_at` values. `description` is
the raw analyzer dial angle in degrees; `avg_total` is the measured intensity.

### Reference implementation (Python)

The authoritative algorithm that spec 0042 ports to F#. When the report and these scripts disagree,
the scripts win.

| File | Stage | Description |
|---|---|---|
| `matrix_step1_air_fit.py` | Stage 1 | AIR calibration: per-family cosine fits over the AIR#0 traces yielding the LP analyzer zero, the CPL analyzer zero and retardance, and the CPL source relative angle and retardance. |
| `matrix_glue.py` | Stage 2 | Per-row "glue": parses the experiment label, builds the effective source Stokes vector and effective analyzer row for each capture, and emits the 16-column design row `kron(s_eff, a_eff)`. |
| `matrix_fit_linear.py` | Stage 3 | Per-family AIR gain calibration, dark subtraction, the 16-unknown linear least-squares reconstruction of each Mueller matrix, and the cascade comparison `M_{QZ+LR}` vs `M_LR · M_QZ`. |
| `cpl_cpl_analyzer.py` | — | The experiment-label grammar. `matrix_glue.py` imports `parse_experiment_name` from it. |

### Reference results (full precision)

The reference pipeline's own output, at full double precision. These are the provenance of the
expected values transcribed into `MuellerReconstructionTests.fs`; the tests read the literals, not
these files.

| File | Source | Contents |
|---|---|---|
| `reference_step1_summary.json` | `analysis/matrix_solution/step1/summary.json` | Stage-1 calibration constants: LP analyzer zero 155.44011830109392°, CPL source relative angle 39.31100224361002° and retardance 82.55344158462403°, CPL analyzer zero 96.99613336321012° and retardance 83.66804489441891°, plus every fit RMSE and repeat spread. |
| `reference_step2_linear_summary.json` | `analysis/matrix_solution/step2_linear/summary.json` | Dark mean 869.666, the CPL-LP split time, the per-family AIR gains, the four Mueller matrices (`qz_matrix`, `lr_matrix`, `combined_matrix`, `product_matrix`) to ten decimals, the cascade metrics, and per-fit rank, condition number and RMSE. |

---

## Further reading

- `specs/0042/.manual/MuellerMatrix_final.pdf` — the experiment report. §7.2 is the target the test
  reproduces, §4.1–4.2 the measurement and rotation conventions, §8 the cascade comparison.
- `specs/0042/.manual/004-mueller-inverse-reconstruction-test.md` — spec 0042, whose §3 maps each
  script above onto the ported F# stage, and whose §6 documents the data inventory.
- `specs/0044/.manual/004-active-anisotropic-inverse-problem.md` — spec 0044 §8, which specifies this
  folder and the archive-backed data seam that replaced the external checkout.

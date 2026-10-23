from __future__ import annotations

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd


GLUE_FILES = [
    "cpl_cpl_day1_main_glue.csv",
    "cpl_cpl_day2_corrections_glue.csv",
    "cpl_lp_glue.csv",
    "lp_cpl_glue.csv",
    "lp_lp_glue.csv",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Fit first-pass Mueller matrices from the calibrated glue tables.")
    parser.add_argument("--raw-data-dir", required=True, type=Path, help="Directory with raw final-session CSV exports.")
    parser.add_argument("--glue-dir", required=True, type=Path, help="Directory with derived glue CSVs.")
    parser.add_argument("--diagnostics-dir", required=True, type=Path, help="Directory with final diagnostics outputs.")
    parser.add_argument("--out-dir", required=True, type=Path, help="Directory for fit outputs.")
    parser.add_argument("--value-col", default="avg_total", help="Raw signal column to fit. Default: avg_total")
    return parser.parse_args()


def load_glue_tables(glue_dir: Path) -> pd.DataFrame:
    frames: list[pd.DataFrame] = []
    for name in GLUE_FILES:
        path = glue_dir / name
        frame = pd.read_csv(path)
        frame["source_glue_file"] = name
        frames.append(frame)
    df = pd.concat(frames, ignore_index=True)
    df["captured_at"] = pd.to_datetime(df["captured_at"], utc=True)
    numeric_columns = [
        "description_deg",
        "object_phi_deg",
        "analyzer_physical_deg",
        "capture_index",
    ]
    for prefix in ("s_base_", "s_eff_", "a_base_", "a_eff_"):
        numeric_columns.extend([f"{prefix}{idx}" for idx in range(4)])
    numeric_columns.extend([f"lincoef_m{row}{col}" for col in range(4) for row in range(4)])
    numeric_columns.append("capture_index")
    numeric_columns.append("description")
    numeric_columns.append("n_pixels")
    for channel in ("avg_R", "avg_G", "avg_B", "avg_total"):
        numeric_columns.append(channel)
    for column in numeric_columns:
        if column in df.columns:
            df[column] = pd.to_numeric(df[column], errors="coerce")
    df["exclude_from_fit"] = df["exclude_from_fit"].astype(str).str.lower().eq("true")
    return df


def identity_prediction(df: pd.DataFrame) -> np.ndarray:
    a = df[[f"a_eff_{idx}" for idx in range(4)]].to_numpy(float)
    s = df[[f"s_eff_{idx}" for idx in range(4)]].to_numpy(float)
    return np.einsum("ij,ij->i", a, s)


def fit_scalar_gain(signal: np.ndarray, model: np.ndarray) -> float:
    denom = float(np.dot(model, model))
    if denom <= 0:
        raise ValueError("Degenerate AIR model prediction while fitting scalar gain.")
    return float(np.dot(signal, model) / denom)


def assign_gain_model(df: pd.DataFrame, raw_data_dir: Path, diagnostics_dir: Path, value_col: str) -> tuple[pd.DataFrame, dict[str, object]]:
    out = df.copy()
    dark_mean = float(pd.read_csv(raw_data_dir / "darkness_checks.csv")[value_col].mean())
    out["signal_dark_sub"] = out[value_col] - dark_mean
    out["air_identity_pred"] = identity_prediction(out)
    out["gain_est"] = np.nan
    out["gain_rule"] = ""

    diagnostics = json.loads((diagnostics_dir / "bullshit_checks_comparison.json").read_text(encoding="utf-8"))
    split_time = pd.to_datetime(diagnostics[0]["bullshit_captured_at"], utc=True)

    gain_summary: dict[str, object] = {"dark_mean": dark_mean, "split_time_utc": split_time.isoformat()}

    def fit_air_gain(mask: pd.Series) -> tuple[float, pd.Timestamp]:
        air_rows = out.loc[mask].copy()
        gain = fit_scalar_gain(air_rows["signal_dark_sub"].to_numpy(float), air_rows["air_identity_pred"].to_numpy(float))
        mid_time = air_rows["captured_at"].mean()
        return gain, mid_time

    # CPL-CPL old day1: interpolate scalar gain between AIR start and AIR end.
    mask_day1 = out["source_glue_file"].eq("cpl_cpl_day1_main_glue.csv")
    mask_air_start = mask_day1 & out["experiment"].eq("CPL-(AIR#0)-CPL")
    mask_air_end = mask_day1 & out["experiment"].eq("CPL-(AIR#0)-CPL-BR")
    gain_start, time_start = fit_air_gain(mask_air_start)
    gain_end, time_end = fit_air_gain(mask_air_end)
    gain_summary["cpl_cpl_day1"] = {
        "gain_start": gain_start,
        "gain_end": gain_end,
        "time_start_utc": time_start.isoformat(),
        "time_end_utc": time_end.isoformat(),
    }
    dt = (time_end - time_start).total_seconds()
    if dt <= 0:
        raise ValueError("Non-positive AIR interval for CPL-CPL day1 gain interpolation.")
    idx = out.index[mask_day1]
    weights = ((out.loc[idx, "captured_at"] - time_start).dt.total_seconds() / dt).clip(lower=0.0, upper=1.0)
    out.loc[idx, "gain_est"] = (1.0 - weights) * gain_start + weights * gain_end
    out.loc[idx, "gain_rule"] = "cpl_cpl_day1_interp"
    out.loc[mask_air_start, "gain_est"] = gain_start
    out.loc[mask_air_start, "gain_rule"] = "cpl_cpl_day1_air_start"
    out.loc[mask_air_end, "gain_est"] = gain_end
    out.loc[mask_air_end, "gain_rule"] = "cpl_cpl_day1_air_end"

    # CPL-CPL day2 corrections: one AIR block.
    mask_day2 = out["source_glue_file"].eq("cpl_cpl_day2_corrections_glue.csv")
    gain_day2, time_day2 = fit_air_gain(mask_day2 & out["experiment"].eq("CPL-(AIR#0)-CPL-2"))
    gain_summary["cpl_cpl_day2"] = {"gain": gain_day2, "time_utc": time_day2.isoformat()}
    out.loc[mask_day2, "gain_est"] = gain_day2
    out.loc[mask_day2, "gain_rule"] = "cpl_cpl_day2_air2"

    # LP-LP: stable family, use mean AIR gain from the two valid repeats.
    mask_lp_lp = out["source_glue_file"].eq("lp_lp_glue.csv")
    gain_lp_lp_a, _ = fit_air_gain(mask_lp_lp & out["experiment"].eq("LP-(AIR#0)-LP-2"))
    gain_lp_lp_b, _ = fit_air_gain(mask_lp_lp & out["experiment"].eq("LP-(AIR#0)-LP-2R"))
    gain_lp_lp = float(np.mean([gain_lp_lp_a, gain_lp_lp_b]))
    gain_summary["lp_lp"] = {"gain_air2": gain_lp_lp_a, "gain_air2r": gain_lp_lp_b, "gain_mean": gain_lp_lp}
    out.loc[mask_lp_lp, "gain_est"] = gain_lp_lp
    out.loc[mask_lp_lp, "gain_rule"] = "lp_lp_mean_air"

    # LP-CPL: stable family, use mean AIR gain from the two valid repeats.
    mask_lp_cpl = out["source_glue_file"].eq("lp_cpl_glue.csv")
    gain_lp_cpl_a, _ = fit_air_gain(mask_lp_cpl & out["experiment"].eq("LP-(AIR#0)-CPL-2"))
    gain_lp_cpl_b, _ = fit_air_gain(mask_lp_cpl & out["experiment"].eq("LP-(AIR#0)-CPL-2R"))
    gain_lp_cpl = float(np.mean([gain_lp_cpl_a, gain_lp_cpl_b]))
    gain_summary["lp_cpl"] = {"gain_air2": gain_lp_cpl_a, "gain_air2r": gain_lp_cpl_b, "gain_mean": gain_lp_cpl}
    out.loc[mask_lp_cpl, "gain_est"] = gain_lp_cpl
    out.loc[mask_lp_cpl, "gain_rule"] = "lp_cpl_mean_air"

    # CPL-LP: split at the bullshit check and use pre/post AIR gains.
    mask_cpl_lp = out["source_glue_file"].eq("cpl_lp_glue.csv")
    gain_cpl_lp_pre, _ = fit_air_gain(mask_cpl_lp & out["experiment"].eq("CPL-(AIR#0)-LP-2"))
    gain_cpl_lp_post, _ = fit_air_gain(mask_cpl_lp & out["experiment"].eq("CPL-(AIR#0)-LP-2R"))
    gain_summary["cpl_lp"] = {
        "gain_pre_air2": gain_cpl_lp_pre,
        "gain_post_air2r": gain_cpl_lp_post,
    }
    pre_mask = mask_cpl_lp & out["captured_at"].lt(split_time)
    post_mask = mask_cpl_lp & ~out["captured_at"].lt(split_time)
    out.loc[pre_mask, "gain_est"] = gain_cpl_lp_pre
    out.loc[pre_mask, "gain_rule"] = "cpl_lp_pre_air2"
    out.loc[post_mask, "gain_est"] = gain_cpl_lp_post
    out.loc[post_mask, "gain_rule"] = "cpl_lp_post_air2r"

    if out["gain_est"].isna().any():
        missing = out.loc[out["gain_est"].isna(), ["experiment", "source_glue_file"]].drop_duplicates()
        raise ValueError(f"Missing gain assignments:\n{missing}")

    out["signal_corrected"] = out["signal_dark_sub"] / out["gain_est"]
    out["air_identity_residual"] = out["signal_corrected"] - out["air_identity_pred"]
    return out, gain_summary


def matrix_from_vector(vec: np.ndarray) -> np.ndarray:
    return np.array(vec, dtype=float).reshape((4, 4), order="F")


def solve_single_object(df: pd.DataFrame, matrix_kind: str) -> tuple[np.ndarray, dict[str, object], pd.DataFrame]:
    fit_rows = df[
        df["effective_matrix_kind"].eq(matrix_kind)
        & ~df["exclude_from_fit"]
        & ~df["experiment"].str.contains("(AIR#0)", regex=False)
    ].copy()
    coef_cols = [f"lincoef_m{row}{col}" for col in range(4) for row in range(4)]
    X = fit_rows[coef_cols].to_numpy(float)
    y = fit_rows["signal_corrected"].to_numpy(float)
    beta, *_ = np.linalg.lstsq(X, y, rcond=None)
    predictions = X @ beta
    residuals = y - predictions
    singular_values = np.linalg.svd(X, compute_uv=False)
    rank = int(np.sum(singular_values > 1e-10))
    condition = float(singular_values[0] / singular_values[-1]) if singular_values[-1] > 0 else float("inf")

    fit_rows["prediction_single"] = predictions
    fit_rows["residual_single"] = residuals

    summary = {
        "n_rows": int(len(fit_rows)),
        "rank": rank,
        "unique_least_squares_solution": bool(rank == 16),
        "condition_number": condition,
        "rmse": float(np.sqrt(np.mean(residuals**2))),
        "mean_abs_residual": float(np.mean(np.abs(residuals))),
        "max_abs_residual": float(np.max(np.abs(residuals))),
    }
    return beta, summary, fit_rows


def predict_with_matrix(df: pd.DataFrame, matrix_vec: np.ndarray) -> np.ndarray:
    coef_cols = [f"lincoef_m{row}{col}" for col in range(4) for row in range(4)]
    X = df[coef_cols].to_numpy(float)
    return X @ matrix_vec


def predict_combined(df: pd.DataFrame, qz_vec: np.ndarray, lr_vec: np.ndarray) -> np.ndarray:
    M_qz = matrix_from_vector(qz_vec)
    M_lr = matrix_from_vector(lr_vec)
    values: list[float] = []
    for _, row in df.iterrows():
        a_eff = row[[f"a_eff_{idx}" for idx in range(4)]].to_numpy(float)
        s_eff = row[[f"s_eff_{idx}" for idx in range(4)]].to_numpy(float)
        values.append(float(a_eff @ M_lr @ M_qz @ s_eff))
    return np.array(values, dtype=float)


def summarize_by_family(df: pd.DataFrame, residual_col: str) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for family, group in df.groupby("family"):
        residuals = group[residual_col].to_numpy(float)
        rows.append(
            {
                "family": family,
                "n_rows": int(len(group)),
                "rmse": float(np.sqrt(np.mean(residuals**2))),
                "mean_abs_residual": float(np.mean(np.abs(residuals))),
                "max_abs_residual": float(np.max(np.abs(residuals))),
            }
        )
    return rows


def matrix_to_nested_list(vec: np.ndarray) -> list[list[float]]:
    return matrix_from_vector(vec).round(10).tolist()


def compare_matrices(left_vec: np.ndarray, right_vec: np.ndarray) -> dict[str, object]:
    left = matrix_from_vector(left_vec)
    right = matrix_from_vector(right_vec)
    delta = left - right
    return {
        "frobenius_norm": float(np.linalg.norm(delta, ord="fro")),
        "max_abs_entry_diff": float(np.max(np.abs(delta))),
        "mean_abs_entry_diff": float(np.mean(np.abs(delta))),
        "delta_matrix": delta.round(10).tolist(),
    }


def write_report(
    out_path: Path,
    gain_summary: dict[str, object],
    qz_summary: dict[str, object],
    lr_summary: dict[str, object],
    comb_summary: dict[str, object],
    combined_summary: dict[str, object],
    air_summary: dict[str, object],
    qz_vec: np.ndarray,
    lr_vec: np.ndarray,
    comb_vec: np.ndarray,
    product_vec: np.ndarray,
    matrix_compare: dict[str, object],
) -> None:
    qz_matrix = matrix_from_vector(qz_vec)
    lr_matrix = matrix_from_vector(lr_vec)
    comb_matrix = matrix_from_vector(comb_vec)
    product_matrix = matrix_from_vector(product_vec)
    text = f"""# Linear Mueller Fit

- Signal model: dark-subtracted raw `{gain_summary["dark_mean"]:.3f}` baseline, then AIR-derived scalar gain correction.
- AIR gain rules:
  - `CPL-CPL` day 1: interpolate scalar gain between `AIR` and `AIR-BR`
  - `CPL-CPL` day 2: use `AIR-2`
  - `LP-LP`: mean of `AIR-2` and `AIR-2R`
  - `LP-CPL`: mean of `AIR-2` and `AIR-2R`
  - `CPL-LP`: split at first bullshit check, pre=`AIR-2`, post=`AIR-2R`

## QZ

- rows: `{qz_summary["n_rows"]}`
- rank: `{qz_summary["rank"]}` / `16`
- unique least-squares solution: `{qz_summary["unique_least_squares_solution"]}`
- condition number: `{qz_summary["condition_number"]:.3e}`
- fit RMSE: `{qz_summary["rmse"]:.6f}`

```text
{np.array2string(qz_matrix, precision=6, suppress_small=False)}
```

## LR

- rows: `{lr_summary["n_rows"]}`
- rank: `{lr_summary["rank"]}` / `16`
- unique least-squares solution: `{lr_summary["unique_least_squares_solution"]}`
- condition number: `{lr_summary["condition_number"]:.3e}`
- fit RMSE: `{lr_summary["rmse"]:.6f}`

```text
{np.array2string(lr_matrix, precision=6, suppress_small=False)}
```

## Combined Matrix

- rows: `{comb_summary["n_rows"]}`
- rank: `{comb_summary["rank"]}` / `16`
- unique least-squares solution: `{comb_summary["unique_least_squares_solution"]}`
- condition number: `{comb_summary["condition_number"]:.3e}`
- fit RMSE: `{comb_summary["rmse"]:.6f}`

```text
{np.array2string(comb_matrix, precision=6, suppress_small=False)}
```

## Product Matrix

```text
{np.array2string(product_matrix, precision=6, suppress_small=False)}
```

## Combined Vs Product

- Frobenius norm of matrix difference: `{matrix_compare["frobenius_norm"]:.6f}`
- mean abs entry difference: `{matrix_compare["mean_abs_entry_diff"]:.6f}`
- max abs entry difference: `{matrix_compare["max_abs_entry_diff"]:.6f}`

```text
{np.array2string(matrix_from_vector(comb_vec) - matrix_from_vector(product_vec), precision=6, suppress_small=False)}
```

## Combined Prediction Check

- rows: `{combined_summary["n_rows"]}`
- fitted `M_comb` prediction RMSE: `{combined_summary["fit_rmse"]:.6f}`
- product `M_LR M_QZ` prediction RMSE: `{combined_summary["product_rmse"]:.6f}`
- RMSE ratio `product / fit`: `{combined_summary["product_rmse"] / combined_summary["fit_rmse"]:.3f}`
- product prediction RMSE: `{combined_summary["rmse"]:.6f}`
- mean abs residual: `{combined_summary["mean_abs_residual"]:.6f}`
- max abs residual: `{combined_summary["max_abs_residual"]:.6f}`

## AIR Check

- rows: `{air_summary["n_rows"]}`
- corrected AIR-vs-identity RMSE: `{air_summary["rmse"]:.6f}`
- mean abs residual: `{air_summary["mean_abs_residual"]:.6f}`
"""
    out_path.write_text(text, encoding="utf-8")


def main() -> None:
    args = parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)

    all_rows = load_glue_tables(args.glue_dir)
    all_rows, gain_summary = assign_gain_model(all_rows, args.raw_data_dir, args.diagnostics_dir, args.value_col)

    qz_vec, qz_summary, qz_rows = solve_single_object(all_rows, "qz")
    lr_vec, lr_summary, lr_rows = solve_single_object(all_rows, "lr")
    comb_vec, comb_summary, comb_rows = solve_single_object(all_rows, "qz_lr_product")
    product_vec = np.reshape(matrix_from_vector(lr_vec) @ matrix_from_vector(qz_vec), 16, order="F")
    matrix_compare = compare_matrices(comb_vec, product_vec)

    qzlr_rows = all_rows[
        all_rows["effective_matrix_kind"].eq("qz_lr_product")
        & ~all_rows["exclude_from_fit"]
        & ~all_rows["experiment"].str.contains("(AIR#0)", regex=False)
    ].copy()
    qzlr_rows["prediction_fit_comb"] = predict_with_matrix(qzlr_rows, comb_vec)
    qzlr_rows["residual_fit_comb"] = qzlr_rows["signal_corrected"] - qzlr_rows["prediction_fit_comb"]
    qzlr_rows["prediction_product"] = predict_combined(qzlr_rows, qz_vec, lr_vec)
    qzlr_rows["residual_product"] = qzlr_rows["signal_corrected"] - qzlr_rows["prediction_product"]
    combined_summary = {
        "n_rows": int(len(qzlr_rows)),
        "fit_rmse": float(np.sqrt(np.mean(qzlr_rows["residual_fit_comb"].to_numpy(float) ** 2))),
        "product_rmse": float(np.sqrt(np.mean(qzlr_rows["residual_product"].to_numpy(float) ** 2))),
        "rmse": float(np.sqrt(np.mean(qzlr_rows["residual_product"].to_numpy(float) ** 2))),
        "mean_abs_residual": float(np.mean(np.abs(qzlr_rows["residual_product"].to_numpy(float)))),
        "max_abs_residual": float(np.max(np.abs(qzlr_rows["residual_product"].to_numpy(float)))),
        "fit_by_family": summarize_by_family(qzlr_rows, "residual_fit_comb"),
        "product_by_family": summarize_by_family(qzlr_rows, "residual_product"),
    }

    air_rows = all_rows[all_rows["effective_matrix_kind"].eq("air")].copy()
    air_summary = {
        "n_rows": int(len(air_rows)),
        "rmse": float(np.sqrt(np.mean(air_rows["air_identity_residual"].to_numpy(float) ** 2))),
        "mean_abs_residual": float(np.mean(np.abs(air_rows["air_identity_residual"].to_numpy(float)))),
        "max_abs_residual": float(np.max(np.abs(air_rows["air_identity_residual"].to_numpy(float)))),
        "by_family": summarize_by_family(air_rows, "air_identity_residual"),
    }

    all_rows["prediction_qz"] = np.nan
    all_rows["prediction_lr"] = np.nan
    all_rows["prediction_comb_fit"] = np.nan
    all_rows["prediction_product"] = np.nan
    all_rows["residual_model"] = np.nan

    qz_pred_all = predict_with_matrix(all_rows, qz_vec)
    lr_pred_all = predict_with_matrix(all_rows, lr_vec)
    qz_mask = all_rows["effective_matrix_kind"].eq("qz") & ~all_rows["experiment"].str.contains("(AIR#0)", regex=False)
    lr_mask = all_rows["effective_matrix_kind"].eq("lr") & ~all_rows["experiment"].str.contains("(AIR#0)", regex=False)
    qzlr_mask = all_rows["effective_matrix_kind"].eq("qz_lr_product") & ~all_rows["experiment"].str.contains("(AIR#0)", regex=False)

    all_rows.loc[qz_mask, "prediction_qz"] = qz_pred_all[qz_mask.to_numpy()]
    all_rows.loc[qz_mask, "residual_model"] = all_rows.loc[qz_mask, "signal_corrected"] - all_rows.loc[qz_mask, "prediction_qz"]
    all_rows.loc[lr_mask, "prediction_lr"] = lr_pred_all[lr_mask.to_numpy()]
    all_rows.loc[lr_mask, "residual_model"] = all_rows.loc[lr_mask, "signal_corrected"] - all_rows.loc[lr_mask, "prediction_lr"]
    comb_pred_all = predict_with_matrix(all_rows, comb_vec)
    all_rows.loc[qzlr_mask, "prediction_comb_fit"] = comb_pred_all[qzlr_mask.to_numpy()]
    product_pred_all = predict_combined(all_rows[qzlr_mask].copy(), qz_vec, lr_vec)
    all_rows.loc[qzlr_mask, "prediction_product"] = product_pred_all
    all_rows.loc[qzlr_mask, "residual_model"] = all_rows.loc[qzlr_mask, "signal_corrected"] - all_rows.loc[qzlr_mask, "prediction_product"]

    all_rows.to_csv(args.out_dir / "all_rows_with_fit.csv", index=False)

    summary = {
        "value_col": args.value_col,
        "gain_summary": gain_summary,
        "qz_matrix": matrix_to_nested_list(qz_vec),
        "lr_matrix": matrix_to_nested_list(lr_vec),
        "combined_matrix": matrix_to_nested_list(comb_vec),
        "product_matrix": matrix_to_nested_list(product_vec),
        "combined_vs_product_matrix": matrix_compare,
        "qz_fit": {**qz_summary, "by_family": summarize_by_family(qz_rows, "residual_single")},
        "lr_fit": {**lr_summary, "by_family": summarize_by_family(lr_rows, "residual_single")},
        "combined_fit": {**comb_summary, "by_family": summarize_by_family(comb_rows, "residual_single")},
        "combined_check": combined_summary,
        "air_check": air_summary,
        "files_written": [
            "all_rows_with_fit.csv",
            "report.md",
            "summary.json",
        ],
    }
    (args.out_dir / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    write_report(
        args.out_dir / "report.md",
        gain_summary,
        qz_summary,
        lr_summary,
        comb_summary,
        combined_summary,
        air_summary,
        qz_vec,
        lr_vec,
        comb_vec,
        product_vec,
        matrix_compare,
    )


if __name__ == "__main__":
    main()

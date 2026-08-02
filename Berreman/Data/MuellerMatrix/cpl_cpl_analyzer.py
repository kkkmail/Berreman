from __future__ import annotations

import argparse
import json
import math
import re
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


EXPERIMENT_RE = re.compile(r"^(?P<src>[^-]+)-\((?P<inside>[^)]*)\)-(?P<an>[^-]+)(?:-(?P<note>.*))?$")
OBJECT_RE = re.compile(r"(?P<obj>[A-Z]+)#(?P<angle>-?\d+)")
LEGACY_EXTRA_ARTIFACTS = [
    "analysis_report.md",
    "lr_harmonics_normalized.png",
    "lr_orientation_normalized.png",
    "normalized_fit_sparse_best_bic.csv",
    "quartz_harmonics_normalized.png",
    "quartz_orientation_normalized.png",
    "raw_fit_sparse_best_bic.csv",
]


def parse_experiment_name(name: str) -> dict[str, object]:
    match = EXPERIMENT_RE.match(name)
    if not match:
        return {
            "source": None,
            "analyzer": None,
            "note": "",
            "n_objects": 0,
            "obj1": None,
            "angle1_label": np.nan,
            "obj2": None,
            "angle2_label": np.nan,
        }

    inside = match.group("inside") or ""
    parts = inside.split("-") if inside else []
    objects: list[tuple[str, int]] = []
    for part in parts:
        object_match = OBJECT_RE.match(part)
        if object_match:
            objects.append((object_match.group("obj"), int(object_match.group("angle"))))

    parsed: dict[str, object] = {
        "source": match.group("src"),
        "analyzer": match.group("an"),
        "note": match.group("note") or "",
        "n_objects": len(objects),
        "obj1": None,
        "angle1_label": np.nan,
        "obj2": None,
        "angle2_label": np.nan,
    }
    if len(objects) >= 1:
        parsed["obj1"], parsed["angle1_label"] = objects[0]
    if len(objects) >= 2:
        parsed["obj2"], parsed["angle2_label"] = objects[1]
    return parsed


def design_matrix(theta_deg: np.ndarray, harmonics: list[int]) -> tuple[np.ndarray, list[str]]:
    theta = np.deg2rad(theta_deg)
    columns = [np.ones_like(theta)]
    names = ["const"]
    for harmonic in harmonics:
        columns.append(np.sin(harmonic * theta))
        columns.append(np.cos(harmonic * theta))
        names.extend([f"sin{harmonic}", f"cos{harmonic}"])
    return np.column_stack(columns), names


def fit_harmonic_subset(theta_deg: np.ndarray, values: np.ndarray, harmonics: list[int]) -> dict[str, object]:
    design, names = design_matrix(theta_deg, harmonics)
    beta, *_ = np.linalg.lstsq(design, values, rcond=None)
    fitted = design @ beta
    residuals = values - fitted
    sse = float(np.sum(residuals**2))
    sst = float(np.sum((values - values.mean()) ** 2))
    n_rows = len(values)
    n_params = len(beta)
    if sst > 0:
        r_squared = 1.0 - sse / sst
    else:
        r_squared = 1.0
    if sse <= 0:
        aic = -np.inf
        bic = -np.inf
    else:
        aic = n_rows * np.log(sse / n_rows) + 2 * n_params
        bic = n_rows * np.log(sse / n_rows) + n_params * np.log(n_rows)

    return {
        "harmonics": list(harmonics),
        "names": names,
        "beta": beta,
        "y_hat": fitted,
        "resid": residuals,
        "sse": sse,
        "sst": sst,
        "r2": r_squared,
        "aic": aic,
        "bic": bic,
        "n": n_rows,
        "p": n_params,
        "rmse": math.sqrt(sse / n_rows) if n_rows > 0 else np.nan,
    }


def dense_curve(fit: dict[str, object], x_dense: np.ndarray) -> np.ndarray:
    design, _ = design_matrix(x_dense, fit["harmonics"])
    return design @ fit["beta"]


def full_fourier4(theta_deg: np.ndarray, values: np.ndarray) -> dict[str, object]:
    return fit_harmonic_subset(theta_deg, values, [1, 2, 3, 4])


def harmonic_summary(theta_deg: np.ndarray, values: np.ndarray, max_harmonic: int = 4) -> pd.DataFrame:
    rows: list[dict[str, float | int]] = []
    for harmonic in range(1, max_harmonic + 1):
        fit = fit_harmonic_subset(theta_deg, values, [harmonic])
        sin_coeff = float(fit["beta"][1])
        cos_coeff = float(fit["beta"][2])
        amplitude = math.hypot(sin_coeff, cos_coeff)
        phase_deg = math.degrees(math.atan2(sin_coeff, cos_coeff))
        rows.append(
            {
                "k": harmonic,
                "sin_coeff": sin_coeff,
                "cos_coeff": cos_coeff,
                "amplitude": amplitude,
                "phase_deg_from_cos_form": phase_deg,
                "single_harmonic_r2": fit["r2"],
            }
        )
    return pd.DataFrame(rows)


def baseline_interp_for_experiment(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.Series]:
    air_start_name = "CPL-(AIR#0)-CPL"
    air_end_name = "CPL-(AIR#0)-CPL-BR"

    air_start = (
        df[df["experiment"] == air_start_name]
        .groupby("description", as_index=False)["avg_total"]
        .mean()
        .rename(columns={"avg_total": "air_start"})
    )
    air_end = (
        df[df["experiment"] == air_end_name]
        .groupby("description", as_index=False)["avg_total"]
        .mean()
        .rename(columns={"avg_total": "air_end"})
    )
    air = air_start.merge(air_end, on="description", how="outer").sort_values("description")

    t0 = df[df["experiment"] == air_start_name]["captured_at"].mean()
    t1 = df[df["experiment"] == air_end_name]["captured_at"].mean()
    experiment_mid = df.groupby("experiment")["captured_at"].mean()
    weight = ((experiment_mid - t0) / (t1 - t0)).clip(lower=0, upper=1)
    return air, weight


def add_normalization(df: pd.DataFrame, air: pd.DataFrame, weight: pd.Series) -> pd.DataFrame:
    air_lookup = air.set_index("description").to_dict("index")
    air_start_lookup = air.set_index("description")["air_start"]
    air_end_lookup = air.set_index("description")["air_end"]

    def interp_baseline(row: pd.Series) -> float:
        item = air_lookup[int(row["description"])]
        blend = float(weight.loc[row["experiment"]])
        return (1.0 - blend) * item["air_start"] + blend * item["air_end"]

    out = df.copy()
    out["air_interp_baseline"] = out.apply(interp_baseline, axis=1)
    out["norm_interp_air"] = out["avg_total"] / out["air_interp_baseline"]
    out["norm_start_air"] = out.apply(
        lambda row: row["avg_total"] / air_start_lookup.loc[int(row["description"])], axis=1
    )
    out["norm_end_air"] = out.apply(
        lambda row: row["avg_total"] / air_end_lookup.loc[int(row["description"])], axis=1
    )
    return out


def unique_angle_trace(df: pd.DataFrame, experiment: str, value_col: str) -> pd.DataFrame:
    return (
        df[df["experiment"] == experiment]
        .groupby("description", as_index=False)[value_col]
        .mean()
        .sort_values("description")
        .reset_index(drop=True)
    )


def save_fit_tables(df: pd.DataFrame, value_col: str, prefix: str, out_dir: Path) -> tuple[pd.DataFrame, pd.DataFrame]:
    full_rows: list[dict[str, object]] = []
    harmonic_frames: list[pd.DataFrame] = []

    for experiment in sorted(df["experiment"].unique()):
        trace = unique_angle_trace(df, experiment, value_col)
        theta = trace["description"].to_numpy()
        values = trace[value_col].to_numpy()

        full_fit = full_fourier4(theta, values)
        full_row: dict[str, object] = {
            "experiment": experiment,
            "value_col": value_col,
            "fit_kind": "fourier4_full",
            "r2": full_fit["r2"],
            "rmse": full_fit["rmse"],
            "aic": full_fit["aic"],
            "bic": full_fit["bic"],
        }
        for coeff_name, coeff_value in zip(full_fit["names"], full_fit["beta"]):
            full_row[coeff_name] = float(coeff_value)
        full_rows.append(full_row)

        summary = harmonic_summary(theta, values)
        summary.insert(0, "value_col", value_col)
        summary.insert(0, "experiment", experiment)
        harmonic_frames.append(summary)

    full_df = pd.DataFrame(full_rows)
    harmonic_df = pd.concat(harmonic_frames, ignore_index=True)

    full_df.to_csv(out_dir / f"{prefix}_fit_fourier4.csv", index=False)
    harmonic_df.to_csv(out_dir / f"{prefix}_harmonic_summary.csv", index=False)
    return full_df, harmonic_df


def make_air_drift_plot(df: pd.DataFrame, out_dir: Path, title_suffix: str) -> dict[str, float]:
    air_start = unique_angle_trace(df, "CPL-(AIR#0)-CPL", "avg_total")
    air_end = unique_angle_trace(df, "CPL-(AIR#0)-CPL-BR", "avg_total")
    merged = air_start.merge(air_end, on="description", suffixes=("_start", "_end"))
    merged["ratio_end_over_start"] = merged["avg_total_end"] / merged["avg_total_start"]

    fig, axes = plt.subplots(1, 2, figsize=(12, 4), constrained_layout=True)

    axes[0].plot(merged["description"], merged["avg_total_start"], marker="o", label="AIR start")
    axes[0].plot(merged["description"], merged["avg_total_end"], marker="o", label="AIR end / BR")
    axes[0].set_xlabel("Analyzer angle (deg)")
    axes[0].set_ylabel("avg_total")
    axes[0].set_title(f"Air sweeps{title_suffix}")
    axes[0].legend()

    axes[1].plot(merged["description"], merged["ratio_end_over_start"], marker="o")
    axes[1].axhline(
        merged["ratio_end_over_start"].mean(),
        linestyle="--",
        label=f"mean={merged['ratio_end_over_start'].mean():.4f}",
    )
    axes[1].set_xlabel("Analyzer angle (deg)")
    axes[1].set_ylabel("end/start ratio")
    axes[1].set_title(f"Drift ratio by angle{title_suffix}")
    axes[1].legend()

    fig.savefig(out_dir / "air_drift.png", dpi=180)
    plt.close(fig)

    shape_start = merged["avg_total_start"] / merged["avg_total_start"].mean()
    shape_end = merged["avg_total_end"] / merged["avg_total_end"].mean()
    shape_diff = shape_end - shape_start

    return {
        "mean_ratio": float(merged["ratio_end_over_start"].mean()),
        "median_ratio": float(merged["ratio_end_over_start"].median()),
        "min_ratio": float(merged["ratio_end_over_start"].min()),
        "max_ratio": float(merged["ratio_end_over_start"].max()),
        "shape_rms_abs_diff": float(np.sqrt(np.mean(shape_diff**2))),
        "shape_max_abs_diff": float(np.max(np.abs(shape_diff))),
    }


def grid_plot_with_fit(df: pd.DataFrame, value_col: str, title: str, file_name: str, out_dir: Path) -> None:
    experiments = sorted(df["experiment"].unique())
    ncols = 3
    nrows = math.ceil(len(experiments) / ncols)
    fig, axes = plt.subplots(nrows, ncols, figsize=(14, 4 * nrows), constrained_layout=True)
    axes_array = np.array(axes).reshape(-1)
    x_dense = np.linspace(0, 360, 721)

    for axis, experiment in zip(axes_array, experiments):
        trace = unique_angle_trace(df, experiment, value_col)
        theta = trace["description"].to_numpy()
        values = trace[value_col].to_numpy()
        fit = full_fourier4(theta, values)
        axis.plot(theta, values, "o", label="data")
        axis.plot(x_dense, dense_curve(fit, x_dense), "-", label=f"4-harm fit, R^2={fit['r2']:.3f}")
        axis.set_title(experiment)
        axis.set_xlabel("Analyzer angle (deg)")
        axis.set_ylabel(value_col)
        axis.legend(fontsize=8)

    for axis in axes_array[len(experiments) :]:
        axis.axis("off")

    fig.suptitle(title, fontsize=14)
    fig.savefig(out_dir / file_name, dpi=180)
    plt.close(fig)


def prune_legacy_outputs(out_dir: Path) -> None:
    for name in LEGACY_EXTRA_ARTIFACTS:
        path = out_dir / name
        if path.exists():
            path.unlink()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Analyze CPL-CPL CSV sweeps and plot Fourier-fit summaries.")
    parser.add_argument("--csv", required=True, type=Path, help="Input CSV file.")
    parser.add_argument("--out-dir", required=True, type=Path, help="Directory for plots and derived tables.")
    parser.add_argument("--title-suffix", default="", help="Optional suffix appended to plot titles.")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)
    prune_legacy_outputs(args.out_dir)

    df = pd.read_csv(args.csv)
    parsed = df["experiment"].apply(parse_experiment_name).apply(pd.Series)
    df = pd.concat([df, parsed], axis=1)
    df["captured_at"] = pd.to_datetime(df["captured_at"])

    air, weight = baseline_interp_for_experiment(df)
    df = add_normalization(df, air, weight)

    df.to_csv(args.out_dir / "processed_measurements.csv", index=False)
    air.to_csv(args.out_dir / "air_baseline_table.csv", index=False)

    save_fit_tables(df, "avg_total", "raw", args.out_dir)
    save_fit_tables(df, "norm_interp_air", "normalized", args.out_dir)

    air_stats = make_air_drift_plot(df, args.out_dir, args.title_suffix)
    grid_plot_with_fit(
        df,
        "avg_total",
        f"Raw avg_total sweeps with 4-harmonic Fourier fits{args.title_suffix}",
        "raw_sweeps_fourier4.png",
        args.out_dir,
    )
    grid_plot_with_fit(
        df,
        "norm_interp_air",
        f"Normalized sweeps with 4-harmonic Fourier fits{args.title_suffix}",
        "normalized_sweeps_fourier4.png",
        args.out_dir,
    )
    summary = {
        "air_stats": air_stats,
        "files_written": sorted(path.name for path in args.out_dir.iterdir()),
    }
    (args.out_dir / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")


if __name__ == "__main__":
    main()

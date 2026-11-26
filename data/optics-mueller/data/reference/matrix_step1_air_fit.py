from __future__ import annotations

import argparse
import json
import math
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Step 1 AIR fit for effective CPL source/analyzer behavior.")
    parser.add_argument("--data-dir", required=True, type=Path, help="Directory with final exported CSVs.")
    parser.add_argument("--out-dir", required=True, type=Path, help="Directory for step-1 outputs.")
    return parser.parse_args()


def angle_mod_180(angle_deg: float) -> float:
    return float(angle_deg % 180.0)


def wrap_180(angle_deg: float) -> float:
    return float(((angle_deg + 90.0) % 180.0) - 90.0)


def retardance_from_visibility(visibility: float) -> float:
    return float(np.rad2deg(np.arccos(np.clip(abs(visibility), 0.0, 1.0))))


def rotation_matrix(angle_deg: float) -> np.ndarray:
    theta = np.deg2rad(2.0 * angle_deg)
    c = np.cos(theta)
    s = np.sin(theta)
    return np.array(
        [
            [1.0, 0.0, 0.0, 0.0],
            [0.0, c, s, 0.0],
            [0.0, -s, c, 0.0],
            [0.0, 0.0, 0.0, 1.0],
        ],
        dtype=float,
    )


def linear_polarizer(angle_deg: float) -> np.ndarray:
    base = 0.5 * np.array(
        [
            [1.0, 1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0, 0.0],
            [0.0, 0.0, 0.0, 0.0],
            [0.0, 0.0, 0.0, 0.0],
        ],
        dtype=float,
    )
    return rotation_matrix(-angle_deg) @ base @ rotation_matrix(angle_deg)


def retarder(angle_deg: float, retardance_deg: float) -> np.ndarray:
    delta = np.deg2rad(retardance_deg)
    base = np.array(
        [
            [1.0, 0.0, 0.0, 0.0],
            [0.0, 1.0, 0.0, 0.0],
            [0.0, 0.0, np.cos(delta), np.sin(delta)],
            [0.0, 0.0, -np.sin(delta), np.cos(delta)],
        ],
        dtype=float,
    )
    return rotation_matrix(-angle_deg) @ base @ rotation_matrix(angle_deg)


def load_air_traces(data_dir: Path, csv_name: str) -> pd.DataFrame:
    df = pd.read_csv(data_dir / csv_name)
    air = df[df["experiment"].str.contains("(AIR#0)", regex=False)].copy()
    air["description"] = pd.to_numeric(air["description"], errors="coerce")
    return air


def prepare_normalized_traces(df: pd.DataFrame, dark_mean: float) -> pd.DataFrame:
    rows: list[pd.DataFrame] = []
    for experiment, group in df.groupby("experiment"):
        trace = (
            group.groupby("description", as_index=False)["avg_total"]
            .mean()
            .sort_values("description")
            .reset_index(drop=True)
        )
        trace["signal"] = trace["avg_total"] - dark_mean
        trace["norm"] = trace["signal"] / trace["signal"].mean()
        trace["experiment"] = experiment
        rows.append(trace[["experiment", "description", "signal", "norm"]])
    return pd.concat(rows, ignore_index=True)


def fit_free_cosine(df: pd.DataFrame) -> dict[str, float]:
    alpha = np.deg2rad(2.0 * df["description"].to_numpy(float))
    y = df["norm"].to_numpy(float) - 1.0
    design = np.column_stack([np.cos(alpha), np.sin(alpha)])
    coeffs, *_ = np.linalg.lstsq(design, y, rcond=None)
    fitted = 1.0 + design @ coeffs
    resid = df["norm"].to_numpy(float) - fitted
    visibility = float(np.hypot(coeffs[0], coeffs[1]))
    zero_angle = angle_mod_180(0.5 * np.rad2deg(np.arctan2(coeffs[1], coeffs[0])))
    return {
        "p_cos2": float(coeffs[0]),
        "q_sin2": float(coeffs[1]),
        "visibility": visibility,
        "zero_angle_deg": zero_angle,
        "rmse_norm": float(np.sqrt(np.mean(resid**2))),
    }


def fit_fixed_phase(df: pd.DataFrame, zero_angle_deg: float) -> dict[str, float]:
    alpha = np.deg2rad(2.0 * (df["description"].to_numpy(float) - zero_angle_deg))
    basis = np.cos(alpha)
    y = df["norm"].to_numpy(float) - 1.0
    visibility = float(np.dot(y, basis) / np.dot(basis, basis))
    fitted = 1.0 + visibility * basis
    resid = df["norm"].to_numpy(float) - fitted
    return {
        "visibility": visibility,
        "rmse_norm": float(np.sqrt(np.mean(resid**2))),
    }


def fit_lp_frame_coeffs(df: pd.DataFrame, zero_angle_deg: float) -> dict[str, float]:
    alpha = np.deg2rad(2.0 * (df["description"].to_numpy(float) - zero_angle_deg))
    y = df["norm"].to_numpy(float) - 1.0
    design = np.column_stack([np.cos(alpha), np.sin(alpha)])
    coeffs, *_ = np.linalg.lstsq(design, y, rcond=None)
    fitted = 1.0 + design @ coeffs
    resid = df["norm"].to_numpy(float) - fitted
    return {
        "q_lp": float(coeffs[0]),
        "u_lp": float(coeffs[1]),
        "visibility": float(np.hypot(coeffs[0], coeffs[1])),
        "rmse_norm": float(np.sqrt(np.mean(resid**2))),
    }


def derive_source_cpl_from_lp_frame(q_lp: float, u_lp: float) -> dict[str, float]:
    two_theta = np.arctan2(1.0 - q_lp, u_lp)
    theta_rel_deg = angle_mod_180(0.5 * np.rad2deg(two_theta))
    sin_two_theta = np.sin(two_theta)
    if abs(sin_two_theta) < 1e-9:
        raise ValueError("Source retarder angle is numerically ill-conditioned.")
    one_minus_cos_delta = (1.0 - q_lp) / (sin_two_theta * sin_two_theta)
    cos_delta = 1.0 - one_minus_cos_delta
    retardance_deg = float(np.rad2deg(np.arccos(np.clip(cos_delta, -1.0, 1.0))))
    return {
        "theta_rel_deg": theta_rel_deg,
        "retardance_deg": retardance_deg,
    }


def predict_normalized_cosine(alpha_deg: np.ndarray, visibility: float, zero_angle_deg: float) -> np.ndarray:
    return 1.0 + visibility * np.cos(np.deg2rad(2.0 * (alpha_deg - zero_angle_deg)))


def source_state_cpl(source_theta_rel_deg: float, retardance_deg: float) -> np.ndarray:
    s_lin = np.array([1.0, 1.0, 0.0, 0.0], dtype=float)
    return retarder(source_theta_rel_deg, retardance_deg) @ s_lin


def predict_cpl_lp_source(alpha_deg: np.ndarray, analyzer_zero_deg: float, source_theta_rel_deg: float, source_delta_deg: float) -> np.ndarray:
    s_lin = np.array([1.0, 1.0, 0.0, 0.0], dtype=float)
    s_src = retarder(source_theta_rel_deg, source_delta_deg) @ s_lin
    out: list[float] = []
    for alpha in alpha_deg:
        beta = alpha - analyzer_zero_deg
        out.append(float((linear_polarizer(beta) @ s_src)[0]))
    values = np.array(out, dtype=float)
    return values / values.mean()


def predict_cpl_cpl_trace(
    alpha_deg: np.ndarray,
    analyzer_zero_deg: float,
    source_theta_rel_deg: float,
    source_delta_deg: float,
    analyzer_delta_deg: float,
) -> np.ndarray:
    s_src = source_state_cpl(source_theta_rel_deg, source_delta_deg)
    out: list[float] = []
    for alpha in alpha_deg:
        beta = alpha - analyzer_zero_deg
        analyzer = linear_polarizer(beta) @ retarder(beta + 45.0, analyzer_delta_deg)
        out.append(float((analyzer @ s_src)[0]))
    values = np.array(out, dtype=float)
    return values / values.mean()


def repeat_spread(values: list[float]) -> float:
    if not values:
        return 0.0
    return float(max(values) - min(values))


def build_overview_plot(
    out_path: Path,
    lp_lp: pd.DataFrame,
    cpl_lp: pd.DataFrame,
    lp_cpl: pd.DataFrame,
    cpl_cpl: pd.DataFrame,
    cpl_cpl_trace: pd.DataFrame,
    lp_fit: dict[str, float],
    cpl_lp_strict: dict[str, float],
    cpl_lp_source_relaxed: dict[str, float],
    lp_cpl_fit: dict[str, float],
    cpl_cpl_best_curve: np.ndarray,
) -> None:
    fig, axes = plt.subplots(2, 2, figsize=(13, 9), constrained_layout=True)
    alpha_dense = np.linspace(0.0, 360.0, 721)

    panels = [
        (
            axes[0, 0],
            lp_lp,
            "LP-LP AIR",
            [
                ("fit", predict_normalized_cosine(alpha_dense, lp_fit["visibility"], lp_fit["zero_angle_deg"]), "#1f4b99", "-"),
            ],
        ),
        (
            axes[0, 1],
            cpl_lp,
            "CPL-LP AIR",
            [
                ("strict 45 model", predict_normalized_cosine(alpha_dense, cpl_lp_strict["visibility"], lp_fit["zero_angle_deg"]), "#b84a00", "-"),
                (
                    "source theta free",
                    predict_cpl_lp_source(
                        alpha_dense,
                        lp_fit["zero_angle_deg"],
                        cpl_lp_source_relaxed["theta_rel_deg"],
                        cpl_lp_source_relaxed["retardance_deg"],
                    ),
                    "#7f2f8a",
                    "--",
                ),
            ],
        ),
        (
            axes[1, 0],
            lp_cpl,
            "LP-CPL AIR",
            [
                ("fit", predict_normalized_cosine(alpha_dense, lp_cpl_fit["visibility"], lp_cpl_fit["zero_angle_deg"]), "#1b7f5a", "-"),
            ],
        ),
    ]

    for axis, frame, title, curves in panels:
        for experiment, group in frame.groupby("experiment"):
            axis.plot(group["description"], group["norm"], "o", ms=4, label=experiment)
        for label, curve, color, style in curves:
            axis.plot(alpha_dense, curve, style, color=color, lw=1.5, label=label)
        axis.set_title(title)
        axis.set_xlabel("Analyzer dial angle (deg)")
        axis.set_ylabel("Normalized AIR signal")
        axis.grid(alpha=0.25)
        axis.legend(fontsize=8)

    axis = axes[1, 1]
    for experiment, group in cpl_cpl.groupby("experiment"):
        axis.plot(group["description"], group["norm"], "o", ms=4, label=experiment)
    axis.plot(cpl_cpl_trace["description"].to_numpy(float), cpl_cpl_best_curve, "-", color="#b84a00", lw=1.5, label="best CPL-CPL check")
    axis.set_title("CPL-CPL AIR check")
    axis.set_xlabel("Analyzer dial angle (deg)")
    axis.set_ylabel("Normalized AIR signal")
    axis.grid(alpha=0.25)
    axis.legend(fontsize=8)

    fig.savefig(out_path, dpi=180)
    plt.close(fig)


def main() -> None:
    args = parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)

    dark_df = pd.read_csv(args.data_dir / "darkness_checks.csv")
    dark_mean = float(dark_df["avg_total"].mean())

    lp_lp = prepare_normalized_traces(load_air_traces(args.data_dir, "lp_lp.csv"), dark_mean)
    cpl_lp = prepare_normalized_traces(load_air_traces(args.data_dir, "cpl_lp.csv"), dark_mean)
    lp_cpl = prepare_normalized_traces(load_air_traces(args.data_dir, "lp_cpl.csv"), dark_mean)
    cpl_cpl = prepare_normalized_traces(load_air_traces(args.data_dir, "cpl_cpl_day2_corrections.csv"), dark_mean)

    lp_lp_fit = fit_free_cosine(lp_lp)
    cpl_lp_free = fit_free_cosine(cpl_lp)
    cpl_lp_strict = fit_fixed_phase(cpl_lp, lp_lp_fit["zero_angle_deg"])
    cpl_lp_lp_frame = fit_lp_frame_coeffs(cpl_lp, lp_lp_fit["zero_angle_deg"])
    cpl_lp_source_relaxed = {
        **cpl_lp_lp_frame,
        **derive_source_cpl_from_lp_frame(cpl_lp_lp_frame["q_lp"], cpl_lp_lp_frame["u_lp"]),
    }
    lp_cpl_fit = fit_free_cosine(lp_cpl)

    lp_lp_repeat_fits = [fit_free_cosine(frame) for _, frame in lp_lp.groupby("experiment")]
    cpl_lp_repeat_fits = [fit_free_cosine(frame) for _, frame in cpl_lp.groupby("experiment")]
    lp_cpl_repeat_fits = [fit_free_cosine(frame) for _, frame in lp_cpl.groupby("experiment")]

    phase_shift_source = wrap_180(cpl_lp_free["zero_angle_deg"] - lp_lp_fit["zero_angle_deg"])
    source_delta_raw = retardance_from_visibility(cpl_lp_strict["visibility"])
    analyzer_delta_raw = retardance_from_visibility(lp_cpl_fit["visibility"])
    source_delta_lp_ref = retardance_from_visibility(cpl_lp_strict["visibility"] / lp_lp_fit["visibility"])
    analyzer_delta_lp_ref = retardance_from_visibility(lp_cpl_fit["visibility"] / lp_lp_fit["visibility"])

    cpl_cpl_trace = cpl_cpl.groupby("description", as_index=False)["norm"].mean().sort_values("description").reset_index(drop=True)
    cpl_cpl_best: dict[str, float | str | list[float]] | None = None
    for source_sign in (+1.0, -1.0):
        for analyzer_sign in (+1.0, -1.0):
            predicted = predict_cpl_cpl_trace(
                cpl_cpl_trace["description"].to_numpy(float),
                lp_cpl_fit["zero_angle_deg"],
                cpl_lp_source_relaxed["theta_rel_deg"],
                source_sign * cpl_lp_source_relaxed["retardance_deg"],
                analyzer_sign * analyzer_delta_raw,
            )
            rmse = float(np.sqrt(np.mean((cpl_cpl_trace["norm"].to_numpy(float) - predicted) ** 2)))
            record: dict[str, float | str | list[float]] = {
                "source_theta_rel_deg": float(cpl_lp_source_relaxed["theta_rel_deg"]),
                "source_delta_deg": float(source_sign * cpl_lp_source_relaxed["retardance_deg"]),
                "analyzer_delta_deg": float(analyzer_sign * analyzer_delta_raw),
                "relative_handedness": "same" if source_sign == analyzer_sign else "opposite",
                "rmse_norm": rmse,
                "predicted_range": float(predicted.max() - predicted.min()),
                "curve": predicted.tolist(),
            }
            if cpl_cpl_best is None or rmse < float(cpl_cpl_best["rmse_norm"]):
                cpl_cpl_best = record

    assert cpl_cpl_best is not None

    build_overview_plot(
        args.out_dir / "air_step1_overview.png",
        lp_lp,
        cpl_lp,
        lp_cpl,
        cpl_cpl,
        cpl_cpl_trace,
        lp_lp_fit,
        cpl_lp_strict,
        cpl_lp_source_relaxed,
        lp_cpl_fit,
        np.array(cpl_cpl_best["curve"], dtype=float),
    )

    summary = {
        "dark_mean_avg_total": dark_mean,
        "lp_lp_anchor": {
            **lp_lp_fit,
            "repeat_zero_angle_deg": [fit["zero_angle_deg"] for fit in lp_lp_repeat_fits],
            "repeat_visibility": [fit["visibility"] for fit in lp_lp_repeat_fits],
            "repeat_zero_spread_deg": repeat_spread([fit["zero_angle_deg"] for fit in lp_lp_repeat_fits]),
        },
        "cpl_source_from_cpl_lp": {
            "strict_45_model": {
                **cpl_lp_strict,
                "lp_zero_angle_deg": lp_lp_fit["zero_angle_deg"],
                "retardance_raw_deg": source_delta_raw,
                "retardance_lp_reference_deg": source_delta_lp_ref,
            },
            "source_theta_relaxed_model": {
                **cpl_lp_source_relaxed,
                "repeat_zero_angle_deg": [fit["zero_angle_deg"] for fit in cpl_lp_repeat_fits],
                "repeat_visibility": [fit["visibility"] for fit in cpl_lp_repeat_fits],
                "repeat_zero_spread_deg": repeat_spread([fit["zero_angle_deg"] for fit in cpl_lp_repeat_fits]),
                "phase_shift_vs_lp_anchor_deg": phase_shift_source,
            },
        },
        "cpl_analyzer_from_lp_cpl": {
            **lp_cpl_fit,
            "retardance_raw_deg": analyzer_delta_raw,
            "retardance_lp_reference_deg": analyzer_delta_lp_ref,
            "repeat_zero_angle_deg": [fit["zero_angle_deg"] for fit in lp_cpl_repeat_fits],
            "repeat_visibility": [fit["visibility"] for fit in lp_cpl_repeat_fits],
            "repeat_zero_spread_deg": repeat_spread([fit["zero_angle_deg"] for fit in lp_cpl_repeat_fits]),
        },
        "cpl_cpl_check": {
            "best_fit": {key: value for key, value in cpl_cpl_best.items() if key != "curve"},
            "relative_sign_only": True,
            "note": "Flipping both retardance signs together gives the same CPL-CPL prediction; only the relative sign is constrained.",
        },
        "files_written": [
            "air_step1_overview.png",
            "report.md",
            "summary.json",
        ],
    }

    (args.out_dir / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")

    report = f"""# Step 1 AIR Fit

- LP analyzer anchor from `LP-LP` AIR: `{lp_lp_fit["zero_angle_deg"]:.2f} deg` raw dial, repeat spread `{summary["lp_lp_anchor"]["repeat_zero_spread_deg"]:.2f} deg`
- CPL source from `CPL-LP` AIR, strict `45 deg` model:
  - retardance `{source_delta_raw:.2f} deg` raw, `{source_delta_lp_ref:.2f} deg` after LP-LP visibility reference
  - strict-fit RMSE `{cpl_lp_strict["rmse_norm"]:.4f}`
  - free-phase AIR check wants `{cpl_lp_free["zero_angle_deg"]:.2f} deg`, shifted `{phase_shift_source:.2f} deg` from the LP anchor
- CPL source from `CPL-LP` AIR, relaxed source-angle model:
  - source retarder angle `{cpl_lp_source_relaxed["theta_rel_deg"]:.2f} deg` relative to the source LP
  - source retardance `{cpl_lp_source_relaxed["retardance_deg"]:.2f} deg`
  - relaxed-fit RMSE `{cpl_lp_source_relaxed["rmse_norm"]:.4f}`
- CPL analyzer from `LP-CPL` AIR:
  - zero angle `{lp_cpl_fit["zero_angle_deg"]:.2f} deg` raw dial, repeat spread `{summary["cpl_analyzer_from_lp_cpl"]["repeat_zero_spread_deg"]:.2f} deg`
  - retardance `{analyzer_delta_raw:.2f} deg` raw, `{analyzer_delta_lp_ref:.2f} deg` after LP-LP visibility reference
  - fit RMSE `{lp_cpl_fit["rmse_norm"]:.4f}`
- CPL-CPL AIR check:
  - best relative handedness: `{cpl_cpl_best["relative_handedness"]}`
  - best RMSE `{float(cpl_cpl_best["rmse_norm"]):.4f}`
  - same-sign solutions are much worse
"""
    (args.out_dir / "report.md").write_text(report, encoding="utf-8")


if __name__ == "__main__":
    main()

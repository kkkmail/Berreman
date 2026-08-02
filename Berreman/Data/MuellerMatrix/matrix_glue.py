from __future__ import annotations

import argparse
import json
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

from cpl_cpl_analyzer import parse_experiment_name


SCIENCE_FILES = [
    "cpl_cpl_day1_main.csv",
    "cpl_cpl_day2_corrections.csv",
    "cpl_lp.csv",
    "lp_cpl.csv",
    "lp_lp.csv",
]

EXCLUDED_POINTS = [
    {
        "experiment": "LP-(LR#90)-CPL-2",
        "description": 140.0,
        "capture_index": 17,
        "reason": "Likely contaminated frame; keep visible in plots but exclude from harmonic fits.",
    }
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build matrix-solver glue rows from raw exported CSVs.")
    parser.add_argument("--data-dir", required=True, type=Path, help="Directory with raw final-session CSV exports.")
    parser.add_argument("--step1-dir", required=True, type=Path, help="Directory with step-1 AIR fit outputs.")
    parser.add_argument("--out-dir", required=True, type=Path, help="Directory for derived glue CSVs.")
    return parser.parse_args()


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


def canonical_polarizer(name: str | None) -> str | None:
    if name is None:
        return None
    if "CPL" in name:
        return "CPL"
    if "LP" in name:
        return "LP"
    return name


def family_from_experiment(name: str) -> str:
    parsed = parse_experiment_name(name)
    return f"{canonical_polarizer(parsed['source'])}-{canonical_polarizer(parsed['analyzer'])}"


def is_air_experiment(name: str) -> bool:
    return "(AIR#0)" in name


def object_rotation_from_parsed(parsed: dict[str, object]) -> tuple[float, str]:
    n_objects = int(parsed["n_objects"])
    if n_objects == 0:
        return 0.0, "air"

    angle1 = float(parsed["angle1_label"])
    if angle1 not in (0.0, 45.0, 90.0):
        raise ValueError(f"Unsupported object angle label: {angle1}")

    if n_objects == 1:
        obj1 = str(parsed["obj1"]).lower()
        return angle1, obj1

    angle2 = float(parsed["angle2_label"])
    if angle2 != angle1:
        raise ValueError(f"Combined object row uses mismatched rotations: {angle1} vs {angle2}")
    return angle1, "qz_lr_product"


def vector_column_major_names(prefix: str) -> list[str]:
    names: list[str] = []
    for col in range(4):
        for row in range(4):
            names.append(f"{prefix}_m{row}{col}")
    return names


@dataclass(frozen=True)
class SourceModel:
    name: str
    theta_rel_deg: float | None = None
    retardance_deg: float | None = None

    def base_state(self) -> np.ndarray:
        s_lin = np.array([1.0, 1.0, 0.0, 0.0], dtype=float)
        if self.name == "LP":
            return s_lin
        if self.name == "CPL":
            assert self.theta_rel_deg is not None and self.retardance_deg is not None
            return retarder(self.theta_rel_deg, self.retardance_deg) @ s_lin
        raise ValueError(f"Unsupported source model: {self.name}")


@dataclass(frozen=True)
class AnalyzerModel:
    name: str
    zero_dial_deg: float
    dial_sign: int
    theta_rel_deg: float | None = None
    retardance_deg: float | None = None

    def physical_angle_deg(self, raw_description_deg: float) -> float:
        return float(self.dial_sign * (raw_description_deg - self.zero_dial_deg))

    def base_row(self, raw_description_deg: float) -> np.ndarray:
        beta = self.physical_angle_deg(raw_description_deg)
        if self.name == "LP":
            matrix = linear_polarizer(beta)
        elif self.name == "CPL":
            assert self.theta_rel_deg is not None and self.retardance_deg is not None
            matrix = linear_polarizer(beta) @ retarder(beta + self.theta_rel_deg, self.retardance_deg)
        else:
            raise ValueError(f"Unsupported analyzer model: {self.name}")
        return matrix[0, :].astype(float)


@dataclass(frozen=True)
class FamilyGlue:
    family: str
    source: SourceModel
    analyzer: AnalyzerModel


def load_step1_models(step1_dir: Path) -> tuple[dict[str, FamilyGlue], dict[str, object]]:
    summary = json.loads((step1_dir / "summary.json").read_text(encoding="utf-8"))
    lp_zero = float(summary["lp_lp_anchor"]["zero_angle_deg"])
    source_theta_rel = float(summary["cpl_source_from_cpl_lp"]["source_theta_relaxed_model"]["theta_rel_deg"])
    source_delta = float(summary["cpl_source_from_cpl_lp"]["source_theta_relaxed_model"]["retardance_deg"])
    cpl_zero = float(summary["cpl_analyzer_from_lp_cpl"]["zero_angle_deg"])
    analyzer_delta_mag = float(summary["cpl_analyzer_from_lp_cpl"]["retardance_raw_deg"])

    source_lp = SourceModel(name="LP")
    source_cpl = SourceModel(name="CPL", theta_rel_deg=source_theta_rel, retardance_deg=source_delta)

    analyzer_lp = AnalyzerModel(name="LP", zero_dial_deg=lp_zero, dial_sign=-1)
    analyzer_cpl = AnalyzerModel(
        name="CPL",
        zero_dial_deg=cpl_zero,
        dial_sign=+1,
        theta_rel_deg=45.0,
        retardance_deg=-analyzer_delta_mag,
    )

    families = {
        "LP-LP": FamilyGlue("LP-LP", source_lp, analyzer_lp),
        "LP-CPL": FamilyGlue("LP-CPL", source_lp, analyzer_cpl),
        "CPL-LP": FamilyGlue("CPL-LP", source_cpl, analyzer_lp),
        "CPL-CPL": FamilyGlue("CPL-CPL", source_cpl, analyzer_cpl),
    }

    calibration = {
        "source_lp_state": [1.0, 1.0, 0.0, 0.0],
        "source_cpl_theta_rel_deg": source_theta_rel,
        "source_cpl_retardance_deg": source_delta,
        "lp_analyzer_zero_dial_deg": lp_zero,
        "lp_analyzer_description_to_physical_deg": "beta_lp = -(description - zero_lp)",
        "cpl_analyzer_zero_dial_deg": cpl_zero,
        "cpl_analyzer_description_to_physical_deg": "beta_cpl = +(description - zero_cpl)",
        "cpl_analyzer_theta_rel_deg": 45.0,
        "cpl_analyzer_retardance_deg": -analyzer_delta_mag,
        "retardance_sign_convention": "source positive, analyzer negative; only the relative sign is constrained by CPL-CPL AIR",
    }
    return families, calibration


def mark_excluded_points(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out["exclude_from_fit"] = False
    out["exclude_reason"] = ""
    out["description_deg"] = pd.to_numeric(out["description"], errors="coerce")
    out["capture_index_int"] = pd.to_numeric(out["capture_index"], errors="coerce")
    for rule in EXCLUDED_POINTS:
        mask = out["experiment"].eq(rule["experiment"]) & out["description_deg"].eq(rule["description"])
        mask &= out["capture_index_int"].eq(rule["capture_index"])
        out.loc[mask, "exclude_from_fit"] = True
        out.loc[mask, "exclude_reason"] = rule["reason"]
    return out


def enrich_frame(df: pd.DataFrame, family_glue: dict[str, FamilyGlue]) -> pd.DataFrame:
    out = mark_excluded_points(df)
    parsed = out["experiment"].apply(parse_experiment_name).apply(pd.Series)
    parsed.columns = [f"parsed_{column}" for column in parsed.columns]
    out = pd.concat([out, parsed], axis=1)
    out["family"] = out["experiment"].map(family_from_experiment)

    vector_names = vector_column_major_names("lincoef")
    vector_values: list[list[float]] = []
    source_base_values: list[list[float]] = []
    source_eff_values: list[list[float]] = []
    analyzer_base_values: list[list[float]] = []
    analyzer_eff_values: list[list[float]] = []
    analyzer_physical_values: list[float] = []
    object_phi_values: list[float] = []
    matrix_kind_values: list[str] = []
    source_kind_values: list[str] = []
    analyzer_kind_values: list[str] = []

    for _, row in out.iterrows():
        family = row["family"]
        if family not in family_glue:
            raise ValueError(f"Unsupported family: {family}")
        glue = family_glue[family]
        parsed_row = {
            "n_objects": row["parsed_n_objects"],
            "obj1": row["parsed_obj1"],
            "angle1_label": row["parsed_angle1_label"],
            "obj2": row["parsed_obj2"],
            "angle2_label": row["parsed_angle2_label"],
        }
        object_phi_deg, matrix_kind = object_rotation_from_parsed(parsed_row)
        raw_description_deg = float(row["description_deg"])

        s_base = glue.source.base_state()
        a_base = glue.analyzer.base_row(raw_description_deg)
        s_eff = rotation_matrix(object_phi_deg) @ s_base
        a_eff = a_base @ rotation_matrix(-object_phi_deg)
        lincoef = np.kron(s_eff, a_eff)

        vector_values.append(lincoef.tolist())
        source_base_values.append(s_base.tolist())
        source_eff_values.append(s_eff.tolist())
        analyzer_base_values.append(a_base.tolist())
        analyzer_eff_values.append(a_eff.tolist())
        analyzer_physical_values.append(glue.analyzer.physical_angle_deg(raw_description_deg))
        object_phi_values.append(object_phi_deg)
        matrix_kind_values.append(matrix_kind)
        source_kind_values.append(glue.source.name)
        analyzer_kind_values.append(glue.analyzer.name)

    out["object_phi_deg"] = object_phi_values
    out["effective_matrix_kind"] = matrix_kind_values
    out["source_model_kind"] = source_kind_values
    out["analyzer_model_kind"] = analyzer_kind_values
    out["analyzer_physical_deg"] = analyzer_physical_values

    for idx in range(4):
        out[f"s_base_{idx}"] = [values[idx] for values in source_base_values]
        out[f"s_eff_{idx}"] = [values[idx] for values in source_eff_values]
        out[f"a_base_{idx}"] = [values[idx] for values in analyzer_base_values]
        out[f"a_eff_{idx}"] = [values[idx] for values in analyzer_eff_values]
    for idx, name in enumerate(vector_names):
        out[name] = [values[idx] for values in vector_values]

    out = out.drop(columns=["capture_index_int"])
    return out


def main() -> None:
    args = parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)

    families, calibration = load_step1_models(args.step1_dir)
    written_files: list[str] = []

    for csv_name in SCIENCE_FILES:
        input_path = args.data_dir / csv_name
        df = pd.read_csv(input_path)
        enriched = enrich_frame(df, families)
        output_name = input_path.stem + "_glue.csv"
        enriched.to_csv(args.out_dir / output_name, index=False)
        written_files.append(output_name)

    summary = {
        "calibration": calibration,
        "object_rotation_convention": "Standing behind the laser and looking along the beam, +90 deg is counter-clockwise.",
        "object_frame_absorption": {
            "source": "s_eff = R(phi) s_base",
            "analyzer": "a_eff^T = a_base(alpha)^T R(-phi)",
        },
        "linear_coefficient_order_column_major": vector_column_major_names("lincoef"),
        "linear_identity": "I = (s_eff^T otimes a_eff^T) vec(M)",
        "files_written": written_files,
    }
    (args.out_dir / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")


if __name__ == "__main__":
    main()

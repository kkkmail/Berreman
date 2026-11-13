namespace BerremanTestSupport

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Optimization.StagedMuellerPath

/// The optics-mueller reference pipeline's full-precision results, transcribed from `reference_step1_summary.json` and
/// `reference_step2_linear_summary.json` in `data/optics-mueller/data/reference/` (step 329), which `MuellerReconstructionTests`
/// and AT-1 assert against, so no test reads a file for its expectations (spec 0044 §8.5; spec 0050 S10.3).
module MuellerReference =

    // ---------------------------------------------------------------------------------------------------
    // Spec 0044 §8.5 / §9 — acceptance bands for the measured-data facts, against the reference pipeline's
    // OWN full-precision output (data/optics-mueller/data/reference/reference_step1_summary.json and
    // reference_step2_linear_summary.json). These replaced the former ~1° / 2e-3 bands, which compared
    // against the report's ROUNDED printed values and therefore could not detect numerical drift smaller
    // than the rounding.
    //
    // The §9 protocol governs these two numbers: measure the actual agreement, pin ONE ORDER OF MAGNITUDE
    // looser than observed, and record the observed figure here. They are never widened to make a failing
    // test pass — the F# re-derives every constant independently through a different SVD implementation, so
    // agreement worse than ~1e-6 would indicate a PROCEDURAL divergence from the reference pipeline that
    // must be found and fixed, not absorbed.
    //
    // OBSERVED (measured 2026-08-02; recorded per §9, see specs/0044/.manual/006-implementation-log.md):
    //   Stage-1 constants — dark_mean, z_LP, z_CPL and δ_an reproduce the reference BIT-EXACTLY (Δ = 0);
    //                       θ_src is off by 7.1e-15° and δ_src by 1.4e-14°, i.e. one to two ulp.
    //   Stage-3 matrices  — worst element deviations 4.5e-11 (M_QZ, M[2,3]), 4.9e-11 (M_LR, M[0,0]) and
    //                       3.9e-11 (M_{QZ+LR}, M[1,3]).
    // The matrix figures are the amplification of that ulp-level calibration difference through a
    // 900-row × 16-column least-squares solve of condition number ≈ 10 — that is, the F# port and the Python
    // reference agree to the last bit the calculation can carry, and differ only in floating-point summation
    // order. For scale: the reconstructed elements are O(1) and the previous acceptance band was 2e-3, so
    // these bands are seven to eight orders of magnitude tighter than what they replace.
    // ---------------------------------------------------------------------------------------------------

    /// Acceptance band for the Stage-1 calibration constants (degrees). Pinned per §9 at ~70× the observed
    /// worst deviation of 1.4e-14°; the extra headroom over the bare order-of-magnitude rule absorbs
    /// cross-CPU differences in vectorized summation, which this quantity is directly exposed to.
    let stage1Tol : float = 1.0e-12

    /// Acceptance band for the Stage-3 reconstructed matrix elements and the §8 cascade metrics. Pinned per
    /// §9 at ~20× the observed worst deviation of 4.9e-11.
    /// It also bands the AIR gains, worst 2.9e-11 (`lp_cpl.gain_air2`) when spec 0050 was authored, 2026-11-04 (~34×, §9).
    /// It also bands the product-check RMSE ratio, whose observed deviation was 5.7e-13 when spec 0050 was authored, 2026-11-04.
    let dataMatrixTol : float = 1.0e-9

    /// `dark_mean_avg_total`.
    let darkMean : DarkMean = DarkMean 869.666

    /// `lp_lp_anchor.zero_angle_deg`.
    let lpAnalyzerZero : Angle = Angle.degree 155.44011830109392<deg>

    /// `cpl_analyzer_from_lp_cpl.zero_angle_deg`.
    let cplAnalyzerZero : Angle = Angle.degree 96.99613336321012<deg>

    /// `cpl_analyzer_from_lp_cpl.retardance_raw_deg`, the magnitude stage 1 derives.
    let cplAnalyzerRetardance : Retardance = Retardance.degree 83.66804489441891<deg>

    /// `cpl_source_from_cpl_lp.source_theta_relaxed_model.theta_rel_deg`.
    let cplSourceAxisOffset : Angle = Angle.degree 39.31100224361002<deg>

    /// `cpl_source_from_cpl_lp.source_theta_relaxed_model.retardance_deg`.
    let cplSourceRetardance : Retardance = Retardance.degree 82.55344158462403<deg>

    /// `qz_matrix`.
    let qzMatrix : MuellerMatrix =
        RealMatrix4x4.create
            [ [  0.9412063946;  0.0409543238; -0.0259356814; -0.1220013287 ]
              [  0.0525351141;  0.6606049231; -0.3659862371; -0.1421373359 ]
              [ -0.0129854586;  0.4061015401;  0.6094438915; -0.0169558310 ]
              [ -0.0957187769;  0.0752040278;  0.0184691606;  1.0306648444 ] ]
        |> MuellerMatrix

    /// `lr_matrix`.
    let lrMatrix : MuellerMatrix =
        RealMatrix4x4.create
            [ [  0.9931797644; -0.0046529422;  0.0596850441; -0.1036207527 ]
              [  0.3531940843;  0.1474307188; -0.0954724381; -0.6568249890 ]
              [  0.3425940212; -0.3946501705;  0.6900321099; -0.5968927113 ]
              [ -0.0104722933;  0.1247347199;  0.4614122160;  0.5910486653 ] ]
        |> MuellerMatrix

    /// `combined_matrix`.
    let combinedMatrix : MuellerMatrix =
        RealMatrix4x4.create
            [ [  0.9001173290;  0.0831007067;  0.0350162173; -0.1712567077 ]
              [  0.3413132788; -0.1694202380; -0.1576526274; -0.5502997288 ]
              [  0.3377633308; -0.1048712506;  0.7681312510; -0.5366531986 ]
              [  0.0999436274;  0.0459751884;  0.3292576266;  0.3168329794 ] ]
        |> MuellerMatrix

    /// `combined_vs_product_matrix.frobenius_norm`.
    let cascadeFrobenius : MatrixDeviation = MatrixDeviation 0.5714178795538456

    /// `combined_vs_product_matrix.mean_abs_entry_diff`.
    let cascadeMeanAbs : MatrixDeviation = MatrixDeviation 0.11328628636347338

    /// `combined_vs_product_matrix.max_abs_entry_diff`.
    let cascadeMaxAbs : MatrixDeviation = MatrixDeviation 0.26806464658062423

    /// `combined_vs_product_matrix.delta_matrix`: the 0-indexed positions of its two largest magnitudes.
    /// Spec 0044 note: delta[3][3] = -0.2680646466 and delta[3][1] = -0.2678257477 corroborate independently
    /// the data-driven positions the 0042 implementation chose over the slice text's.
    let cascadeLargestDeltaPositions : Set<MuellerRowIndex * MuellerColumnIndex> =
        Set.ofList [ (MuellerRowIndex 3, MuellerColumnIndex 3); (MuellerRowIndex 3, MuellerColumnIndex 1) ]

    /// `gain_summary.lp_lp.gain_air2`.
    let lpLpGainA : AirGain = AirGain 45028.89958338373

    /// `gain_summary.lp_lp.gain_air2r`.
    let lpLpGainB : AirGain = AirGain 44514.55313613668

    /// `gain_summary.lp_cpl.gain_air2`.
    let lpCplGainA : AirGain = AirGain 64241.39738340334

    /// `gain_summary.lp_cpl.gain_air2r`.
    let lpCplGainB : AirGain = AirGain 63009.84001492676

    /// `gain_summary.cpl_lp.gain_pre_air2`.
    let cplLpGainPre : AirGain = AirGain 36886.57620207925

    /// `gain_summary.cpl_lp.gain_post_air2r`.
    let cplLpGainPost : AirGain = AirGain 40015.20947516168

    /// `gain_summary.cpl_cpl_day1.gain_start`.
    let cplCplDay1GainStart : AirGain = AirGain 34623.69085844701

    /// `gain_summary.cpl_cpl_day1.gain_end`.
    let cplCplDay1GainEnd : AirGain = AirGain 37056.15607055087

    /// `gain_summary.cpl_cpl_day2.gain`.
    let cplCplDay2Gain : AirGain = AirGain 36225.080943100205

    /// `combined_check.fit_rmse`.
    let combinedFitRmse : ResidualRmse = ResidualRmse 0.10162659513724465

    /// `combined_check.product_rmse`.
    let productRmse : ResidualRmse = ResidualRmse 0.12833930975940816

    /// The report's `product / fit` (`matrix_fit_linear.py:334`).
    let productCheckRmseRatio : ProductRmseRatio = ProductRmseRatio.between productRmse combinedFitRmse

    /// `matrix_glue.py:22` `EXCLUDED_POINTS`, the caller's data to the reconstruction (`ExcludedPoint`, spec 0042 §0).
    let excludedPoints : ExcludedPoint list =
        [ { experiment = MuellerExperimentLabel "LP-(LR#90)-CPL-2"; description = Angle.degree 140.0<deg>; captureIndex = CaptureIndex 17 } ]

/// Placeholder for the library-agnostic optimization interface (§A.8): a
/// residual/objective `(float[] -> float[])`, optional Jacobian, bounds,
/// constraints, and a result record, with ALGLIB behind the seam. Part H
/// fills this in. Linear algebra MUST NOT be routed through ALGLIB.
module OpticalConstructor.Optimization.Optimization

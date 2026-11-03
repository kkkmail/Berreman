# Stored-format changes made by the ELEV ledger's rows

The row that changes a stored shape appends one line here (`002-elevation-plan.md` §5). A stored shape is a field or
case whose JSON, settings or database representation changed. Each line says what an old file now meets; under `020`
§R that is a typed error, never a migration. STL keeps the same list for its wire (STL `specs/0229/.manual/wire-changes.md`).

| Row | Type | Field or case | Stored in | What an old file meets |
|---|---|---|---|---|
| E022 | `OpticalConstructor.Optimization.FitQuality.FitReport` | every field: `chiSquared` / `mse` / `reducedChiSquared` / `degreesOfFreedom` elevated (`ChiSquared`, `MeanSquaredResidual`, `ReducedChiSquared`, `DegreesOfFreedom`), `parameters` / `standardErrors` a `DesignVector`, `confidenceIntervals` a `ParameterBounds` (was `(float * float)[]`), `covariance` / `correlation` a `ParameterMatrix`, `residuals` a `ResidualVector` | the `.fit-history.binz` sidecar (`FitReport.toBinz`, Softellect `BinaryZippedFormat`), whose bytes `SynthesisFitPage.saveFitReport` hands to its caller | the product never reads a sidecar back (no caller of `saveFitReport` writes one today); `FitReport.tryFromBinz` returns `Error (DeserializationExn _)` for an old payload, never a throw |

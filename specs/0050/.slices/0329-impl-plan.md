# Step 0329 — impl plan

## Approach

The reference half of `Berreman/Data/MuellerMatrix/` moves into the optics-mueller kit folder
`data/optics-mueller/data/reference/` (001 §Q AT-1, §S9; `.spec-md` §3.20). `data.zip` stays for step 330.
No code and no behaviour change.

1. Move the seven files under the same names: `README.md`, `cpl_cpl_analyzer.py`, `matrix_glue.py`,
   `matrix_step1_air_fit.py`, `matrix_fit_linear.py`, `reference_step1_summary.json`,
   `reference_step2_linear_summary.json`.
   - The worker base forbids git mutation commands, so the move is a filesystem `mv`. The supervisor's commit
     stages it, and git records it as a rename.
   - Hash the six scripts and summaries before and after, to prove they keep their bytes.
2. Edit the moved `README.md` only where the move makes it untrue:
   - (a) the opening paragraph;
   - (b) the three places that say `data.zip` is here;
   - (c) one new `Further reading` bullet.
3. Edit the provenance comments in `MuellerReconstructionTests.fs` (six sites: lines 31, 154, 504, 608–609,
   831–832, 996) to name the kit path. No code line changes.

## Files

- `data/optics-mueller/data/reference/*` (seven, moved from `Berreman/Data/MuellerMatrix/`)
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` (comments only)

## Risks

- **CRLF churn.** Verify LF after editing.
- **Untracked new files** until the supervisor stages them. Every s32 invariant greps tracked files under
  `Berreman/` or `Elevation/`, so none of them sees the kit path.
- **Out-of-scope stale texts.** `MuellerDataStore.fs:150` (the `archiveEntryName` doc) still points at
  `Berreman/Data/MuellerMatrix/README.md`; the slice assigns it to step 331. The class doc's `data.zip` bullet and
  `loadDataSet`'s doc stay for step 330.

## Verification

- Build.
- `MuellerReconstructionTests` (the selected class), with its count unchanged.
- Elevation.
- The 16 s32 invariants.
- Byte-for-byte hash compare.

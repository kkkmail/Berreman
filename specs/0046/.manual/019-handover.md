# 019 — Handover (terminal restart, 2026-10-26)

Read this first, then `CLAUDE.md`, then `003-implementation-log.md` (**Status** and the last sections).

## Where things stand

- Branch `0046`, working tree **clean**. Everything is committed by the operator; the last commits are
  `c0bfb48` (the five-item UI round below) and `4c4b450` (the `018` report). I have committed nothing.
- No work is in progress. No request is open: the last request (the research report `018`) was delivered
  and answered in prose.
- Last verified gate state, at `c0bfb48`:
  - full solution build: 0 errors, 0 warnings;
  - `OpticalConstructor.Tests` **727 / 727**;
  - `OpticalConstructor.Ui.Tests` **767 / 767** (ui-smoke 208);
  - `BerremanTests` NOT run (operator: "STOP berreman tests");
  - no CRLF churn.

## What this session did

1. **Five UI items** (log section "Open chart window removed, scroll law for every window, …"):
   - **Open chart window removed.** It duplicated a collected experiment's View.
   - **Every window fits its screen and scrolls.**
     - `Controls/WindowLayout.fs`: `MinWidthPanel` and `WindowFit.keepOnScreen` / `fitWithin`.
     - Every window constructor calls `keepOnScreen`, and `wheelTarget` sets the order: the page
       scrolls first, then the table zooms.
     - WrapPanel rows were made to actually wrap (DockPanel labels).
     - The law is `ScrollExtentTests`: at 800 × 600 and 1000 × 540, every control can be scrolled fully
       on screen.
   - **Data files inverse-only.** The Experiments bay's "Data files" section (Attach data file…) shows
     only in the Inverse constructor.
   - **Main opens on Add**, and the bay is renamed "Rotation" → **"Rotate"**.
   - **"Selector" → "Select".**
2. **Report `018-sharing-libraries-inverse-planner.md`** (research only, no code changes):
   - **Sharing** without a cloud:
     - identity is GUID + version + content hash, never a name;
     - a self-contained bundle carries the dependency closure and the data files;
     - an import-decision table settles each item;
     - an optional shared-folder library.
   - **Free optical-constant libraries** exist:
     - refractiveindex.info (CC0; the app already imports its YAML);
     - PTB OCDB for EUV/VUV, with uncertainties;
     - the CXRO Henke tables;
     - none has free gyration or magneto-optic data.

     Proposed: a `MaterialSourceProxy` with adapters.
   - **The "oracle"**, renamed **Measurement Planner**. It works in two layers:
     - a symmetry table;
     - a Fisher-information / Cramér–Rao recoverability report, with greedy optimal-design ranking of
       experiments.

     It is deterministic and cached per lab profile, with no LLM.

## Waiting on the operator (from `018`, none blocking)

- The bundle file extension.
- Whether a shared-folder library is wanted.
- Which planner target comes first: isotropic films, uniaxial crystals or gyrotropic crystals.

Nothing from `018` is implemented. Do not start it until the operator asks.

## Standing rules (beyond CLAUDE.md)

- **Never use the questions tool.** Prose only: short questions in the terminal reply, long ones in the
  next NNN MD here. This is in memory.
- **Never bump any version ring** (STL slow ring or Berreman) unless told "bump". This is in memory.
- **Do not run `BerremanTests`** unless told.
- **Commit or push only when asked.**
- **Git:** this shell needs `git -c safe.directory=C:/GitHub/Berreman <command>` (dubious ownership).
  Do not change the global config.
- **Before reporting a run:** verify LF line endings (`git diff --numstat` against
  `--ignore-cr-at-eol`) and zero warnings.
- **Test commands:** run the UI suite in full, about 5 min; ui-smoke is a subset. A failing click test
  usually means the target is scrolled out of view. The click helpers call `BringIntoView` first; keep
  that pattern.
- **This machine's screen** is a virtual 1024 × 768 (headless reports 1920 × 1280), not the operator's
  laptop. Do not judge real-screen fit from this machine.

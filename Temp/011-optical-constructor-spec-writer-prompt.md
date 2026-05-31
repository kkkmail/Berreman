# Spec-writer user prompt — author the Optical Constructor spec in architecture-first installments

**Role:** You are the spec-writer (spec-author cycle) of the AI-Strategy-Generator tool
(`C:\GitHub\AI-Strategy-Generator\`).

**Target repository (where every claim and pointer must resolve):** `C:\GitHub\Berreman\`.

**Source of truth (the preliminary spec to formalize):**
`C:\GitHub\Berreman\specs\0022\.manual\010-optical-constructor-spec-and-tech-stack.md`.

---

## 0. Why this prompt exists (read first)

The preliminary document `010-optical-constructor-spec-and-tech-stack.md` is large (≈480
lines, two feature tiers, nine feature areas, five implementation-decision areas). Past
attempts to turn it into one authoritative `.spec-md` in a single pass **fail to deliver
the whole spec** — the tail is truncated, dropped, or thinned.

**Do not try to author the whole spec at once.** Author it as an **ordered series of
self-contained installments**, starting from a **high-level architecture spine** and then
**working out the details one feature area at a time**, in priority order. Each installment
is a complete, house-style spec document on its own and is small enough to finish in one
pass without truncation.

This decomposition is *for authoring*. Once the installments exist and are approved, the
assembled authoritative spec can be fed to the splitter for implementation slicing — that
is a later, separate step and is **out of scope** for this prompt.

---

## 1. What to produce and where

Author installments as numbered Markdown files that **continue the `010-` sequence** in the
same operator folder:

```
C:\GitHub\Berreman\specs\0022\.manual\011-<arch-slug>.md      <- Installment A0 (architecture spine) — FIRST
C:\GitHub\Berreman\specs\0022\.manual\012-<area-slug>.md      <- Installment D1
C:\GitHub\Berreman\specs\0022\.manual\013-<area-slug>.md      <- Installment D2
...
```

- Produce **one installment file per cycle**. Stop after each file and report; do **not**
  begin the next installment until the previous one is delivered in full (this is the whole
  point — bounded output per pass).
- **Installment A0 (architecture) MUST come first** and MUST be delivered complete before
  any detail installment is started. Every detail installment depends on and cites A0.
- Do **not** write into `.spec-md`, `.slices/`, or any production-tree file of either repo
  in this exercise. Installments are operator-curated drafts in `.manual\`. Assembly of the
  approved installments into the authoritative `.spec-md` is a later operator step.

---

## 2. House style (binding — applies to every installment)

Follow the spec-author conventions exactly. In particular:

1. **Singular, not exploratory.** Pick one direction and state it. Do **not** enumerate
   "Option A vs Option B." The preliminary doc has already made the platform/storage/UI/
   numerics decisions (Part III) — restate them as commitments, do not re-open them.
2. **RFC 2119 keywords** in uppercase: **MUST / MUST NOT / SHALL / SHOULD / MAY**.
3. **Read before cite.** Before committing any `file:line` or `file:symbol` pointer into the
   Berreman repo, exercise the `Read` tool against that exact path and confirm the content
   matches the sentence. Never claim an "existing", "already wired", or "no change" artefact
   without a verified pointer — un-pointered "existing" claims are refuted by definition.
4. **No stale numeric counts.** Either omit counts or re-derive them at authoring time via
   Grep/Glob over the Berreman tree (e.g. "N ESCALATE sites" style hard-coding is forbidden).
5. **State scope positively.** Explicitly name expansions an LLM would plausibly hallucinate
   but that are out of scope (extra caching, retries, abstractions, config knobs, versioning,
   backwards-compat shims, migration code) and say they are NOT to be added unless the
   installment requires them.
6. **Priority ordering.** Within an installment, order parts by load-bearing importance and
   label the critical one `## Part X — <title> (CRITICAL — land first)`.
7. **Mandatory closing sections**, in order, at the end of every installment:
   `## Acceptance criteria` (numbered, testable `AC-<area><n>`),
   `## Files in scope` (every path the implementation may create/edit/delete, annotated
   `(new)`/`(edit)`/`(delete)`),
   `## Test artifacts` (concrete scripts/fixtures/commands; omit only if genuinely none),
   `## References` (carry forward the relevant external links from `010`).
8. **Outline skeleton** for each installment:
   `# Spec 0022.<NN> — <title>` → `## 0. Binding constraints` →
   `## Part A — … (CRITICAL — land first)` with `### A.0 Problem statement` then numbered
   directives `### A.1 …` → further parts → the mandatory closing sections.

### Fidelity rules (no silent drop, no invention)

- Every `[Core]` and `[Standard]` feature in `010` MUST be covered by exactly one
  installment, and each installment MUST state which `010` bullets it owns. Preserve the
  `[Core]`/`[Standard]` tier label on each feature — do not retier.
- Do **not** invent features, files, or decisions absent from `010` and absent from the
  Berreman tree. If a detail is needed but unstated, raise it (see §4) rather than guessing.
- Where `010` **deliberately defers** a decision — notably the FuncUI-clone **linking
  mechanism** ("local NuGet package OR project reference … made later, not now") and the
  **mandatory FuncUI-clone audit** gating step — carry it forward verbatim as an explicit
  `Deferred decision` / `Gating step` directive. Do **not** silently resolve or drop it.

---

## 3. The installment plan (author in this order)

Derive each installment's content from the cited section of `010`, grounded in verified
pointers into the Berreman engine. The engine is strong and mostly exists; the net-new work
is orchestration, data management, libraries, synthesis, and UI — order accordingly.

**Installment A0 — Architecture spine (CRITICAL — author first, deliver complete).**
The skeleton everything else hangs off. It MUST define, at the architecture level only
(no per-feature UI detail):
- Solution/assembly layout for a **.NET 10, F#-first** app (engine ⟶ orchestration ⟶ UI ⟶
  tooling), per `010` Part III §1.
- The **domain model** as F# records/DUs — `OpticalSystem`, `Layer`, `OpticalProperties`,
  and the **beam-tree topology** (`010` Part II §1): every non-mirror element is a node that
  splits incident light into a **transmitted** and a **reflected** branch; **mirrors are the
  special case** carrying the reflected branch only. Define the orchestration layer that sits
  **above** the existing single-system solver and routes the solver's reflected/transmitted
  outputs along each branch (carrying direction, energy, Jones/Stokes state).
- The **engine seams** to reuse, each with a verified pointer: the 4×4 solver
  (`Solvers.fs`, `Media.fs`), field outputs (`FieldFunctions.fs`), tensors
  (`MaterialProperties.fs`, `OpticalProperties/*`), sweeps (`Variables.fs`), charts
  (`Charting.fs`), the **math seam** (`MathNetNumericsMath.fs` / `ExtremeNumericsMath.fs`,
  `MatrixExp.fs`, `MatrixEvd.fs`), units (`Constants.fs`), Gaussian-beam scaffolding
  (`FourierTransform.fs`). Read each before citing.
- The **local-interface extension** contract for curved elements (`010` Part I "Engine note"
  + §1 lenses/curved mirrors): the one place a modest step beyond the 1D plane-wave model is
  allowed — local AOI per surface + ray/beam propagation between elements.
- The **storage architecture** (`010` Part III §2): canonical JSON + published JSON Schema
  validated on load; `.binz` zipped FsPickler (reuse `Softellect.Sys.Serialization`
  `BinaryZippedFormat`) for derived/bulk only; small flat YAML subset; libraries
  (`System.Text.Json` + FSharp.SystemTextJson, FSharp.Data, JsonSchema.Net).
- The **UI architecture** (`010` Part III §3): Avalonia + Avalonia.FuncUI (MVU/Elmish) +
  ScottPlot, WebView2-hosted Plotly.NET, OpenTK 3D viewport — plus the **FuncUI-clone audit
  gating step** and the **deferred linking-mechanism decision** carried as directives.
- The **numerical-library boundaries** (`010` Part III §4): Math.NET clone for linear
  algebra behind the math seam; **ALGLIB for optimization only**, behind a library-agnostic
  optimization interface `(float[] -> float[])` + Jacobian/bounds/constraints/result.
- The **units-of-measure spine** (`010` Part II §2): one canonical SI base (meters) stored
  internally, convert only at the UI/IO boundary; per-element default unit; the net-new
  eV (`E[eV] = 1239.84 / λ[nm]`) and cm⁻¹ conversions.
- A **cross-installment contract**: the named types, seams, file layout, and schema anchors
  that every later installment MUST cite rather than re-define.

**Detail installments — author after A0, in this priority order** (Core-first). Each owns one
feature area of `010` and cites A0's contract:
- **D1 — System construction & beam-tree orchestration** (`010` §1 Core: compose system,
  branching tree, mirror special case, stack/layer editor, incident/exit media, substrate
  thin/plate/wedge switch).
- **D2 — Lenses & curved mirrors** (`010` §1: focusing elements, coatings on curved
  surfaces, EUV depth-graded multilayer; the local-interface extension in concrete form).
- **D3 — Materials & units of measure** (`010` §2: materials library, dispersion-model
  editor, refractiveindex.info/CSV import-export, anisotropic definitions, automatic unit
  conversion incl. eV/cm⁻¹).
- **D4 — Light sources & illumination** (`010` §3).
- **D5 — Calculations & analysis** (`010` §4: spectral/angular scans, sweeps, polarimetric
  outputs, ellipsometric Ψ/Δ, EFI, absorptance, color, convergence controls).
- **D6 — Design synthesis, optimization & fitting** (`010` §5 + Part III §4: merit-function
  editor, Levenberg–Marquardt via ALGLIB `minlm` behind the optimization seam, inverse
  fitting, needle/global methods, validate against the Wolfram reference fits).
- **D7 — Charts & visualization** (`010` §6).
- **D8 — Project persistence & I/O** (`010` §7 + Part III §2: save/open JSON projects, the
  JSON Schema itself, `.binz` sidecars, export CSV/PNG/SVG/PDF, recent/autosave/undo).
- **D9 — UX shell, schematic view, persistent customization, onboarding** (`010` §1 UI
  surfaces, §8 environment/UX, §9 docs/help/onboarding).

You MAY merge or further split a detail installment if a single area is too large or two are
tightly coupled — but if you do, state the deviation and the reason at the top of the
installment, and keep the `[Core]`/`[Standard]` coverage complete with no silent drop.

---

## 4. When you are blocked or uncertain

If a needed detail is genuinely undetermined by both `010` and the Berreman tree, do **not**
invent it. Emit a **questions-only** response for that installment listing the specific
decisions the operator must make, with your single recommended default for each. Prefer to
keep momentum: author everything that *is* determined and quarantine only the open question.

---

## 5. Definition of done (per installment)

An installment is complete when:
1. It owns a stated, non-overlapping subset of `010`'s `[Core]`/`[Standard]` features.
2. Every "existing engine" claim carries a `Read`-verified `file:line`/`file:symbol` pointer
   into `C:\GitHub\Berreman\`.
3. It ends with `## Acceptance criteria`, `## Files in scope`, `## Test artifacts`,
   `## References`, all populated (or `## Test artifacts` justified as empty).
4. It was delivered in full in a single pass with no truncation.
5. For A0 specifically: it defines the cross-installment contract (types, seams, layout,
   schema anchors) that D1…D9 cite.

Begin with **Installment A0 (architecture spine)**. Deliver it complete, then stop and report
before starting D1.

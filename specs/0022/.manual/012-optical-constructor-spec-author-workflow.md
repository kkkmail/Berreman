# 012 — Optical Constructor spec-author Workflow (init → map → reduce)

**What this is.** A self-contained **Workflow script** that autonomously authors the Berreman
*Optical Constructor* `.spec-md` from the preliminary document
`C:\GitHub\Berreman\specs\0022\.manual\010-optical-constructor-spec-and-tech-stack.md`,
using an **init → map → reduce** pipeline and enforcing the spec-writer's exact house style
(top sections `## Part A`, `## Part B`, … `## Part J`; `### X.0 Problem statement` +
numbered `### X.1` directives; RFC 2119; Read-before-cite pointers; unified closing
sections). No operator assembly is required — the **reduce** phase merges the pieces into one
spec and a **verify** phase checks for dropped/invented content.

**How to run (clean session).** Hand this file to a fresh Claude Code session and say "run the
workflow in this file". The session MUST invoke the **Workflow** tool, passing the JavaScript
below (everything inside the fenced block) **verbatim** as the `script` argument. The user
providing this file is the explicit opt-in for multi-agent orchestration. Optional `args`:
`{ "outPath": "<abs path>", "specId": "0022" }` — defaults are set in the script.

**Side effects.** Only the **reduce** (and, if needed, **repair**) agent writes a file: the
assembled draft spec at `outPath` (default
`C:\GitHub\Berreman\specs\0022\.manual\013-optical-constructor-spec.md`). All other agents are
read-only (they Read `010` and the Berreman tree for Read-before-cite). The assembled draft is
an operator artifact; the intended next step is to `init`-seed it into a spec-bundle and run
the spec-writer's claim-check / handoff → splitter → arc-runner pipeline.

````javascript
export const meta = {
  name: 'optical-constructor-spec-author',
  description: 'Author the Berreman Optical Constructor .spec-md from preliminary doc 010 via init→map→reduce in spec-writer house style',
  phases: [
    { title: 'Init',   detail: 'Author Part A (architecture spine) + cross-cutting contract + binding constraints' },
    { title: 'Map',    detail: 'Author Parts B–J in parallel, each citing Part A\'s contract' },
    { title: 'Reduce', detail: 'Merge Parts A–J into one .spec-md; resolve cross-part inconsistencies; write the file' },
    { title: 'Verify', detail: 'Fidelity pass: no [Core]/[Standard] drop, no invention, pointers Read-verify; one repair if needed' },
  ],
}

// ---------------------------------------------------------------------------
// Fixed inputs / outputs
// ---------------------------------------------------------------------------
const SRC     = 'C:/GitHub/Berreman/specs/0022/.manual/010-optical-constructor-spec-and-tech-stack.md'
const REPO    = 'C:/GitHub/Berreman'
const OUT     = (args && args.outPath) || 'C:/GitHub/Berreman/specs/0022/.manual/013-optical-constructor-spec.md'
const SPEC_ID = (args && args.specId) || '0022'

// ---------------------------------------------------------------------------
// House style — enforced verbatim by every authoring agent.
// Distilled from C:/GitHub/AI-Strategy-Generator/src/schemas/spec_writer/spec_author/spec-author.system-md
// ---------------------------------------------------------------------------
const STYLE = [
  'HOUSE STYLE — the output is a `.spec-md`. Enforce exactly:',
  '- Top-level sections: `## 0. Binding constraints`, then `## Part A — <title>`, `## Part B — <title>`, … in SINGLE ASCENDING LETTERS (A, B, C, D, …). NEVER `A1`/`A2`, NEVER `D1`/`D2`. Subsections are `### A.0 Problem statement`, `### A.1`, `### A.2`, …',
  '- `### X.0 Problem statement` = 2–6 paragraphs of descriptive context — the ONLY narrative; everything below it is directive.',
  '- `### X.1 … X.N` = numbered directives; each begins with the MUST/SHALL action, names the concrete file/function, then expands.',
  '- Singular and authoritative. NEVER "Option A vs Option B" in the body. NEVER "TBD" / "implementer should investigate" / "information needed".',
  '- RFC 2119 keywords in UPPERCASE: MUST / MUST NOT / SHALL / SHOULD / SHOULD NOT / MAY. Lowercase words carrying normative intent are forbidden — either capitalise or move to the Problem statement as prose.',
  '- State scope POSITIVELY; explicitly name plausible-expansion exclusions where they intersect this change (caching, retries, abstractions, config knobs, versioning, error wrapping, backwards-compat shims, migration code). Name the ones in play; forbid the unintended.',
  '- MINIMUM implementation only. Anything beyond minimum MUST trace to a named directive. Justify every new module/file/abstraction against a directive or drop it.',
  '- REUSE before invention: when new behaviour touches an area with existing primitives, name the 1–3 primitives to reach for first — one line each + a `file:line`/`file:symbol` citation. Do not enumerate the whole neighbourhood.',
  '- NEVER claim an existing artefact ("existing X", "already wired", "no change", "same as today", "current mechanism") without a concrete `<file>:<symbol>` or `<file>:<line>` pointer in the same or adjacent sentence.',
  '- READ-BEFORE-CITE: before committing ANY `file:line`/`file:symbol` pointer, exercise the Read tool over that exact path (offset near the line) and confirm the content matches the sentence. Un-verified pointers are refuted by definition.',
  '- NO stale numeric counts: drop the number, or re-derive it via Grep/Glob against the tree right now. Never copy a count out of 010 or these notes.',
  '- Locality of reasoning: do NOT direct the implementer toward reflection / codegen / dynamic dispatch / hidden globals / monkey-patching unless a named requirement forces it. LLMs follow imports, not magic.',
  '- Operator-facing UX checklist — apply ONLY to a part that introduces a top-level operator page, a long-running background job, or a destructive action. When triggered, the part MUST commit explicitly on: (1) discoverability/nav entry, (2) progress granularity, (3) pre-confirmation gate vs one-click, (4) completion→results refresh mechanism, (5) opposing-action button UX + undo, (6) committable file vs root .gitignore collisions. A part doing none of those three things SKIPS this checklist entirely.',
  '- TIER FIDELITY: preserve the [Core] / [Standard] label on every feature carried from 010. Do not retier, do not silently drop a feature, do not invent a feature absent from both 010 and the Berreman tree.',
  '- DEFERRED DECISIONS: where 010 deliberately defers a choice — the FuncUI-clone LINKING MECHANISM ("local NuGet package OR project reference … made later, not now") and the mandatory FuncUI-clone AUDIT gating step — carry it forward VERBATIM as an explicit directive. Do NOT resolve it away and do NOT drop it.',
  '- Acceptance-criteria ids are keyed to the part letter: AC-<letter><n> (e.g. AC-B1, AC-B2). Each is one observable outcome of the form "After <trigger>, <subject> MUST <observable behaviour> within <bound>".',
].join('\n')

// ---------------------------------------------------------------------------
// Schemas
// ---------------------------------------------------------------------------
const PART_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    letter:     { type: 'string', description: 'Single uppercase part letter, e.g. "B"' },
    markdown:   { type: 'string', description: 'The full `## Part X — <title>` block: heading, ### X.0 Problem statement, numbered ### X.1.. directives. No closing sections here.' },
    acceptance: { type: 'array', items: { type: 'string' }, description: 'AC-<letter>N bullet lines owned by this part' },
    files:      { type: 'array', items: { type: 'string' }, description: '`<path>` (new|edit|delete) — role lines this part touches' },
    tests:      { type: 'array', items: { type: 'string' }, description: 'Test artifacts (scripts/fixtures/commands) for this part; empty if genuinely none' },
    references: { type: 'array', items: { type: 'string' }, description: 'Relevant external reference URLs from 010 for this part' },
  },
  required: ['letter', 'markdown', 'acceptance', 'files', 'tests', 'references'],
}

const ARCH_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    letter:             { type: 'string' },
    specTitle:          { type: 'string', description: 'One-line title for `# Spec 0022 — <title>`' },
    bindingConstraints: { type: 'string', description: 'Body of `## 0. Binding constraints` — constraints that apply to every part' },
    contract:           { type: 'string', description: 'Cross-cutting contract every Part B–J must cite: named F# types (OpticalSystem/Layer/OpticalProperties + the beam-tree node/branch types), engine seams with Read-verified pointers, assembly/file layout, JSON-schema anchors, the canonical-SI units base.' },
    markdown:           { type: 'string' },
    acceptance:         { type: 'array', items: { type: 'string' } },
    files:              { type: 'array', items: { type: 'string' } },
    tests:              { type: 'array', items: { type: 'string' } },
    references:         { type: 'array', items: { type: 'string' } },
  },
  required: ['letter', 'specTitle', 'bindingConstraints', 'contract', 'markdown', 'acceptance', 'files', 'tests', 'references'],
}

const REDUCE_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    path:           { type: 'string', description: 'Absolute path the assembled .spec-md was written to' },
    inconsistencies:{ type: 'array', items: { type: 'string' }, description: 'Cross-part conflicts found and how each was resolved' },
    partOrder:      { type: 'array', items: { type: 'string' }, description: 'Part letters in final order, must be A,B,C,…' },
  },
  required: ['path', 'inconsistencies', 'partOrder'],
}

const VERDICT_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    clean:           { type: 'boolean' },
    droppedFeatures: { type: 'array', items: { type: 'string' }, description: '[Core]/[Standard] features in 010 not covered by any part' },
    inventedItems:   { type: 'array', items: { type: 'string' }, description: 'Spec content not traceable to 010 or the Berreman tree' },
    badPointers:     { type: 'array', items: { type: 'string' }, description: 'Cited file:line pointers whose content does not match the claim' },
    notes:           { type: 'string' },
  },
  required: ['clean', 'droppedFeatures', 'inventedItems', 'badPointers'],
}

// ---------------------------------------------------------------------------
// Part assignment — deterministic A..J so the letters never drift.
// Part A (architecture) is authored in Init; B..J in Map.
// ---------------------------------------------------------------------------
const MAP_PARTS = [
  { letter: 'B', title: 'Optical-system construction & beam-tree orchestration',
    source: '010 Part II §1 [Core]: compose end-to-end system; branching tree of beams (transmitted+reflected); mirrors = reflected-branch-only special case; visual stack/layer editor; incident & exit medium selectors; substrate thin-film vs plate vs wedge switch; per-element default-unit hooks (full unit behaviour lives in Part D).' },
  { letter: 'C', title: 'Lenses & non-flat (curved) mirror elements',
    source: '010 Part I "Engine note for curved elements" + Part II §1 [Core] lenses/curved mirrors: focusing elements, radius/curvature/aperture/substrate, coatings on curved surfaces, EUV depth-graded multilayer spacing; the concrete local-interface extension (local AOI per surface + ray/beam propagation between elements).' },
  { letter: 'D', title: 'Optical substances / materials management & units of measure',
    source: '010 Part II §2: materials library; dispersion-model editor (Sellmeier/Cauchy/Lorentz/Drude/Tauc-Lorentz/etc.); refractiveindex.info + CSV import/export; anisotropic material definition; material preview plots; AUTOMATIC units-of-measure with per-element default unit and canonical-SI internal base; net-new eV (E[eV]=1239.84/λ[nm]) and cm⁻¹ conversions.' },
  { letter: 'E', title: 'Light sources & illumination',
    source: '010 Part II §3: source/incident-light editor (wavelength, angle, polarization state, intensity); polarization presets + Poincaré/ellipse picker; spectral source profiles; cone/acceptance; Gaussian-beam source; coherent/incoherent flags; multiple sources.' },
  { letter: 'F', title: 'Calculations & analysis',
    source: '010 Part II §4: spectral/angular R,T,A scans; parameter sweeps; polarimetric outputs (Stokes, Jones & Mueller — finish the commented-out muellerMatrix path); ellipsometric Ψ,Δ (N,C,S); in-layer field/EFI profile; absorptance per layer; color (CIE XYZ/Lab/sRGB); convergence controls.' },
  { letter: 'G', title: 'Design synthesis, optimization & fitting',
    source: '010 Part II §5 + Part III §4: target/merit-function editor; local refinement (Levenberg–Marquardt + quasi-Newton + simplex) with bounds/inequality targets; inverse fitting from measured R/T and Ψ,Δ; fit-quality reporting; needle/tunneling synthesis; global optimization. ALGLIB used for OPTIMIZATION ONLY, behind a library-agnostic optimization interface; validate minlm against the Wolfram reference fits.' },
  { letter: 'H', title: 'Charts & visualization',
    source: '010 Part II §6: interactive 1D plots; overlay/comparison; 2D/3D surface & contour; chart customization; cursor/readout & markers; specialized plots (polarization ellipse, Poincaré sphere, field-depth, CIE, n,k dispersion).' },
  { letter: 'I', title: 'Project management, persistence & I/O',
    source: '010 Part II §7 + Part III §2: save/open JSON projects (canonical JSON + published JSON Schema validated on load); the JSON Schema itself; .binz zipped FsPickler sidecars for derived/bulk only (reuse Softellect.Sys.Serialization BinaryZippedFormat); export CSV/Excel + PNG/SVG/PDF; recent files, autosave, undo/redo; report generator; material library import/export; design history/diff.' },
  { letter: 'J', title: 'Environment, UX shell, schematic view, persistent customization & onboarding',
    source: '010 Part II §1 UI surfaces (schematic cross-section view, repeat/period builder, templates/wizards, drag-and-drop, multi-system workspace, persistent UI customization / favorites board) + §8 environment/UX (preferences, theme, dockable panels, input validation, progress/cancel) + §9 documentation/help/onboarding.' },
]

// ---------------------------------------------------------------------------
// INIT — Part A: architecture spine + cross-cutting contract + binding constraints
// ---------------------------------------------------------------------------
phase('Init')

const archPrompt = [
  'You are the spec-author. Author **Part A — Solution architecture & cross-cutting contract** of the authoritative `.spec-md` for the Berreman *Optical Constructor* app, plus the document-wide `## 0. Binding constraints`.',
  '',
  'FIRST: Read the preliminary spec IN FULL: `' + SRC + '`. It is the source of truth for scope; do not invent beyond it.',
  'The target repository where every pointer MUST resolve is `' + REPO + '`. Use Read/Grep/Glob there and obey Read-before-cite.',
  '',
  'Part A is the load-bearing spine every later part depends on and MUST be labelled `## Part A — <title> (CRITICAL — land first)`. At the architecture level only (no per-feature UI detail), Part A MUST define:',
  '- Solution / assembly layout for a .NET 10, F#-first app (engine → orchestration → UI → tooling) — 010 Part III §1.',
  '- The DOMAIN MODEL as F# records/DUs — OpticalSystem, Layer, OpticalProperties — and the BEAM-TREE topology (010 Part II §1): every non-mirror element is a node splitting incident light into a transmitted branch and a reflected branch; mirrors are the special case carrying the reflected branch ONLY. Define the orchestration layer ABOVE the existing single-system solver that routes the solver\'s reflected/transmitted outputs along each branch (direction, energy, Jones/Stokes).',
  '- The engine SEAMS to reuse, each with a Read-verified pointer into ' + REPO + ': the 4×4 solver (Solvers.fs, Media.fs), field outputs (FieldFunctions.fs), tensors (MaterialProperties.fs, OpticalProperties/*), sweeps (Variables.fs), charts (Charting.fs), the math seam (MathNetNumericsMath.fs / ExtremeNumericsMath.fs, MatrixExp.fs, MatrixEvd.fs), units (Constants.fs), Gaussian-beam scaffolding (FourierTransform.fs), source/fields (Fields.fs, StandardLightVariables.fs). Verify each path exists before citing; if a name differs in-tree, cite the real one.',
  '- The local-interface EXTENSION contract for curved elements (010 Part I engine note): the one allowed step beyond the 1D plane-wave model — local AOI per surface + ray/beam propagation between elements. (Detailed in Part C; Part A states the contract.)',
  '- STORAGE architecture (010 Part III §2): canonical JSON + published JSON Schema validated on load; .binz zipped FsPickler for derived/bulk only; small flat YAML subset; libraries (System.Text.Json + FSharp.SystemTextJson, FSharp.Data, JsonSchema.Net).',
  '- UI architecture (010 Part III §3): Avalonia + Avalonia.FuncUI (MVU/Elmish) + ScottPlot, WebView2-hosted Plotly.NET, OpenTK 3D viewport. Carry the FuncUI-clone AUDIT gating step and the DEFERRED linking-mechanism decision verbatim as directives.',
  '- NUMERICAL-library boundaries (010 Part III §4): Math.NET clone for linear algebra behind the math seam; ALGLIB for OPTIMIZATION ONLY behind a library-agnostic optimization interface (float[] -> float[]) + Jacobian/bounds/constraints/result record.',
  '- UNITS spine (010 Part II §2): one canonical SI base (meters) stored internally, convert only at the UI/IO boundary; per-element default unit; net-new eV and cm⁻¹ conversions. (Material UI detail is Part D.)',
  '',
  'Also produce:',
  '- `bindingConstraints`: the body of `## 0. Binding constraints` — constraints that apply to EVERY part (platform/language commitment, the reuse-before-invention rule, the canonical-units rule, the JSON-canonical/.binz-derived rule, the FuncUI audit gate).',
  '- `contract`: a compact, explicit list the later parts (B–J) will cite rather than re-define — the exact named types, the engine seams with their verified pointers, the assembly/file layout, the JSON-schema anchor names, and the canonical-units base. This is the single source other parts import.',
  '',
  'Return `letter`:"A", `specTitle`, `bindingConstraints`, `contract`, `markdown` (the full Part A block), and Part A\'s `acceptance` (AC-A1..), `files`, `tests`, `references`.',
  '',
  STYLE,
].join('\n')

const arch = await agent(archPrompt, { label: 'init:Part A', phase: 'Init', schema: ARCH_SCHEMA })
const partA = { ...arch, title: 'Solution architecture & cross-cutting contract' }
log('Init done: Part A authored; contract established for Parts B–J')

// ---------------------------------------------------------------------------
// MAP — Parts B..J in parallel, each citing Part A's contract
// ---------------------------------------------------------------------------
phase('Map')

const mapResults = await parallel(MAP_PARTS.map((p) => () => {
  const prompt = [
    'You are the spec-author. Author exactly ONE part — **`## Part ' + p.letter + ' — ' + p.title + '`** — of the authoritative `.spec-md` for the Berreman *Optical Constructor* app.',
    '',
    'Scope of THIS part (from the preliminary spec): ' + p.source,
    '',
    'FIRST: Read the preliminary spec IN FULL for context and to lift the exact [Core]/[Standard] bullets you own: `' + SRC + '`.',
    'Target repo for all pointers (Read-before-cite): `' + REPO + '`.',
    '',
    'You MUST build on Part A. Do NOT re-define anything in Part A\'s contract — cite it. Part A title: "' + partA.title + '".',
    'Part A binding constraints (apply to you):',
    partA.bindingConstraints,
    '',
    'Part A cross-cutting contract (cite these named types/seams/anchors; do not redefine):',
    partA.contract,
    '',
    'Author `markdown` as a single `## Part ' + p.letter + ' — ' + p.title + '` block: the heading, then `### ' + p.letter + '.0 Problem statement` (2–6 descriptive paragraphs), then numbered directives `### ' + p.letter + '.1`, `### ' + p.letter + '.2`, … Each directive MUST/SHALL name a concrete file/function in ' + REPO + ' or a concrete new file, with Read-verified pointers for every "existing" claim. Preserve [Core]/[Standard] tier labels. Do NOT write closing sections inside the part — return them separately.',
    '',
    'Return: `letter`:"' + p.letter + '", `markdown`, `acceptance` (AC-' + p.letter + '1.. lines), `files` ("`<path>` (new|edit|delete) — role"), `tests`, `references` (relevant 010 URLs for this part).',
    '',
    STYLE,
  ].join('\n')
  return agent(prompt, { label: 'map:Part ' + p.letter, phase: 'Map', schema: PART_SCHEMA })
    .then((r) => r && { ...r, title: p.title })
}))

const mapParts = mapResults.filter(Boolean)
log('Map done: ' + mapParts.length + '/' + MAP_PARTS.length + ' parts authored')

// ---------------------------------------------------------------------------
// REDUCE — merge A..J into one .spec-md, resolve inconsistencies, write the file
// ---------------------------------------------------------------------------
phase('Reduce')

const allParts = [partA, ...mapParts].map((p) => ({
  letter: p.letter, title: p.title, markdown: p.markdown,
  acceptance: p.acceptance, files: p.files, tests: p.tests, references: p.references,
}))

const reducePrompt = [
  'You are the spec-author REDUCE step. Merge the authored parts below into ONE authoritative `.spec-md` and WRITE it (use the Write tool) to: `' + OUT + '`.',
  '',
  'Assemble in EXACTLY this structure and order:',
  '1. `# Spec ' + SPEC_ID + ' — ' + (partA.specTitle || '<title>') + '`',
  '2. `## 0. Binding constraints` — the binding-constraints body provided below.',
  '3. A `---` separator, then each part\'s markdown in ascending letter order A, B, C, …, J. Each part block already begins with `## Part X — …`; keep `## Part A — … (CRITICAL — land first)` as-is. Put a `---` between parts.',
  '4. `## Acceptance criteria` with one `### Part X acceptance` subsection per part, listing that part\'s AC-X# bullets.',
  '5. `## Files in scope` — the DEDUPLICATED union of every part\'s files lines (one `<path>` appears once; merge role notes; keep the (new|edit|delete) annotation).',
  '6. `## Test artifacts` — the deduplicated union of every part\'s test artifacts.',
  '7. `## References` — the deduplicated union of every part\'s references.',
  '',
  'RECONCILE cross-part inconsistencies before writing — this is the whole point of reduce:',
  '- Resolve any type/record/DU defined in two parts down to a single definition (prefer Part A\'s contract).',
  '- Resolve conflicting file paths or conflicting (new|edit|delete) annotations for the same file; a file created in one part is (edit) in later parts, not (new) twice.',
  '- Unify terminology (e.g. "branch"/"beam node"/"attachment point") to the names Part A established.',
  '- Remove duplicated narrative; keep each directive in exactly one part.',
  '- Verify the letter sequence is contiguous A..J with no `A1`/`D2`-style headings.',
  'Do NOT silently drop or invent content while reconciling; if two parts genuinely conflict on intent, keep Part A\'s framing and record the conflict in `inconsistencies`.',
  '',
  'After writing the file, return `path` (= "' + OUT + '"), `inconsistencies` (each conflict found and how you resolved it), and `partOrder` (the letters in final order).',
  '',
  'Binding constraints body:',
  partA.bindingConstraints,
  '',
  'Authored parts (JSON):',
  JSON.stringify(allParts),
  '',
  STYLE,
].join('\n')

const reduced = await agent(reducePrompt, { label: 'reduce:merge', phase: 'Reduce', schema: REDUCE_SCHEMA })
log('Reduce done: wrote ' + reduced.path + ' (' + reduced.inconsistencies.length + ' inconsistencies resolved)')

// ---------------------------------------------------------------------------
// VERIFY — fidelity: no drop, no invention, pointers Read-verify. One repair if needed.
// ---------------------------------------------------------------------------
phase('Verify')

const verifyPrompt = [
  'You are an adversarial FIDELITY checker. Read the preliminary spec `' + SRC + '` and the assembled spec `' + reduced.path + '` end to end, and audit the assembled spec against the original. Use ' + REPO + ' for pointer checks.',
  'Report:',
  '- `droppedFeatures`: every [Core] or [Standard] feature in 010 NOT covered by any Part A–J of the assembled spec.',
  '- `inventedItems`: assembled-spec content (features, files, decisions) NOT traceable to 010 or to the Berreman tree — including any decision 010 deliberately deferred that the spec silently resolved (FuncUI linking mechanism; FuncUI audit gate).',
  '- `badPointers`: sample the `file:line`/`file:symbol` pointers in the assembled spec; Read each and list any whose content does not match the claim.',
  '- `clean`: true ONLY if all three lists are empty.',
  'Be strict; default to listing a concern rather than letting it pass.',
].join('\n')

const verdict = await agent(verifyPrompt, { label: 'verify:fidelity', phase: 'Verify', schema: VERDICT_SCHEMA })

if (verdict.clean) {
  log('Verify clean — assembled spec at ' + reduced.path)
  return { output: reduced.path, specId: SPEC_ID, parts: reduced.partOrder, inconsistencies: reduced.inconsistencies, verdict }
}

log('Verify found gaps — running one repair pass')
const repairPrompt = [
  'You are the spec-author REPAIR step. The assembled spec at `' + reduced.path + '` has fidelity gaps. Read it, fix ONLY the issues below, and re-WRITE the same file at `' + reduced.path + '` preserving structure and house style (Parts A..J, the closing sections, RFC 2119, Read-before-cite).',
  '- Restore each dropped feature into its correct part (add the directive + an AC-X# + a files-in-scope line): ' + JSON.stringify(verdict.droppedFeatures),
  '- Remove or correct each invented item; restore any deferred decision as an explicit carry-forward directive: ' + JSON.stringify(verdict.inventedItems),
  '- Fix or remove each bad pointer (Read-verify the replacement): ' + JSON.stringify(verdict.badPointers),
  'Do not introduce new scope. After writing, return `path`, `inconsistencies` (what you changed), and `partOrder`.',
  '',
  STYLE,
].join('\n')

const repaired = await agent(repairPrompt, { label: 'reduce:repair', phase: 'Verify', schema: REDUCE_SCHEMA })
log('Repair done — assembled spec at ' + repaired.path)
return { output: repaired.path, specId: SPEC_ID, parts: repaired.partOrder, firstPassVerdict: verdict, repairChanges: repaired.inconsistencies }
````

## Notes

- **Section naming is enforced** at three layers: the deterministic `MAP_PARTS` letters (A–J), the `STYLE` block (single ascending letters, `### X.0/X.1`), and the reduce step's explicit "verify the letter sequence is contiguous A..J with no `A1`/`D2`-style headings."
- **init → map → reduce** maps to phases `Init` (Part A + contract) → `Map` (Parts B–J in parallel, each importing Part A's contract) → `Reduce` (single merged `.spec-md` + inconsistency resolution) → `Verify` (adversarial fidelity, one conditional repair). No operator assembly.
- **Scale:** ~1 + 9 + 1 + 1 (+1 repair) ≈ 12–13 agents. Increase coverage by raising verifier strictness or adding a second map pass if a part comes back thin.
- **After it runs:** the assembled draft at `outPath` is ready to be `init`-seeded into a spec-bundle and pushed through the spec-writer's own **claim-check → handoff → splitter → arc-runner** pipeline.

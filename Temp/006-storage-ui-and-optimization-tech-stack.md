# Tech-Stack Research: Storage Formats, Windows UI, and Optimization Library

Scope: a discussion/evaluation report (not a design) for three decisions on the path
to a full optical-constructor app built on this F#/.NET 9 codebase. Three independent
topics: (1) high-level storage formats, judged by **LLM-maintainability**; (2) UI tech
stack for a **Windows-only** app, with required open-source libraries; (3) the hard
one — a **numerical optimization** strategy, weighing .NET-native libraries against
**native (C/C++/FORTRAN) interop**.

Relevant facts about the current codebase that shape the recommendations:

- It is **F#/.NET 9**, with the domain model expressed as **F# records and
  discriminated unions** (`OpticalSystem`, `Layer`, `OpticalProperties`, …).
- It already references **three numerical libraries**: `MathNet.Numerics`,
  **Extreme Optimization** (`Extreme.Mathematics`, a *commercial* .NET numerical
  library — see `ExtremeNumericsMath.fs`), and **ALGLIB** (the `!Temp/OdeSolvers`
  `Solver.fs` already calls `alglib.odesolver*`). So **ALGLIB is already a known
  quantity in this codebase.**
- It already uses **Plotly.NET** for charts and has **a proven, production native-interop
  pattern**: the sibling repo **`C:\GitHub\OdePackInterop`** (NuGet
  `Softellect.OdePackInterop`) wraps the classic **FORTRAN ODEPACK** solver — see the
  *"established FORTRAN interop"* box in §3.

---

## 1. Storage / high-level data formats (LLM-maintainability lens)

### What "easy for an LLM to maintain" actually means

The deciding factors are not exotic:

- **Plain UTF-8 text**, so a model can read and rewrite it directly.
- **Comments allowed**, so intent/units/citations live next to the data.
- **Diff-friendly / line-oriented**, so edits are small, reviewable, and merge cleanly
  (this is the single biggest factor for both LLM and human maintenance).
- **Low ambiguity / hard to corrupt** with a small edit (an LLM dropping one space
  should not silently change meaning).
- **A machine-checkable schema**, so a model's edit can be *validated* before it is
  trusted. This is the real lever — format choice matters less than having a schema.
- **Token efficiency** is a minor, secondary concern.

### The candidates

| Format | Comments | Diff-friendly | Error-proneness | Schema story | Notes |
|---|---|---|---|---|---|
| **JSON** | No (native) | Good | Low (explicit delimiters) | Excellent (JSON Schema) | Ubiquitous; verbose; every tool/LLM knows it cold |
| **JSONC / JSON5** | Yes | Good | Low | Via JSON Schema | JSON + comments/trailing commas; great for hand/LLM editing |
| **YAML** | Yes | Good | **High** (indentation, tabs, `:` quirks, "Norway problem") | JSON Schema works | Compact & readable but the easiest to *silently* break |
| **TOML** | Yes | **Very good** (flat, line-oriented) | Low | Weak/younger tooling | Ideal for config/preferences; awkward for deep nesting |
| **XML** | Yes | Verbose diffs | Low | Excellent (XSD) | Heavy, dated; no advantage here |
| **F# script (`.fsx`) as data** | Yes | Good | Low (compiler-checked!) | The F# type system | Already how examples are authored; see note below |
| **SQLite / binary** | n/a | **Poor** (opaque) | n/a | n/a | Not LLM-editable; only for large result sets/caches |

### Reading the table

- **JSON is the safe default for the project/document file.** It is the lingua franca
  LLMs handle most reliably and unambiguously, has first-class **JSON Schema**
  validation, and diffs well. Its only real weakness — no comments — is removed by
  using **JSONC/JSON5** for files meant to be hand- or model-edited, or by carrying a
  `"_comment"`/`"description"` convention (the model already has `description` fields).
- **YAML reads nicely but is the riskiest for automated edits**: indentation
  sensitivity and implicit typing make it the easiest format for an LLM to break
  subtly. Use it only if human readability is paramount, and always validate.
- **TOML is excellent for small, flat config** (preferences, environment, recent
  folders) precisely because it is line-oriented and comment-friendly; it gets clumsy
  for the deeply nested optical-system tree.
- **Ignore the hype around new "LLM-native" formats (TOON/TRON, etc.).** They exist
  mainly to cut *token cost* on large tabular payloads, are brand-new (late 2025) with
  no editor/linting/schema tooling, and the optimization here is the wrong one: a
  saved optical project is small, and what matters for LLM *maintenance* is tooling,
  schema validation, and unambiguous edits — all of which favor boring, ubiquitous JSON.
- **The `.fsx`-as-data option is worth a serious look** given this codebase: optical
  systems are *already* expressed as F# values in the example scripts, the F# compiler
  validates them for free (the strongest possible "schema"), and an LLM is very good at
  writing F#. The cost is that the format is then code (executes, harder to sandbox,
  not trivially machine-rewritable field-by-field). A reasonable split is **F# scripts
  as a power-user/automation surface, JSON as the canonical saved-project format.**

### Suggested shape (not a design — just the shape)

- **Project/document files → JSON (with a published JSON Schema), comments via JSONC.**
- **Preferences/config → TOML** (or the same JSON+Schema to avoid a second parser).
- **Large numeric results/caches → a binary/columnar or SQLite sidecar**, referenced
  from the JSON, never inlined.
- Whatever is chosen, **ship a schema and validate on load** — that is what makes the
  data safely LLM-maintainable.

### F#/.NET libraries to back this (all open-source)

- **System.Text.Json** (built in) **+ [FSharp.SystemTextJson](https://github.com/Tarmil/FSharp.SystemTextJson)** —
  high-performance, makes DUs/records/options/`option` serialize idiomatically.
  Recommended primary.
- **[Thoth.Json](https://thoth-org.github.io/Thoth.Json/)** — explicit, type-safe
  encoders/decoders with excellent error messages; good when you want total control of
  the on-disk shape (and Fable-compatible if a web UI ever appears).
- **[Tomlyn](https://github.com/xoofx/Tomlyn)** — complete TOML 1.0 for .NET, if TOML
  is chosen for config.
- **[FSharp.Data](https://fsprojects.github.io/FSharp.Data/)** — JSON/CSV/XML type
  providers, handy for *importing* external measurement/material data.
- **JSON Schema validation**: `JsonSchema.Net` or `NJsonSchema`.

---

## 2. UI tech stack — Windows-only desktop

### Constraints & priorities

Windows-only (so cross-platform is a bonus, not a requirement); F#-first is strongly
preferred to keep one language end-to-end; the app is **scientific and interactive**
(live editing, parametric sweeps, lots of charts, eventually a 2D/3D viewport).

### Framework options

| Option | F# ergonomics | Maturity on Windows | Charts/Graphics | Verdict |
|---|---|---|---|---|
| **Avalonia + [Avalonia.FuncUI](https://github.com/AvaloniaCommunity/Avalonia.FuncUI)** | **Excellent** (native F# MVU/Elmish) | Very good; actively developed | ScottPlot/OxyPlot/LiveCharts2 all have Avalonia controls; WebView available | **Top pick for F#** |
| **WPF + [Elmish.WPF](https://github.com/elmish/Elmish.WPF)** | Good (MVU over XAML/MVVM) | **Highest** (20+ yrs, huge ecosystem) | ScottPlot, OxyPlot, LiveCharts2, **HelixToolkit (3D)**, WebView2 | **Strong Windows-native alternative** |
| **WinUI 3 / Windows App SDK** | Poor F# story | Newer, bumpy adoption | OK | Not recommended for F# |
| **.NET MAUI + [Fabulous](https://github.com/fabulous-dev/Fabulous)** | Good (F# MVU) | Aimed at mobile; desktop weaker | Limited | Overkill/mismatched |
| **WinForms** | OK | High but dated | ScottPlot/OxyPlot | Fine for a quick internal tool only |
| **Web-hybrid (Blazor Hybrid / [Photino](https://www.tryphotino.io/) / Electron)** | OK | High | **Plotly.NET renders natively (HTML)** | Good if you lean into Plotly/web charts |

### Charts (this matters because Plotly.NET is already in use)

- **Plotly.NET produces interactive HTML/JS**, so in a native desktop shell it needs a
  **WebView2** host. That is easy and keeps your existing chart code — but it is a
  web view inside a native app.
- **[ScottPlot](https://scottplot.net/)** is the strongest *native* option:
  open-source, fast with large datasets, interactive, F#-friendly, and ships controls
  for **WPF, WinForms, and Avalonia**. Best fit for live scientific plots.
- **[OxyPlot](https://oxyplot.github.io/)** (mature, lightweight) and
  **[LiveCharts2](https://livecharts.dev/)** (modern, animated) are solid alternatives.
- A pragmatic split: **ScottPlot for fast interactive 2D plots**, keep **Plotly.NET via
  WebView2** for publication-quality/exportable charts you already generate.
- 3D viewport (later): **HelixToolkit** (WPF) or OpenTK / Avalonia's 3D for a custom
  renderer.

### Recommendation

- **Primary: Avalonia + Avalonia.FuncUI + ScottPlot**, with **WebView2** to host any
  Plotly.NET output. Rationale: idiomatic F# MVU (matches the functional codebase),
  modern and actively maintained, Windows-first today but not a dead end, and a clean
  charting story. All components are open-source.
- **Conservative alternative: WPF + Elmish.WPF + ScottPlot (+ HelixToolkit, WebView2).**
  Choose this if you want the deepest, most battle-tested Windows tooling/designer
  support and the richest 3D option, and you accept XAML in the mix.
- **Web-hybrid (Photino/Blazor Hybrid)** is a legitimate third path *only if* you
  decide charts and UI should both be web tech (leveraging Plotly fully); otherwise it
  adds a web runtime for little gain on a Windows-only target.

---

## 3. Numerical optimization library — the hard question

### The actual requirement

For inverse fitting (fit R/T and ellipsometric Ψ/Δ to measured data) and design
synthesis, the workhorse is **nonlinear least-squares via Levenberg–Marquardt**, plus
**bounded/constrained** variants and a few **global / derivative-free** methods (for
multimodal merit landscapes and needle-style synthesis). Jacobians can start as finite
differences and move to analytic later.

### The user's thesis is largely correct

Hand-rewrites of venerable FORTRAN/C numerical code into C#/.NET have a poor track
record, and several .NET options are thin or abandoned (e.g. **Math.NET Numerics**
optimization is usable but limited — LM, Nelder–Mead, BFGS, CG — and **Accord.NET** is
effectively unmaintained). So **native interop** (as already done for ODE solvers) is a
sound default. **But** the landscape now has two genuine exceptions worth checking
*before* committing to interop.

> **Established FORTRAN interop pattern (`C:\GitHub\OdePackInterop`).** The user
> already has a proven, shipped template — the `Softellect.OdePackInterop` NuGet
> package wrapping the classic **FORTRAN ODEPACK** stiff-ODE solver (`DLSODE`). The
> pattern is directly reusable for an optimization library:
> - **Compile the FORTRAN/C source to a C-ABI DLL** with the **Intel FORTRAN compiler
>   (oneAPI)**, x64 / Release (`OdePack.dll`); a thin FORTRAN shim exports a clean
>   entry point (`DLSODEINTEROP`).
> - **P/Invoke from C#**: `[DllImport("OdePack.dll", CallingConvention = Cdecl,
>   EntryPoint = …)]`; FORTRAN passes everything by reference → `ref int`/`ref double`/
>   `double*`; arrays as `[In, Out] double[]`; callbacks (derivative/Jacobian) as
>   `[UnmanagedFunctionPointer(Cdecl)]` delegates; **pin arrays with `fixed`** so the
>   GC can't move them during the native call.
> - **Ship the runtime DLLs** alongside (`libifcoremd.dll`, `libmmd.dll`,
>   `vcruntime140.dll`) and pack them into the NuGet (`CopyToOutputDirectory` +
>   `PackageCopyToOutput`).
> - **Key lesson for F#:** calling the unsafe P/Invoke + delegate marshaling *directly*
>   from F# hit `InvalidProgramException`, so the repo keeps the **unsafe interop in a
>   small C# layer** (`OdeSolver.RunFSharp(...)`) that F# calls. Do the same here:
>   **C# owns the `DllImport`/`unsafe`/pinning; F# owns the domain and the
>   residual/objective function.**
> - They evaluated **SUNDIALS (a C port)** but a C++/CLI wrapper hit the known
>   *E0337 "Linkage specification is incompatible"* error — i.e. prefer **plain C-ABI
>   P/Invoke over C++/CLI shims**. This is exactly why **cminpack/NLopt (C ABI)** and
>   **MINPACK (FORTRAN via the same Intel toolchain)** are low-risk, whereas wrapping
>   **Ceres (C++)** would reintroduce the C++/CLI headache.
>
> **Implication:** since the user already owns the Intel-FORTRAN → DLL → P/Invoke →
> NuGet pipeline, wrapping **MINPACK (public-domain FORTRAN LM)** is arguably the
> *lowest-friction* native route *for this user specifically* — it reuses the exact
> build/runtime/packaging machinery already in `OdePackInterop`.

### Option A — Genuine .NET-native libraries (no interop)

- **[ALGLIB](https://www.alglib.net/) (C# edition).** The important nuance: ALGLIB's C#
  version is a **real, professionally maintained native implementation, not a bad
  port** — the same vendor ships C++ and C#. It includes LM (`lsfit`/`minlm`), L-BFGS,
  bound/linear/nonlinear-constrained solvers, and curve fitting with confidence
  intervals. **Free edition is GPL**; a commercial license is available. If GPL (or a
  purchase) is acceptable, this is the lowest-friction high-quality answer.
- **Extreme Optimization (`Extreme.Mathematics`) — already referenced in this repo.**
  It has nonlinear least squares and curve fitting. It is **commercial**, but if a
  license is already held, the fastest path is to *reuse what's there* before adding a
  dependency.
- **[Math.NET Numerics](https://numerics.mathdotnet.com/optimization)** (MIT) — fine
  for simple/local problems and zero-friction to add; likely insufficient alone for
  robust constrained synthesis.
- **[csnumerics](https://github.com/cureos/csnumerics)** (C# ports of L-BFGS-B, BOBYQA,
  LINCOA) — useful supplements; verify the license for your distribution.

### Option B — Native interop to public-domain / permissive C/C++/FORTRAN

This matches the user's ODE-solver precedent. Candidates, ranked for this use case:

| Library | Lang | Algorithms | License | Interop ease | Fit |
|---|---|---|---|---|---|
| **[cminpack](https://github.com/devernay/cminpack)** | C | LM (`lmder`/`lmdif`), hybrd | **BSD-like (permissive)** | **Easy** (clean C, P/Invoke) | **Best workhorse for LSQ fitting** |
| **[MINPACK](https://www.netlib.org/minpack/)** | FORTRAN | LM (the original) | **Public domain** | Moderate (FORTRAN ABI) | Gold standard; cminpack is the easier face of it |
| **[NLopt](https://nlopt.readthedocs.io/)** | C | Large suite: local+**global**, derivative-free (COBYLA, BOBYQA, NEWUOA, Nelder–Mead), DIRECT, CRS, ISRES, MMA, SLSQP | LGPL (+ MIT parts) | **Easy**; wrapper exists → **[NLoptNet](https://www.nuget.org/packages/NLoptNet)** | **Best for variety/global & bounded** |
| **[Ceres Solver](http://ceres-solver.org/)** | C++ | Large-scale NLS, bounds, **autodiff** | New BSD | **Hard** (C++ → needs C shim) | Powerful but heavyweight for this app |
| **[levmar](http://users.ics.forth.gr/~lourakis/levmar/)** | C/C++ | Box/constrained LM | **GPL** | Easy | Good algo, but GPL |
| **GSL** | C | multifit-nlinear, multimin | GPL | Moderate | Broad, but GPL |
| **Ipopt** | C++/FORTRAN | Large-scale constrained interior-point | EPL | Hard | Only if you need big constrained problems |

License note: **LGPL (NLopt) is fine for a closed app when dynamically linked via
P/Invoke** (which is exactly how interop works), since the library remains replaceable.
GPL libraries (levmar, GSL, ALGLIB-free) make the *whole app* GPL — acceptable for an
in-house/research tool, a real constraint for redistribution.

### Recommendation

1. **First, check the two no-interop escape hatches.** If the **Extreme Optimization**
   license already in the repo covers nonlinear LSQ/curve-fitting, prototype the fit
   there — zero new dependencies. Failing that, evaluate **ALGLIB C#** (if GPL or a
   purchase is acceptable); it is the rare .NET-native library that is actually good.
2. **If interop is preferred (per the ODE precedent), pick the least-squares workhorse
   + a global/constrained companion:**
   - **Least-squares fitting workhorse — `lmdif`/`lmder` (Levenberg–Marquardt).** Two
     equally good ways to get it:
     (a) **MINPACK (public-domain FORTRAN)** wrapped with the **exact `OdePackInterop`
     pipeline** the user already owns (Intel FORTRAN → DLL → P/Invoke → NuGet) — least
     *new* tooling for this user; or
     (b) **cminpack (BSD C)** — the same algorithm as clean C, even simpler P/Invoke,
     no FORTRAN toolchain needed.
     Either directly replaces the Wolfram `FindMinimum`/`NonlinearModelFit` fitting path
     (start with finite-difference Jacobian, add analytic later).
   - **NLopt (via NLoptNet or a thin P/Invoke)** for **global, derivative-free, and
     bounded/constrained** methods needed for design synthesis and escaping local minima.
   Together they cover the full §5 optimization spec from the feature list, all are
   clean C-ABI (matching the established pattern), and the combined licensing
   (public-domain/BSD + LGPL) is redistribution-friendly.
3. **Avoid** rewriting FORTRAN/C into managed code by hand (the user's exact concern),
   **avoid GPL-only** options (levmar, GSL) unless GPL for the whole product is
   acceptable, and **avoid C++/CLI shims** (the SUNDIALS E0337 trap) — reserve **Ceres**
   for later only if large-scale problems or autodiff justify a C-ABI shim around it.
4. **Practical interop tips** (carried over from `OdePackInterop`): keep the
   `DllImport`/`unsafe`/array-pinning in a **small C# layer** and expose an idiomatic
   F# residual/objective function (`float[] -> float[]`) above it (this sidesteps the
   F# `InvalidProgramException` they already hit); ship the native `.dll` plus its
   runtime dependencies per-RID (`x64`, Release); and unit-test against MINPACK's
   classic test problems plus a Wolfram-computed reference fit to confirm parity.

---

### Sources (web research)

**Established interop precedent (in-repo)**
- `C:\GitHub\OdePackInterop` — `Softellect.OdePackInterop` (FORTRAN ODEPACK/DLSODE
  wrapper); see its `Interop.cs`, `OdeSolver.cs`, `.csproj`, and `README.md` for the
  Intel-FORTRAN → DLL → P/Invoke → NuGet pattern reused in §3.
- [ODEPACK FORTRAN source (netlib)](https://www.netlib.org/odepack/) ·
  [FORTRAN interoperability with .NET (CodeProject)](https://www.codeproject.com/Articles/1065197/Introduction-to-FORTRAN-Interoperability-with-NET)

**Storage formats**
- [TOON vs JSON vs YAML vs CSV for LLM apps](https://www.piotr-sikora.com/blog/2025-11-29-toon-format-comparison-csv-json-yaml)
- [TOML vs YAML vs JSON comparison (Haihai)](https://www.haihai.ai/toml-vs-yaml-vs-json/)
- [JSON vs YAML vs TOML — choosing a config format (dev.to)](https://dev.to/biswajitpanday/json-vs-yaml-vs-toml-choosing-the-right-configuration-format-5h8h)
- [YAML vs JSON efficiency for language models](https://medium.com/better-programming/yaml-vs-json-which-is-more-efficient-for-language-models-5bc11dc0f054)
- [FSharp.SystemTextJson](https://github.com/Tarmil/FSharp.SystemTextJson), [Thoth.Json](https://thoth-org.github.io/Thoth.Json/), [Tomlyn](https://github.com/xoofx/Tomlyn), [FSharp.Data](https://fsprojects.github.io/FSharp.Data/)

**UI**
- [Avalonia.FuncUI](https://github.com/AvaloniaCommunity/Avalonia.FuncUI) · [FuncUI docs](https://funcui.avaloniaui.net/) · [F# desktop apps (fsharp.org)](https://fsharp.org/use/desktop-apps/)
- [Fabulous](https://github.com/fabulous-dev/Fabulous)
- [ScottPlot](https://scottplot.net/) · [OxyPlot](https://oxyplot.github.io/) · [LiveCharts2](https://livecharts.dev/) · [Plotly.NET](https://plotly.net/)

**Optimization**
- [Math.NET Numerics optimization](https://numerics.mathdotnet.com/optimization)
- [ALGLIB (C#/C++ numerical library)](https://www.alglib.net/)
- [cminpack](https://github.com/devernay/cminpack) · [MINPACK (netlib)](https://www.netlib.org/minpack/)
- [NLopt](https://nlopt.readthedocs.io/) · [NLopt algorithms](https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/) · [NLoptNet (NuGet)](https://www.nuget.org/packages/NLoptNet)
- [Ceres Solver](http://ceres-solver.org/) · [levmar](http://users.ics.forth.gr/~lourakis/levmar/) · [csnumerics](https://github.com/cureos/csnumerics)

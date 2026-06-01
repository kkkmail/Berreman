# Resolver decision -- 001.slice-md

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0024\.slices\001.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0024\.slices\001-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0024\.slices\001-impl-log.md`
- Judge MD: `C:\GitHub\Berreman\specs\0024\.artifacts\code_judge\001-03-code-judge.md`
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0024\.artifacts\architecture_critic\001-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0024\.artifacts\reuse_critic\001-02-reuse-critic.md`
- Verified against code: `OpticalConstructor.Ui.Tests/SmokeTests.fs`,
  `OpticalConstructor.Ui.Tests/TestApp.fs`,
  `OpticalConstructor.App/Program.fs`
- Escalation reason: `too many failures (2/2): code-judge route-back-to-worker on cycle 2` (category **failures-cap**)

## Diagnosis

The cause is concrete and I confirmed it against the code, not just the artefacts.
AC-U1.3 and §U1.7 are explicit that the `ui-smoke` test MUST "assert the window opens
and `OnFrameworkInitializationCompleted` ran (Program.fs:50)" and exercise the
`Initialize`/`AppShell.themeVariant` theme seam. The smoke test does neither:
`SmokeTests.fs:39` constructs `MainWindow()` directly and shows it, so the real `App`
application lifetime is never driven — `App.OnFrameworkInitializationCompleted`
(`Program.fs:56`) and `App.Initialize`'s theme seam `AppShell.themeVariant`
(`Program.fs:52-54`) do not execute. The headless `TestApp` adds `FluentTheme()` itself
(`TestApp.fs:27`), bypassing the one mapping the spec calls "the only theme seam." The
render-one-frame half of AC-U1.3 is genuinely met; the lifetime/theme half — a named,
binding acceptance criterion on a slice the spec marks CRITICAL and gating — is only
approximated and exercised by no gate. The judge is correct on the narrow legal point; the
architecture critic's "most defensible headless reading … ship it" is a judgment call to
relax a literally-stated AC, which is an operator/spec decision, not the worker's licence.

This is a strong `issue-hint` case rather than a stuck-worker escalation. The failures-cap
(2/2) was not reached by the worker oscillating on one problem: cycle 1 blocked on the
reuse/wavelength gap, which the worker then fixed **cleanly** in attempt 02 via the
`Templates.bandpassFilter ()` seed (both critics confirm it resolved). Cycle 2 surfaced a
**different, fresh** blocker — the smoke-test lifetime gap — that the worker has never been
directed at. The cap is an artefact of two sequential single-findings against a budget of
two, not a worker who keeps fixing the same thing the wrong way. A third attempt aimed
precisely at the one remaining named AC is highly likely to land: the worker is demonstrably
competent, the surface is otherwise green (all seven gates pass; both critics lean ship; the
only judge blocker is this one item), and the worker can verify locally against the still-live
`ui-smoke`/`ui-tests` gates until green.

The fix is small and bounded. It is test-only: it touches no frozen core module (§0.1), adds
no package (`Avalonia.Headless` already exposes the lifetime-setup API), and requires no new
design decision or scope change. The path is well-known Avalonia.Headless territory — build
the real `App` over the headless platform and trigger framework-init via a non-blocking
`SetupWithLifetime` on a `ClassicDesktopStyleApplicationLifetime`, or point the existing
`TestAppBuilder` at the real `App` so its `Initialize` theme seam runs through the session —
confined to `SmokeTests.fs` and `TestApp.fs` (two files, within the cap). The judge's own
read agrees this is "one bounded re-spawn away … the single most impactful change." The
spec-mandated dual `construction`/`workspace` project seed and the inherited stale
`ui-tests.gates` description are explicitly out of scope and must be left alone so the worker
does not over-correct. My confidence in both the diagnosis and the unblock is high.

## Verdict

issue-hint

## Hint

In `OpticalConstructor.Ui.Tests/SmokeTests.fs`, replace the direct `MainWindow()`
construction in the `ui-smoke` test with a path that drives the REAL `App` lifetime
headlessly — build `AppBuilder.Configure<App>().UseHeadless(AvaloniaHeadlessPlatformOptions())`
and trigger framework-init via a non-blocking `SetupWithLifetime` on a
`ClassicDesktopStyleApplicationLifetime`, then assert `App.OnFrameworkInitializationCompleted`
actually ran (e.g. the desktop lifetime's `MainWindow` is set and visible) AND that the
`App.Initialize` theme seam (`AppShell.themeVariant`, `Program.fs:52-54`) executed, per
AC-U1.3 / §U1.7. If the existing `HeadlessUnitTestSession` app instance conflicts, point
`TestApp.fs`'s `TestAppBuilder.BuildAvaloniaApp` at the real `App` so its `Initialize` runs
the theme seam, and run the lifetime setup inside `HeadlessSession.run`, keeping every
per-panel/page render-one-frame assertion and all gates green. Edit only `SmokeTests.fs` and
`TestApp.fs`, do NOT touch the spec-mandated dual construction/workspace project seed
(R-2/§0.1) or the inherited `ui-tests.gates` description, and note the change in the impl-log.

```json
{"verdict": "issue-hint", "rationale": "Confirmed against the code: the only judge blocker is a single stated, binding AC of this CRITICAL gating slice. AC-U1.3/§U1.7 require the ui-smoke test to assert App.OnFrameworkInitializationCompleted ran (Program.fs:56) and exercise the Initialize/AppShell.themeVariant theme seam (Program.fs:52-54), but SmokeTests.fs:39 constructs MainWindow() directly and never drives the real App lifetime, and TestApp.fs:27 adds FluentTheme() itself, bypassing that seam. The failures-cap (2/2) was reached by two DIFFERENT sequential findings -- cycle 1's reuse/wavelength gap, which the worker fixed cleanly in attempt 02, then this fresh cycle-2 lifetime gap the worker has never been directed at -- not by a stuck worker. The fix is test-only, touches no frozen module, adds no package, stays within 2 files (SmokeTests.fs, TestApp.fs), and uses well-known Avalonia.Headless SetupWithLifetime machinery, with live ui-smoke/ui-tests gates to verify. A first-time, precisely-targeted hint to a demonstrably competent worker on an otherwise-green surface (all gates pass, both critics lean ship) is highly likely to unblock the slice in one more try.", "operator_reply_text": "In OpticalConstructor.Ui.Tests/SmokeTests.fs, replace the direct MainWindow() construction in the ui-smoke test with a path that drives the REAL App lifetime headlessly -- build AppBuilder.Configure<App>().UseHeadless(AvaloniaHeadlessPlatformOptions()) and trigger framework-init via a non-blocking SetupWithLifetime on a ClassicDesktopStyleApplicationLifetime, then assert App.OnFrameworkInitializationCompleted actually ran (e.g. the desktop lifetime's MainWindow is set and visible) AND that the App.Initialize theme seam (AppShell.themeVariant, Program.fs:52-54) executed, per AC-U1.3 / §U1.7. If the existing HeadlessUnitTestSession app instance conflicts, point TestApp.fs's TestAppBuilder.BuildAvaloniaApp at the real App so its Initialize runs the theme seam, and run the lifetime setup inside HeadlessSession.run, keeping every per-panel/page render-one-frame assertion and all gates green. Edit only SmokeTests.fs and TestApp.fs, do NOT touch the spec-mandated dual construction/workspace project seed (R-2/§0.1) or the inherited ui-tests.gates description, and note the change in the impl-log.", "confidence": "high"}
```

{
  "claims": [
    {
      "id": 1,
      "phrase": "AC‑D4: The *Legacy* entry opens the existing dockable Construction / Synthesis‑Fit screen (D.5); the constructor is the default landing page (D.6).",
      "spec_location": "Acceptance criteria § Part D acceptance, AC‑D4 (chunk 6, lines 999-1132)",
      "evidence": "Class-anchored on the named screen (no file:symbol pointer in sentence; (D.5) is a spec-section ref). Resolves uniquely in Berreman/OpticalConstructor/OpticalConstructor.Ui/: Shell.fs defines Page.Construction / Page.SynthesisFit (lines 74-75), carries the sub-models (construction : ConstructionPage.Model line 100, fit : SynthesisFitPage.Model option line 103), and routes docking (SetPanelDock of panel : string * dock : DockSide, line 83). Backing modules ConstructionPage.fs, ConstructionView.fs, SynthesisFitPage.fs and FitView.fs all exist. The existing dockable Construction / Synthesis-Fit screen resolves to real artefacts.",
      "verdict": "CONFIRMED"
    }
  ]
}

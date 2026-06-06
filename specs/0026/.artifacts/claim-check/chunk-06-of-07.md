{
  "claims": [
    {
      "id": 1,
      "phrase": "AC‑D4: The *Legacy* entry opens the existing dockable Construction / Synthesis‑Fit screen (D.5); the constructor is the default landing page (D.6).",
      "spec_location": "Acceptance criteria § Part D acceptance, AC‑D4 (chunk 6, lines 999-1132)",
      "evidence": "Class-anchored on the named screen (no file pointer in sentence; (D.5) is a spec-section ref). Glob resolves both named artefacts: Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructionPage.fs (module OpticalConstructor.Ui.ConstructionPage) and Berreman/OpticalConstructor/OpticalConstructor.Ui/SynthesisFitPage.fs (module OpticalConstructor.Ui.SynthesisFitPage); sibling ConstructionView.fs and FitView.fs also present. The existing Construction / Synthesis-Fit screen resolves to real modules.",
      "verdict": "CONFIRMED"
    }
  ]
}

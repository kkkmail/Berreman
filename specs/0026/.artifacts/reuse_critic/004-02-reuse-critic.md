# Reuse critique -- 004.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (the `OpticalConstructor` subtree:
  `.Domain`, `.Ui`, `.Ui.Tests`, `.Storage`).
- Files inspected: ~14 read in full (the 5 new slice files + `Schematic.fs`,
  `Placement.fs`, `SourceView.fs`, `Project.fs`, the schema, the SoW) plus a
  grep catalogue across the ~95 `.fs` files of the subtree; well under the 200 cap.
- Extensions: the configured `.py,.md,.json` bounds do not fit this pure-F# repo
  (zero `.py`; every helper is `.fs`), so I walked `.fs` — where reuse lives —
  plus the `.json` schema and the `.md` spec/SoW. (Defensible-interpretation note
  per the read-order contract.)

## Findings

### F1: The standardized toggle abandons the established `Brushes.SteelBlue` active colour while claiming to match it

- **Worker added:** `Controls.accentColor = rgb 33 118 174` (`Controls.fs:68`) and
  `Controls.toggleOnColor = accentColor` (`Controls.fs:103`), used as the "on" fill
  in `Controls.toggle` (`Controls.fs:136`).
- **Existing helper:** the active-toggle idiom `Button.background (if active then
  Brushes.SteelBlue else Brushes.Transparent)`, repeated verbatim at
  `SourceView.fs:108`, `MaterialsView.fs:147`, `ResultsView.fs:189` and `:194`, and
  `Shell.fs:504`. `Brushes.SteelBlue` is the named colour `#4682B4` = (70, 130, 180).
- **Why it matters:** `Controls.fs` is mandated (J.1.1) as the single source of the
  toggle flavour, so every existing SteelBlue toggle is expected to converge on
  `Controls.toggle` in slice 005/006. But the new constant is `#2176AE` =
  (33, 118, 174) — a *different* blue — even though the surrounding doc-comments
  assert it is "a steel blue, matching the active-toggle colour the existing views
  use (`Button.background Brushes.SteelBlue`)" (`Controls.fs:67-68`, `:101`). The
  comment and the value disagree; the convergence the spec asks for will silently
  shift the highlight hue across four existing panels.
- **Suggested action:** either set `accentColor` (or a dedicated `toggleOnColor`) to
  the actual SteelBlue (70, 130, 180) so the unification is colour-preserving, or
  correct the doc-comments to state the colour is deliberately new. This is a pure
  in-file change touching no frozen module.

### F2: `Controls.toBrush` re-implements the existing `SchematicColor -> IBrush` converter

- **Worker added:** `Controls.toBrush (c : SchematicColor) : IBrush =
  ImmutableSolidColorBrush(Color.FromRgb(c.red, c.green, c.blue))` (`Controls.fs:37-38`).
- **Existing helper:** `ResultsView.brushOf (c : Schematic.SchematicColor) : IBrush =
  SolidColorBrush(Color.FromRgb(c.red, c.green, c.blue))` (`ResultsView.fs:83-84`).
- **Why it matters:** same job (map the pure `SchematicColor` to an Avalonia solid
  brush through `Color.FromRgb`), differing only in mutable vs immutable brush. The
  worker's immutable variant is in fact the *better* one (the Gotchas log explains
  the module-init thread-affinity crash a mutable brush causes), so there are now two
  divergent `SchematicColor`→brush converters in the same `.Ui` namespace, the older
  of which carries the latent thread bug. As more views adopt `SchematicColor`
  styling this fork will be copied again.
- **Suggested action:** lift one shared converter (the immutable form) to a place both
  can call — e.g. a public helper next to `SchematicColor` in `Schematic.fs`, or have
  `ResultsView.brushOf` delegate to `Controls.toBrush`. `ResultsView` is out of this
  slice's scope, so this may be left as-is and noted; the judge decides.

### F3: The private `rgb` constructor is now triplicated

- **Worker added:** `let private rgb (r:int) (g:int) (b:int) : SchematicColor =
  { red = byte r; green = byte g; blue = byte b }` in BOTH `Drawer.fs:29` and
  `ConstructorTable.fs:43` — byte-for-byte identical.
- **Existing helper:** the same private `rgb` in `Schematic.fs:45`.
- **Why it matters:** this is direct duplication of a trivial-but-real helper, and the
  slice adds two fresh copies rather than one. All three live in `OpticalConstructor.Ui`
  and build the same `SchematicColor`. The cost is small per copy but it is the exact
  "reinvent a helper that already exists" pattern the reuse gate exists to catch, and
  it compounds (every future colour-defining `.Ui` module will copy it again).
- **Suggested action:** promote `Schematic.rgb` to a public binding (it sits beside the
  `SchematicColor` type it constructs) and have `Drawer.fs` / `ConstructorTable.fs`
  reuse it; drop the two private copies.

### F4: `Controls.numericWithUnits` near-duplicates `SourceView.numberField`

- **Worker added:** `Controls.numericWithUnits` (`Controls.fs:144-153`) — a horizontal
  `StackPanel` of a label `TextBlock`, a `TextBox.width 120.0`, and a trailing unit
  `TextBlock`.
- **Existing helper:** `SourceView.numberField` (`SourceView.fs:89-101`) — the same
  horizontal `StackPanel` of a label `TextBlock` + a `TextBox.width 120.0`.
- **Why it matters:** the layouts are the same "label / 120-wide entry" row; the
  worker's doc-comment itself says it "Mirrors the existing `SourceView.numberField`
  shape" (`Controls.fs:142`). The contracts differ (the new one appends a unit symbol
  and hands raw text to the caller; `numberField` parses on change via `tryFloat`), so
  they are not drop-in interchangeable — this is a near-miss, not a clone.
- **Suggested action:** acknowledged tension. J.1.1 mandates the flavour live in
  `Controls.fs`, and `SourceView` is frozen spec-0024 code, so "reuse as-is" is not
  available; the correct long-run move is for `numberField` to become a thin wrapper
  over the `Controls` flavour. Reasonable to leave for now and document the intentional
  parallel; flagged so the duplication is visible, not silent.

### F5: Vector add/scale are added as private free functions instead of extending `Placement.Vector3`

- **Worker added:** `Drawer.addv` and `Drawer.scalev` (`Drawer.fs:31-32`) — element-wise
  add and scalar-multiply over `Placement.Vector3`.
- **Existing helper:** `Placement.Vector3` already hosts its arithmetic AS TYPE MEMBERS
  — `dot`, `cross`, `norm`, `normalized` (`Placement.fs:34-47`) — and `Placement.rotateAbout`
  (`Placement.fs:239-249`) is the established consumer of that vocabulary.
- **Why it matters:** add and scale are the natural siblings of `dot`/`cross`, and the
  project's convention is to put `Vector3` arithmetic on the type. Placing them as
  private free functions in a UI drawer diverges from that pattern and hides reusable
  vector math where the next geometry module (slice 005's ray layer) cannot reach it,
  inviting a third re-definition.
- **Suggested action:** add `+` / scalar-`*` (or `add` / `scale`) members to
  `Placement.Vector3` beside `dot`/`cross` and call them from the drawer. Touches the
  slice-001 domain file, so weigh against the "keep placement anchors stable" risk note.

## Bottom line

The diff is disciplined on the big reuse mandates — it correctly reuses
`Placement.orientedBasis`, `ElementPlacement.create`, `TablePoint`, `RayModel`, the
`SchematicColor` value shape, and the units spine, and it does not fork the engine or
the schema. The findings are the small-helper layer: a triplicated `rgb` (F3) and a
toggle colour that silently departs from the repo-wide `Brushes.SteelBlue` while
claiming to match it (F1) are the two most actionable and both fixable without touching
frozen code; F2/F4/F5 are near-misses against out-of-scope or slice-001 modules where
"surface, don't necessarily re-route" is the honest call. None of this binds a
re-spawn — the code judge decides routing.

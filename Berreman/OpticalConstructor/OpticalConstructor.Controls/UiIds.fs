namespace OpticalConstructor.Controls

/// Spec 0038 (step 044): the ONE home for every automation-id literal. Before this step each
/// control / view / window carried its own nested `UiIds` module; those are all retired and their
/// constants gathered here, unchanged in value, under one `[<RequireQualifiedAccess>] module UiIds`
/// with a nested per-surface sub-module. Control sites and tests address an id as
/// `UiIds.<Surface>.<member>`. Fixed ids are `[<Literal>]` (so a test may cite them in an
/// `InlineData` attribute); parametric, host-code-derived ids stay functions.
///
/// This module is compiled FIRST in `OpticalConstructor.Controls` so every downstream control,
/// view and window (and every test) resolves it. It is deliberately DOMAIN-FREE — the Controls
/// project references no Domain types — so the two window views' Domain-typed id helpers
/// (`LibraryWindowView.versionRow`/`entryNode`, `MaterialsWindowView.versionRow`/`entryNode`) stay
/// as plain functions beside their views rather than moving here; every literal they carried does.
[<RequireQualifiedAccess>]
module UiIds =

    // ---- Controls surfaces -------------------------------------------------------------------

    /// The reusable rotation-controls bar (`RotationControls`).
    module Rotation =
        [<Literal>]
        let r1Minus = "RotationR1MinusButton"
        [<Literal>]
        let r1Plus = "RotationR1PlusButton"
        [<Literal>]
        let r2Minus = "RotationR2MinusButton"
        [<Literal>]
        let r2Plus = "RotationR2PlusButton"
        [<Literal>]
        let r3Minus = "RotationR3MinusButton"
        [<Literal>]
        let r3Plus = "RotationR3PlusButton"
        [<Literal>]
        let r1Field = "RotationR1Field"
        [<Literal>]
        let r2Field = "RotationR2Field"
        [<Literal>]
        let r3Field = "RotationR3Field"
        [<Literal>]
        let lockR3 = "RotationLockR3Button"
        // The two reset buttons keep these stable ids in BOTH modes: "Reset"/"Reset All" when idle,
        // and the confirmation "Yes"/"No" when a reset is armed (the label/action changes, not the id).
        [<Literal>]
        let reset = "RotationResetButton"
        [<Literal>]
        let resetAll = "RotationResetAllButton"

    /// The element-palette bar (`ElementPaletteControls`).
    module ElementPalette =
        [<Literal>]
        let removeSelected = "PaletteRemoveSelectedButton"
        /// The add button id for an entry — the entry id, prefixed so it cannot collide with other ids.
        let addButton (entryId : string) : string = "PaletteAdd_" + entryId

    /// The position-on-the-ray bar (`RayPositionControls`).
    module RayPosition =
        [<Literal>]
        let minus = "RayPositionMinusButton"
        [<Literal>]
        let plus = "RayPositionPlusButton"
        [<Literal>]
        let field = "RayPositionField"
        [<Literal>]
        let reset = "RayPositionResetButton"
        [<Literal>]
        let readout = "RayPositionReadout"

    /// The renderer "large control" (`RendererControls`).
    module Renderer =
        [<Literal>]
        let swapRenderer = "RendererSwapButton"
        [<Literal>]
        let readout = "RendererReadout"
        [<Literal>]
        let railsSlider = "RendererRailsSlider"
        [<Literal>]
        let capCirclesSlider = "RendererCapCirclesSlider"
        [<Literal>]
        let capRadialsSlider = "RendererCapRadialsSlider"
        [<Literal>]
        let railOpacitySlider = "RendererRailOpacitySlider"
        [<Literal>]
        let faceOpacitySlider = "RendererFaceOpacitySlider"
        [<Literal>]
        let lineOpacitySlider = "RendererLineOpacitySlider"

    /// The main-screen ribbon of bays (`Ribbon`).
    module Ribbon =
        /// The selectable tab for a bay — its name, prefixed so it cannot collide with other ids.
        let tab (name : string) : string = "RibbonTab_" + name
        /// The single active content slot — named by the bay currently shown.
        let pane (name : string) : string = "RibbonPane_" + name

    /// The kind-constrained Library bay list/tree (`LibraryControls`).
    module Library =
        [<Literal>]
        let readout = "LibraryBoundReadout"
        [<Literal>]
        let kindLabel = "LibraryKindLabel"
        [<Literal>]
        let tree = "LibraryTree"
        /// A leaf row's clickable id — the entry id, prefixed so it cannot collide with other ids.
        let entry (entryId : string) : string = "LibraryEntry_" + entryId
        /// The pending-entry full-description text (shown before confirm).
        [<Literal>]
        let description = "LibraryEntryDescription"
        /// The Confirm / Cancel actions of the pending bind.
        [<Literal>]
        let confirm = "LibraryConfirmButton"
        [<Literal>]
        let cancel = "LibraryCancelButton"

    /// The layer-stack band view / Details bay (`LayerBandsControls`).
    module LayerBands =
        [<Literal>]
        let stack = "LayerBandsStack"
        [<Literal>]
        let title = "LayerBandsTitle"
        /// The i-th band's drawn rectangle (0-based, top-to-bottom in band order).
        let band (i : int) : string = "LayerBand_" + string i

    /// The Experiments bay (`ExperimentControls`).
    module Experiment =
        [<Literal>]
        let readout = "ExperimentReadout"
        [<Literal>]
        let candidates = "ExperimentCandidates"
        /// A candidate element's clickable id — the element id, prefixed so it cannot collide.
        let candidate (elementId : string) : string = "ExperimentCandidate_" + elementId
        /// A variable-choice selector, by its code ("wavelength" / "r1" / "r2").
        let variable (code : string) : string = "ExperimentVariable_" + code
        /// A measurement-choice selector, by its code ("t" / "r" / "both").
        let measurement (code : string) : string = "ExperimentMeasurement_" + code
        /// The range inputs.
        [<Literal>]
        let rangeMin = "ExperimentRangeMin"
        [<Literal>]
        let rangeMax = "ExperimentRangeMax"
        [<Literal>]
        let rangePoints = "ExperimentRangePoints"
        /// The Add / Update and New actions.
        [<Literal>]
        let addButton = "ExperimentAddButton"
        [<Literal>]
        let newButton = "ExperimentNewButton"
        /// The collection list and a row's Edit / Remove actions.
        [<Literal>]
        let collection = "ExperimentCollection"
        let editButton (id : string) : string = "ExperimentEdit_" + id
        let removeButton (id : string) : string = "ExperimentRemove_" + id
        /// A row's "View" action — open the pop-out chart window for that experiment.
        let viewButton (id : string) : string = "ExperimentView_" + id
        /// The explicit "open the pop-out chart window" action (also opened by double-clicking the chart).
        [<Literal>]
        let openChart = "ExperimentOpenChart"
        /// The inline result polyline (the first series — the intensity / Ψ curve).
        [<Literal>]
        let chart = "ExperimentChart"
        /// The ellipsometer Ψ/Δ single-point readout text.
        [<Literal>]
        let psiDelta = "EllipsometerReadout"
        /// The chart description text shown under the inline chart.
        [<Literal>]
        let description = "ExperimentChartDescription"

    /// The materials workbench list surface (`MaterialsControls`).
    module Materials =
        [<Literal>]
        let searchBox = "MaterialSearchBox"
        [<Literal>]
        let categoryFilter = "MaterialCategoryFilter"
        [<Literal>]
        let dispersionFilter = "MaterialDispersionFilter"
        [<Literal>]
        let list = "MaterialsList"
        [<Literal>]
        let addButton = "AddMaterialButton"
        [<Literal>]
        let editButton = "EditMaterialButton"
        [<Literal>]
        let removeButton = "RemoveMaterialButton"
        [<Literal>]
        let viewButton = "ViewMaterialButton"
        /// A listed row's clickable id — the material id, prefixed so it cannot collide.
        let row (materialId : string) : string = "MaterialRow_" + materialId
        /// A category facet option's clickable id, by its host-supplied code.
        let categoryOption (code : string) : string = "MaterialCategoryOption_" + code
        /// A dispersion facet option's clickable id, by its host-supplied code.
        let dispersionOption (code : string) : string = "MaterialDispersionOption_" + code

    /// The samples workbench list surface (`SampleLibraryControls`).
    module SampleLibrary =
        [<Literal>]
        let searchBox = "SampleSearchBox"
        [<Literal>]
        let substrateFilter = "SampleSubstrateFilter"
        [<Literal>]
        let list = "SamplesList"
        [<Literal>]
        let addButton = "AddSampleButton"
        [<Literal>]
        let editButton = "EditSampleButton"
        [<Literal>]
        let removeButton = "RemoveSampleButton"
        [<Literal>]
        let viewButton = "ViewSampleButton"
        [<Literal>]
        let makeMultilayerButton = "MakeMultilayerButton"
        /// A listed row's clickable id — the sample id, prefixed so it cannot collide.
        let row (sampleId : string) : string = "SampleRow_" + sampleId
        /// A substrate facet option's clickable id, by its host-supplied code.
        let substrateOption (code : string) : string = "SampleSubstrateOption_" + code

    /// The Category-manager list surface (`CategoryControls`).
    module Category =
        [<Literal>]
        let list = "CategoriesList"
        [<Literal>]
        let addButton = "AddCategoryButton"
        [<Literal>]
        let nameBox = "CategoryNameBox"
        [<Literal>]
        let removeButton = "RemoveCategoryButton"
        [<Literal>]
        let saveButton = "CategorySaveButton"
        [<Literal>]
        let cancelButton = "CategoryCancelButton"
        [<Literal>]
        let blockMessage = "CategoryBlockMessage"
        /// A row's inline name box id — the base id prefixed with the row's category id.
        let rowNameBox (categoryId : string) : string = nameBox + "_" + categoryId
        /// A row's Save (rename) button id.
        let rowSaveButton (categoryId : string) : string = saveButton + "_" + categoryId
        /// A row's Remove button id (present only for a non-built-in row).
        let rowRemoveButton (categoryId : string) : string = removeButton + "_" + categoryId
        /// A row's Cancel button id.
        let rowCancelButton (categoryId : string) : string = cancelButton + "_" + categoryId

    /// The domain-free faceted, filterable tree (`FacetedTreeControls`).
    module FacetedTree =
        [<Literal>]
        let filterBox = "FacetFilterBox"
        [<Literal>]
        let representationPicker = "FacetRepresentationPicker"
        [<Literal>]
        let breadcrumbStrip = "FacetBreadcrumbStrip"
        [<Literal>]
        let resultCount = "FacetResultCount"
        [<Literal>]
        let offersPanel = "FacetOffersPanel"
        [<Literal>]
        let showTreeButton = "FacetShowTreeButton"
        [<Literal>]
        let tree = "FacetTree"
        [<Literal>]
        let scrollViewer = "FacetScroll"
        /// A representation option's clickable id, by its code.
        let representationOption (code : string) : string = "FacetRepresentationOption_" + code
        /// A breadcrumb chip's clickable id, by its code.
        let breadcrumbChip (code : string) : string = "FacetBreadcrumbChip_" + code
        /// An offer group's container id, by its code.
        let offerGroup (code : string) : string = "FacetOfferGroup_" + code
        /// An offered value's clickable id: group code THEN value code (the same discrete key can
        /// appear under two facets, so the group is part of the identity).
        let offeredValue (groupCode : string) (valueCode : string) : string =
            "FacetOfferedValue_" + groupCode + "_" + valueCode
        /// A numeric offer group's manual min–max box id, by the group's code.
        let manualRangeBox (groupCode : string) : string = "FacetManualRangeBox_" + groupCode
        /// A tree node row's clickable id, by the node's (tree-unique) code.
        let treeNode (code : string) : string = "FacetTreeNode_" + code
        /// A tree node's disclosure-chevron id (spec 0040 step 002) — present ONLY for a parent
        /// (a node with children); a leaf renders none. A distinct prefix from `treeNode` so the
        /// chevron is never miscounted as a label row.
        let treeNodeChevron (code : string) : string = "FacetTreeChevron_" + code

    /// The pop-out ScottPlot chart window's controls (`ChartWindow`, formerly `ChartWindowIds`).
    module ChartWindow =
        [<Literal>]
        let plot = "ChartWindowPlot"
        /// Spec 030: the element picker (which part of the chart is being formatted) and the properties panel.
        [<Literal>]
        let elementSelector = "ChartWindowElement"
        [<Literal>]
        let propertiesPanel = "ChartWindowProperties"
        /// The selected element's font stepper + readout (shown for the header / axes / legend).
        [<Literal>]
        let fontMinus = "ChartWindowFontMinus"
        [<Literal>]
        let fontPlus = "ChartWindowFontPlus"
        [<Literal>]
        let fontSize = "ChartWindowFontSize"
        /// Axis property controls.
        [<Literal>]
        let axisAuto = "ChartWindowAxisAuto"
        [<Literal>]
        let axisMin = "ChartWindowAxisMin"
        [<Literal>]
        let axisMax = "ChartWindowAxisMax"
        [<Literal>]
        let axisFormat = "ChartWindowAxisFormat"
        [<Literal>]
        let axisDecimalsMinus = "ChartWindowAxisDecimalsMinus"
        [<Literal>]
        let axisDecimalsPlus = "ChartWindowAxisDecimalsPlus"
        [<Literal>]
        let axisDecimals = "ChartWindowAxisDecimals"
        /// Legend property controls.
        [<Literal>]
        let legendVisible = "ChartWindowLegendVisible"
        [<Literal>]
        let legendPlacement = "ChartWindowLegendPlacement"
        /// Series (chart line) property controls.
        [<Literal>]
        let seriesVisible = "ChartWindowSeriesVisible"
        [<Literal>]
        let seriesThicknessMinus = "ChartWindowSeriesThickMinus"
        [<Literal>]
        let seriesThicknessPlus = "ChartWindowSeriesThickPlus"
        [<Literal>]
        let seriesThickness = "ChartWindowSeriesThick"
        [<Literal>]
        let seriesColor = "ChartWindowSeriesColor"
        [<Literal>]
        let seriesMarkers = "ChartWindowSeriesMarkers"
        /// The series' Y-axis side picker (left / right — spec 0033/018).
        [<Literal>]
        let seriesAxis = "ChartWindowSeriesAxis"
        /// The polar ⇄ XY toggle (only for angular charts).
        [<Literal>]
        let polarToggle = "ChartWindowPolar"
        [<Literal>]
        let majorGrid = "ChartWindowMajorGrid"
        [<Literal>]
        let minorGrid = "ChartWindowMinorGrid"
        [<Literal>]
        let exportPng = "ChartWindowExportPng"
        [<Literal>]
        let exportCsv = "ChartWindowExportCsv"
        [<Literal>]
        let description = "ChartWindowDescription"

    // ---- Ui view / window surfaces -----------------------------------------------------------

    /// The SolverHandoffWindow terminal summary (`SolverHandoffView`).
    module Handoff =
        [<Literal>]
        let window = "SolverHandoffWindow"
        [<Literal>]
        let collectionName = "SolverHandoffCollectionName"
        [<Literal>]
        let solverMessage = "SolverHandoffSolverMessage"
        [<Literal>]
        let summaryList = "SolverHandoffSummaryList"
        [<Literal>]
        let closeButton = "SolverHandoffCloseButton"
        /// A collected experiment's summary block, by its id (prefixed so it cannot collide).
        let summaryRow (experimentId : int) : string = "SolverHandoffRow_" + string experimentId
        /// A collected experiment's TYPED validation-status line, by its id.
        let statusRow (experimentId : int) : string = "SolverHandoffStatus_" + string experimentId

    /// The combined table + element rotation scene (`TableAndElementRotationView`).
    module TableAndElementRotation =
        [<Literal>]
        let canvas = "TableElementCanvas"
        [<Literal>]
        let rotateR1Minus = "TeRotateR1MinusButton"
        [<Literal>]
        let rotateR1Plus = "TeRotateR1PlusButton"
        [<Literal>]
        let rotateR2Minus = "TeRotateR2MinusButton"
        [<Literal>]
        let rotateR2Plus = "TeRotateR2PlusButton"
        [<Literal>]
        let rotateR3Minus = "TeRotateR3MinusButton"
        [<Literal>]
        let rotateR3Plus = "TeRotateR3PlusButton"
        [<Literal>]
        let unlockR3 = "TeUnlockR3Button"
        [<Literal>]
        let reset = "TeResetButton"
        [<Literal>]
        let readout = "TeReadout"
        /// Spec 0038 (031): the out-of-band dispersion warning badge on scene element index `i` — an
        /// indexed id so a headless test addresses the per-element badge.
        [<Literal>]
        let badgePrefix = "OutOfBandBadge_"
        let outOfBandBadge (i : int) : string = $"%s{badgePrefix}%d{i}"

    /// The Sample editor (`SampleEditorView`).
    module SampleEditor =
        [<Literal>]
        let window = "SampleEditorWindow"
        [<Literal>]
        let nameBox = "SampleNameBox"
        [<Literal>]
        let addLayerButton = "AddLayerButton"
        [<Literal>]
        let makeRepeatBlockButton = "MakeRepeatBlockButton"
        [<Literal>]
        let selectByMaterialButton = "SelectByMaterialButton"
        [<Literal>]
        let setLayerHeightButton = "SetLayerHeightButton"
        [<Literal>]
        let setLayerMaterialButton = "SetLayerMaterialButton"
        [<Literal>]
        let setOrientationOfSelectedButton = "SetOrientationOfSelectedButton"
        [<Literal>]
        let removeSelectedLayersButton = "RemoveSelectedLayersButton"
        [<Literal>]
        let repeatCountStepper = "RepeatCountStepper"
        [<Literal>]
        let qwotEntryBox = "QwotEntryBox"
        [<Literal>]
        let saveButton = "SampleEditorSaveButton"
        [<Literal>]
        let cancelButton = "SampleEditorCancelButton"
        /// Spec 0038 (033): the unsaved-edit exit confirm surface — the prompt row and its two
        /// actions (Discard changes / Keep editing).
        [<Literal>]
        let exitConfirm = "SampleEditorExitConfirm"
        [<Literal>]
        let discardButton = "SampleEditorDiscardButton"
        [<Literal>]
        let keepEditingButton = "SampleEditorKeepEditingButton"
        // Supporting fixed ids (not slice-mandated, same naming discipline).
        [<Literal>]
        let descriptionBox = "SampleDescriptionBox"
        [<Literal>]
        let layerHeightBox = "SampleLayerHeightBox"
        [<Literal>]
        let phiBox = "SampleOrientationPhiBox"
        [<Literal>]
        let thetaBox = "SampleOrientationThetaBox"
        [<Literal>]
        let psiBox = "SampleOrientationPsiBox"
        [<Literal>]
        let clearSelectionButton = "ClearSelectionButton"
        [<Literal>]
        let moveUpButton = "MoveSelectedUpButton"
        [<Literal>]
        let moveDownButton = "MoveSelectedDownButton"
        [<Literal>]
        let qwotDerivedText = "QwotDerivedText"
        [<Literal>]
        let filmsCount = "SampleFilmsCount"
        // Substrate / lower half-space surfaces (spec 0033 gap G12).
        [<Literal>]
        let substrateSummary = "SampleSubstrateSummary"
        [<Literal>]
        let lowerSummary = "SampleLowerSummary"
        [<Literal>]
        let setSubstrateButton = "SetSubstrateButton"
        [<Literal>]
        let clearSubstrateButton = "ClearSubstrateButton"
        [<Literal>]
        let setLowerButton = "SetLowerButton"
        [<Literal>]
        let clearLowerButton = "ClearLowerButton"
        [<Literal>]
        let statusText = "SampleEditorStatus"
        // The supported-emission R / T checkboxes (spec 0040 Part D.2 step 010): a Plate exposes
        // both; a ThinFilm shows R fixed on + disabled and renders no T box.
        [<Literal>]
        let emitReflectedCheck = "SampleEmitReflectedCheck"
        [<Literal>]
        let emitTransmittedCheck = "SampleEmitTransmittedCheck"
        [<Literal>]
        let stackTable = "SampleStackTable"
        [<Literal>]
        let repeatCountStepperPlus = "RepeatCountStepperPlus"
        [<Literal>]
        let repeatCountStepperMinus = "RepeatCountStepperMinus"
        /// The toolbar fold stepper carries the mandated literal; each period group's INLINE
        /// stepper carries the group-indexed member of the same family.
        let groupStepper (groupIndex : int) : string = $"RepeatCountStepper_%d{groupIndex}"
        let groupStepperPlus (groupIndex : int) : string = $"RepeatCountStepperPlus_%d{groupIndex}"
        let groupStepperMinus (groupIndex : int) : string = $"RepeatCountStepperMinus_%d{groupIndex}"
        /// A top-level single layer's row / a period group's nested cell-layer row.
        let layerRow (itemIndex : int) : string = $"SampleLayerRow_%d{itemIndex}"
        let cellLayerRow (itemIndex : int) (cellIndex : int) : string = $"SampleLayerRow_%d{itemIndex}_%d{cellIndex}"
        /// A period group's collapsible super-row and its rotating-triangle expander.
        let groupRow (itemIndex : int) : string = $"SampleGroupRow_%d{itemIndex}"
        let groupExpander (itemIndex : int) : string = $"SampleGroupExpander_%d{itemIndex}"
        /// A row's thickness readout cell (what the bulk set-thickness acceptance observes).
        let layerThickness (itemIndex : int) : string = $"SampleLayerThickness_%d{itemIndex}"
        let cellLayerThickness (itemIndex : int) (cellIndex : int) : string = $"SampleLayerThickness_%d{itemIndex}_%d{cellIndex}"
        /// A row's per-layer orientation editor (present ONLY for anisotropic materials).
        let layerOrientation (itemIndex : int) : string = $"SampleLayerOrientation_%d{itemIndex}"
        let cellLayerOrientation (itemIndex : int) (cellIndex : int) : string = $"SampleLayerOrientation_%d{itemIndex}_%d{cellIndex}"
        /// A SubstrateKind facet option's clickable id, by its stable code.
        let substrateOption (code : string) : string = "SampleSubstrateKind_" + code
        /// A layer row's Choose material… verb (spec 0038 step 019): a top-level single layer's
        /// slot / a period group's nested cell-layer slot — opens the Materials window in Select
        /// state targeted at that row's `LayerPosition`.
        let chooseMaterialButton (itemIndex : int) : string = $"ChooseMaterialButton_%d{itemIndex}"
        let cellChooseMaterialButton (itemIndex : int) (cellIndex : int) : string = $"ChooseMaterialButton_%d{itemIndex}_%d{cellIndex}"

    /// The Material editor (`MaterialEditorView`).
    module MaterialEditor =
        [<Literal>]
        let window = "MaterialEditorWindow"
        [<Literal>]
        let nameBox = "MaterialNameBox"
        [<Literal>]
        let anisotropyToggle = "AnisotropyToggle"
        [<Literal>]
        let absorbingToggle = "AbsorbingToggle"
        /// Spec 0035 (017): the eps branch is two mutually-exclusive options — Constant / Dispersive.
        [<Literal>]
        let constantToggle = "ConstantToggle"
        [<Literal>]
        let dispersiveToggle = "DispersiveToggle"
        [<Literal>]
        let activeToggle = "ActiveToggle"
        [<Literal>]
        let magneticToggle = "MagneticToggle"
        /// Spec 0035 (011) — the per-rung Constant/Dispersive sub-toggles.
        [<Literal>]
        let activityDispersiveToggle = "ActivityDispersiveToggle"
        [<Literal>]
        let magneticDispersiveToggle = "MagneticDispersiveToggle"
        [<Literal>]
        let gyrationClassPicker = "GyrationClassPicker"
        [<Literal>]
        let handednessSwitch = "HandednessSwitch"
        [<Literal>]
        let dispersionModelPicker = "DispersionModelPicker"
        [<Literal>]
        let addSegmentButton = "AddSegmentButton"
        [<Literal>]
        let saveButton = "MaterialEditorSaveButton"
        [<Literal>]
        let cancelButton = "MaterialEditorCancelButton"
        /// Spec 0038 (033): the unsaved-edit exit confirm surface.
        [<Literal>]
        let exitConfirm = "MaterialEditorExitConfirm"
        [<Literal>]
        let discardButton = "MaterialEditorDiscardButton"
        [<Literal>]
        let keepEditingButton = "MaterialEditorKeepEditingButton"
        // Supporting fixed ids (not slice-mandated, same naming discipline).
        [<Literal>]
        let descriptionBox = "MaterialDescriptionBox"
        [<Literal>]
        let categoryPicker = "MaterialCategoryPicker"
        [<Literal>]
        let gyrationAxisPicker = "GyrationAxisPicker"
        [<Literal>]
        let muDiagonalBox = "MuDiagonalBox"
        [<Literal>]
        let muParallelBox = "MuParallelBox"
        [<Literal>]
        let muGyrationBox = "MuGyrationBox"
        [<Literal>]
        let statusText = "MaterialEditorStatus"
        [<Literal>]
        let summaryText = "MaterialComplexitySummary"
        [<Literal>]
        let previewChart = "MaterialPreviewChart"
        [<Literal>]
        let gainWarning = "MaterialGainWarning"
        [<Literal>]
        let viewOnlyNote = "MaterialViewOnlyNote"
        /// Spec 0038 (032): the two-pane split's vertical GridSplitter, the tabbed preview's TabControl,
        /// its three tabs, and the gyration / μ tab chart hosts (the n/k tab reuses `previewChart`).
        [<Literal>]
        let splitter = "MaterialEditorSplitter"
        [<Literal>]
        let previewTabs = "MaterialPreviewTabs"
        [<Literal>]
        let nkTab = "MaterialPreviewNkTab"
        [<Literal>]
        let gyrationTab = "MaterialPreviewGyrationTab"
        [<Literal>]
        let muTab = "MaterialPreviewMuTab"
        [<Literal>]
        let gyrationChart = "MaterialGyrationChart"
        [<Literal>]
        let muChart = "MaterialMuChart"
        /// A per-series show/hide toggle in a preview tab, by the tab's stable code and the series name.
        let seriesToggle (tabCode : string) (seriesName : string) : string = $"PreviewSeriesToggle_%s{tabCode}_%s{seriesName}"
        /// An anisotropy option, by the choice's stable code.
        let anisotropyOption (code : string) : string = "AnisotropyOption_" + code
        /// A material-category option, by the category's stable code.
        let categoryOption (code : string) : string = "MaterialCategoryOption_" + code
        /// A principal-index entry (1-based axis slot; present per the anisotropy choice).
        let indexBox (axisNumber : int) : string = $"PrincipalIndexBox_%d{axisNumber}"
        /// A principal absorption (k) entry (present only while the absorbing rung is unlocked).
        let absorptionBox (axisNumber : int) : string = $"AbsorptionIndexBox_%d{axisNumber}"
        /// Segment 0's model picker carries the mandated literal; later segments carry the
        /// indexed member of the same family (the 022 RepeatCountStepper precedent).
        let segmentModelPicker (segmentIndex : int) : string =
            if segmentIndex = 0 then dispersionModelPicker else $"DispersionModelPicker_%d{segmentIndex}"
        let segmentLowerBox (segmentIndex : int) : string = $"SegmentLowerBox_%d{segmentIndex}"
        let segmentUpperBox (segmentIndex : int) : string = $"SegmentUpperBox_%d{segmentIndex}"
        let segmentRemoveButton (segmentIndex : int) : string = $"SegmentRemoveButton_%d{segmentIndex}"
        /// A segment's dispersion-model option, by segment index and the model kind's code.
        let modelOption (segmentIndex : int) (code : string) : string = $"DispersionModelOption_%d{segmentIndex}_%s{code}"
        /// A gyration-class option, by the class's stable code.
        let gyrationClassOption (code : string) : string = "GyrationClassOption_" + code
        /// A handedness option ("Right" / "Left").
        let handednessOption (code : string) : string = "HandednessOption_" + code
        /// A magnetization-axis option ("X" / "Y" / "Z").
        let gyrationAxisOption (code : string) : string = "GyrationAxisOption_" + code
        /// A Polder-mu kind option ("Scalar" / "Gyromagnetic").
        let muKindOption (code : string) : string = "MuKindOption_" + code
        /// A per-segment dispersion-model COEFFICIENT entry (spec 0033 gap G7), by
        /// segment index and the parameter's stable key.
        let segmentParamBox (segmentIndex : int) (key : string) : string = $"SegmentParamBox_{segmentIndex}_{key}"
        /// Per-principal-axis id families (spec 0033 comment 009 — uniaxial / biaxial
        /// dispersive media carry one formula per axis). The FIRST axis reduces to the
        /// single-axis ids above, so the slice-mandated literals and existing tests stay
        /// valid; the extraordinary / y / z axes get an axis-suffixed member.
        let axisModelPickerOf (segmentIndex : int) (isFirst : bool) (axisCode : string) : string =
            if isFirst then segmentModelPicker segmentIndex else $"{segmentModelPicker segmentIndex}_ax{axisCode}"
        let axisModelOptionOf (segmentIndex : int) (isFirst : bool) (axisCode : string) (code : string) : string =
            if isFirst then modelOption segmentIndex code else $"{modelOption segmentIndex code}_ax{axisCode}"
        let axisParamBoxOf (segmentIndex : int) (isFirst : bool) (axisCode : string) (key : string) : string =
            if isFirst then segmentParamBox segmentIndex key else $"{segmentParamBox segmentIndex key}_ax{axisCode}"
        /// A gyration-tensor COMPONENT entry (spec 0033 gap G9), by the component's
        /// stable code (`g11` / `g33` / …).
        let gyrationComponentBox (code : string) : string = "GyrationComponentBox_" + code
        /// Spec 0035 (011): a gyration component's DISPERSION-FORMULA editor container (the
        /// activity Dispersive sub-branch), by the component's stable code — the id the
        /// headless proof probes for a per-component formula editor.
        let gyrationComponentFormulaEditor (code : string) : string = "GyrationFormulaEditor_" + code
        /// One coefficient entry inside a gyration component's dispersion-formula editor, by
        /// the component code and the wrapped model parameter's key (the raw `SumOfTerms`
        /// coefficient surface the segments already use).
        let gyrationComponentFormulaBox (code : string) (key : string) : string = $"GyrationFormulaBox_{code}_{key}"
        /// Spec 0035 (011): a Polder component's DISPERSION-FORMULA editor container (the
        /// magnetic Dispersive sub-branch), by the component's stable code
        /// (`muDiagonal` / `muParallel` / `muGyration`).
        let polderComponentFormulaEditor (code : string) : string = "PolderFormulaEditor_" + code
        /// One coefficient entry inside a Polder component's dispersion-formula editor.
        let polderComponentFormulaBox (code : string) (key : string) : string = $"PolderFormulaBox_{code}_{key}"

    /// The Category editor window (`CategoryEditorView`) — every list/row/verb id is reused from
    /// `UiIds.Category`; only the window-level id lives here.
    module CategoryEditor =
        [<Literal>]
        let window = "CategoryEditorWindow"

    /// The single-instance Library window (`LibraryWindowView`). The Domain-typed `versionRow` /
    /// `entryNode` id helpers stay beside the view (Controls is domain-free); every literal is here.
    module LibraryWindow =
        [<Literal>]
        let window = "LibraryWindow"
        [<Literal>]
        let treeHost = "LibraryFacetTreeHost"
        [<Literal>]
        let viewPanel = "LibraryViewPanel"
        [<Literal>]
        let addSampleButton = "LibraryAddSampleButton"
        [<Literal>]
        let makeMultilayerButton = "LibraryMakeMultilayerButton"
        [<Literal>]
        let editButton = "LibraryEditButton"
        [<Literal>]
        let removeButton = "LibraryRemoveButton"
        [<Literal>]
        let removeConfirmButton = "LibraryRemoveConfirmButton"
        [<Literal>]
        let removeCancelButton = "LibraryRemoveCancelButton"
        [<Literal>]
        let message = "LibraryWindowMessage"
        /// The lifecycle surface (spec 0038 step 023).
        [<Literal>]
        let showInactiveToggle = "LibraryShowInactiveToggle"
        [<Literal>]
        let markInactiveButton = "LibraryMarkInactiveButton"
        [<Literal>]
        let markActiveButton = "LibraryMarkActiveButton"
        [<Literal>]
        let supersedeButton = "LibrarySupersedeButton"
        [<Literal>]
        let lifecycleConfirmButton = "LibraryLifecycleConfirmButton"
        [<Literal>]
        let lifecycleCancelButton = "LibraryLifecycleCancelButton"
        [<Literal>]
        let versionsPanel = "LibraryVersionsPanel"
        [<Literal>]
        let viewOnlyNote = "LibraryViewOnlyNote"
        /// The Select-state pair and the fixed-constraint banner (spec 0038 step 016).
        [<Literal>]
        let selectButton = "LibrarySelectButton"
        [<Literal>]
        let selectCloseButton = "LibrarySelectCloseButton"
        [<Literal>]
        let selectConstraint = "LibrarySelectConstraint"

    /// The single-instance Materials window (`MaterialsWindowView`). As with `LibraryWindow`, the
    /// Domain-typed `versionRow` / `entryNode` helpers stay beside the view; every literal is here.
    module MaterialsWindow =
        [<Literal>]
        let window = "MaterialsWindow"
        [<Literal>]
        let treeHost = "MaterialsFacetTreeHost"
        [<Literal>]
        let viewPanel = "MaterialsViewPanel"
        [<Literal>]
        let viewPanelChart = "MaterialsViewPanelNkChart"
        [<Literal>]
        let addButton = "MaterialsAddButton"
        [<Literal>]
        let editButton = "MaterialsEditButton"
        [<Literal>]
        let removeButton = "MaterialsRemoveButton"
        [<Literal>]
        let categoriesButton = "MaterialsCategoriesButton"
        [<Literal>]
        let removeConfirmButton = "MaterialsRemoveConfirmButton"
        [<Literal>]
        let removeCancelButton = "MaterialsRemoveCancelButton"
        [<Literal>]
        let message = "MaterialsWindowMessage"
        /// The lifecycle surface (spec 0038 step 023).
        [<Literal>]
        let showInactiveToggle = "MaterialsShowInactiveToggle"
        [<Literal>]
        let markInactiveButton = "MaterialsMarkInactiveButton"
        [<Literal>]
        let markActiveButton = "MaterialsMarkActiveButton"
        [<Literal>]
        let supersedeButton = "MaterialsSupersedeButton"
        [<Literal>]
        let lifecycleConfirmButton = "MaterialsLifecycleConfirmButton"
        [<Literal>]
        let lifecycleCancelButton = "MaterialsLifecycleCancelButton"
        [<Literal>]
        let versionsPanel = "MaterialsVersionsPanel"
        [<Literal>]
        let viewOnlyNote = "MaterialsViewOnlyNote"
        /// The Select-state pair and the fixed-constraint banner (spec 0038 step 016).
        [<Literal>]
        let selectButton = "MaterialsSelectButton"
        [<Literal>]
        let selectCloseButton = "MaterialsSelectCloseButton"
        [<Literal>]
        let selectConstraint = "MaterialsSelectConstraint"

    // ---- TestWindows diagnostic scene surfaces -----------------------------------------------

    /// The renderer test scene (`RendererTestView`).
    module RendererTest =
        [<Literal>]
        let canvas = "RendererTestCanvas"

    /// The snap-to-reflected-light test scene (`SnapToReflectedView`).
    module SnapToReflected =
        [<Literal>]
        let canvas = "SnapReflectedCanvas"
        [<Literal>]
        let readout = "SnapReflectedReadout"

    /// The snap-to-beam test scene (`SnapToBeamView`).
    module SnapToBeam =
        [<Literal>]
        let canvas = "SnapToBeamCanvas"
        [<Literal>]
        let readout = "SnapToBeamReadout"

    /// The element-movement test scene (`ElementMovementView`).
    module ElementMovement =
        [<Literal>]
        let canvas = "ElementMovementCanvas"

    /// The element-rotation test scene (`ElementRotationView`).
    module ElementRotation =
        [<Literal>]
        let canvas = "ElementRotationCanvas"
        [<Literal>]
        let rotateR1Minus = "ElemRotateR1MinusButton"
        [<Literal>]
        let rotateR1Plus = "ElemRotateR1PlusButton"
        [<Literal>]
        let rotateR2Minus = "ElemRotateR2MinusButton"
        [<Literal>]
        let rotateR2Plus = "ElemRotateR2PlusButton"
        [<Literal>]
        let rotateR3Minus = "ElemRotateR3MinusButton"
        [<Literal>]
        let rotateR3Plus = "ElemRotateR3PlusButton"
        [<Literal>]
        let unlockR3 = "ElemUnlockR3Button"
        [<Literal>]
        let reset = "ElemResetButton"
        [<Literal>]
        let readout = "ElemRotationReadout"

    /// The table-rotation test scene (`TableRotationView`).
    module TableRotation =
        [<Literal>]
        let canvas = "TableRotationCanvas"
        [<Literal>]
        let rotateR1Minus = "RotateR1MinusButton"
        [<Literal>]
        let rotateR1Plus = "RotateR1PlusButton"
        [<Literal>]
        let rotateR2Minus = "RotateR2MinusButton"
        [<Literal>]
        let rotateR2Plus = "RotateR2PlusButton"
        [<Literal>]
        let rotateR3Minus = "RotateR3MinusButton"
        [<Literal>]
        let rotateR3Plus = "RotateR3PlusButton"
        [<Literal>]
        let resetView = "ResetViewButton"
        [<Literal>]
        let readout = "TableRotationReadout"

    /// The diagnostic test launcher window (`TestLauncherWindow`, project TestWindows.App).
    module TestLauncher =
        [<Literal>]
        let openTableRotationTestButton = "OpenTableRotationTestButton"
        [<Literal>]
        let openElementRotationTestButton = "OpenElementRotationTestButton"
        [<Literal>]
        let openTableAndElementRotationTestButton = "OpenTableAndElementRotationTestButton"
        [<Literal>]
        let openElementMovementTestButton = "OpenElementMovementTestButton"
        [<Literal>]
        let openRendererTestButton = "OpenRendererTestButton"
        [<Literal>]
        let openSnapToBeamTestButton = "OpenSnapToBeamTestButton"
        [<Literal>]
        let openSnapToReflectedTestButton = "OpenSnapToReflectedTestButton"

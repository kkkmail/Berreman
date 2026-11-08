namespace OpticalConstructor.Controls

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types

/// Where a repeated control sits in its surface (a band, a layer row, a dispersion segment, a principal
/// axis): what tells it apart from its siblings when that is a position.
type ConstructorUiPosition =
    | ConstructorUiPosition of int

    member this.value = let (ConstructorUiPosition v) = this in v

/// What tells a repeated control apart from its siblings when that is a name: the host's stable id or
/// code of the entry, element, experiment or option the control stands for.
type ConstructorUiKey =
    | ConstructorUiKey of string

    member this.value = let (ConstructorUiKey v) = this in v

/// The stable automation id of one Optical Constructor control (spec 0048 R1, the shape of STL's `VncUiId`).
///
/// The views and the tests share an id, so it is a domain value: one member per control, its text
/// unchanged from spec 0038's `UiIds` literals. An id derived from a Domain value is an extension
/// member in the Ui project (`ConstructorUiIdOfDomain`), because Controls references no Domain type.
type ConstructorUiId =
    | ConstructorUiId of string

    member this.value = let (ConstructorUiId v) = this in v

    /// The id as the control's `AutomationProperties.AutomationId`.
    member this.automationAttribute<'Control when 'Control :> Control> () : IAttr<'Control> =
        AttrBuilder<'Control>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, this.value, ValueNone)

    // --- The reusable rotation-controls bar (`RotationControls`). ---
    static member rotationR1MinusButton : ConstructorUiId = ConstructorUiId "RotationR1MinusButton"
    static member rotationR1PlusButton : ConstructorUiId = ConstructorUiId "RotationR1PlusButton"
    static member rotationR2MinusButton : ConstructorUiId = ConstructorUiId "RotationR2MinusButton"
    static member rotationR2PlusButton : ConstructorUiId = ConstructorUiId "RotationR2PlusButton"
    static member rotationR3MinusButton : ConstructorUiId = ConstructorUiId "RotationR3MinusButton"
    static member rotationR3PlusButton : ConstructorUiId = ConstructorUiId "RotationR3PlusButton"
    static member rotationR1Field : ConstructorUiId = ConstructorUiId "RotationR1Field"
    static member rotationR2Field : ConstructorUiId = ConstructorUiId "RotationR2Field"
    static member rotationR3Field : ConstructorUiId = ConstructorUiId "RotationR3Field"
    static member rotationLockR3Button : ConstructorUiId = ConstructorUiId "RotationLockR3Button"
    /// Spec 0046: the R1 / R2 lock toggles (offered for elements; the light source starts locked).
    static member rotationLockR1Button : ConstructorUiId = ConstructorUiId "RotationLockR1Button"
    static member rotationLockR2Button : ConstructorUiId = ConstructorUiId "RotationLockR2Button"
    // The two reset buttons keep these stable ids in BOTH modes: "Reset"/"Reset All" when idle,
    // and the confirmation "Yes"/"No" when a reset is armed (the label/action changes, not the id).
    static member rotationResetButton : ConstructorUiId = ConstructorUiId "RotationResetButton"
    static member rotationResetAllButton : ConstructorUiId = ConstructorUiId "RotationResetAllButton"

    // --- The element-palette bar (`ElementPaletteControls`). ---
    static member paletteRemoveSelectedButton : ConstructorUiId = ConstructorUiId "PaletteRemoveSelectedButton"
    /// The add button of a palette entry.
    static member paletteAddButton (entry : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"PaletteAdd_{entry.value}"

    // --- The position-on-the-ray bar (`RayPositionControls`). ---
    static member rayPositionMinusButton : ConstructorUiId = ConstructorUiId "RayPositionMinusButton"
    static member rayPositionPlusButton : ConstructorUiId = ConstructorUiId "RayPositionPlusButton"
    static member rayPositionField : ConstructorUiId = ConstructorUiId "RayPositionField"
    static member rayPositionResetButton : ConstructorUiId = ConstructorUiId "RayPositionResetButton"
    static member rayPositionReadout : ConstructorUiId = ConstructorUiId "RayPositionReadout"

    // --- The light source's free x / y / z bar (`TablePositionControls`, spec 0046). ---
    static member tablePositionXMinusButton : ConstructorUiId = ConstructorUiId "TablePositionXMinusButton"
    static member tablePositionXPlusButton : ConstructorUiId = ConstructorUiId "TablePositionXPlusButton"
    static member tablePositionXField : ConstructorUiId = ConstructorUiId "TablePositionXField"
    static member tablePositionXLockButton : ConstructorUiId = ConstructorUiId "TablePositionXLockButton"
    static member tablePositionYMinusButton : ConstructorUiId = ConstructorUiId "TablePositionYMinusButton"
    static member tablePositionYPlusButton : ConstructorUiId = ConstructorUiId "TablePositionYPlusButton"
    static member tablePositionYField : ConstructorUiId = ConstructorUiId "TablePositionYField"
    static member tablePositionYLockButton : ConstructorUiId = ConstructorUiId "TablePositionYLockButton"
    static member tablePositionZMinusButton : ConstructorUiId = ConstructorUiId "TablePositionZMinusButton"
    static member tablePositionZPlusButton : ConstructorUiId = ConstructorUiId "TablePositionZPlusButton"
    static member tablePositionZField : ConstructorUiId = ConstructorUiId "TablePositionZField"
    static member tablePositionZLockButton : ConstructorUiId = ConstructorUiId "TablePositionZLockButton"
    static member tablePositionResetButton : ConstructorUiId = ConstructorUiId "TablePositionResetButton"
    static member tablePositionReadout : ConstructorUiId = ConstructorUiId "TablePositionReadout"

    // --- The renderer "large control" (`RendererControls`). ---
    static member rendererSwapButton : ConstructorUiId = ConstructorUiId "RendererSwapButton"
    static member rendererReadout : ConstructorUiId = ConstructorUiId "RendererReadout"
    static member rendererRailsSlider : ConstructorUiId = ConstructorUiId "RendererRailsSlider"
    static member rendererCapCirclesSlider : ConstructorUiId = ConstructorUiId "RendererCapCirclesSlider"
    static member rendererCapRadialsSlider : ConstructorUiId = ConstructorUiId "RendererCapRadialsSlider"
    static member rendererRailOpacitySlider : ConstructorUiId = ConstructorUiId "RendererRailOpacitySlider"
    static member rendererFaceOpacitySlider : ConstructorUiId = ConstructorUiId "RendererFaceOpacitySlider"
    static member rendererLineOpacitySlider : ConstructorUiId = ConstructorUiId "RendererLineOpacitySlider"

    // --- The main-screen ribbon of bays (`Ribbon`). ---
    /// The selectable tab of a bay, by the bay's name.
    static member ribbonTab (bay : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"RibbonTab_{bay.value}"
    /// The single active content slot, named by the bay it shows.
    static member ribbonPane (bay : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"RibbonPane_{bay.value}"

    // --- The kind-constrained Library bay list/tree (`LibraryControls`). ---
    static member libraryBoundReadout : ConstructorUiId = ConstructorUiId "LibraryBoundReadout"
    static member libraryKindLabel : ConstructorUiId = ConstructorUiId "LibraryKindLabel"
    static member libraryTree : ConstructorUiId = ConstructorUiId "LibraryTree"
    /// A leaf row's clickable box, by the entry's id.
    static member libraryEntry (entry : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"LibraryEntry_{entry.value}"
    /// The pending-entry full-description text (shown before confirm).
    static member libraryEntryDescription : ConstructorUiId = ConstructorUiId "LibraryEntryDescription"
    /// The Confirm / Cancel actions of the pending bind.
    static member libraryConfirmButton : ConstructorUiId = ConstructorUiId "LibraryConfirmButton"
    static member libraryCancelButton : ConstructorUiId = ConstructorUiId "LibraryCancelButton"

    // --- The layer-stack band view / Details bay (`LayerBandsControls`). ---
    static member layerBandsStack : ConstructorUiId = ConstructorUiId "LayerBandsStack"
    static member layerBandsTitle : ConstructorUiId = ConstructorUiId "LayerBandsTitle"
    /// A band's drawn rectangle, top to bottom in band order from 0.
    static member layerBand (band : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"LayerBand_{band.value}"

    // --- The Experiments bay (`ExperimentControls`). ---
    static member experimentReadout : ConstructorUiId = ConstructorUiId "ExperimentReadout"
    static member experimentCandidates : ConstructorUiId = ConstructorUiId "ExperimentCandidates"
    /// A candidate element's clickable box, by the element's id.
    static member experimentCandidate (element : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ExperimentCandidate_{element.value}"
    /// A variable choice, by its code ("wavelength" / "r1" / "r2").
    static member experimentVariable (variable : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ExperimentVariable_{variable.value}"
    /// A measurement choice, by its code ("t" / "r" / "both").
    static member experimentMeasurement (measurement : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ExperimentMeasurement_{measurement.value}"
    /// Spec 0046: the hint shown instead of the capture choices while no detector captures the chosen
    /// element's light.
    static member experimentNoCapture : ConstructorUiId = ConstructorUiId "ExperimentNoCapture"
    /// Spec 0046: the source-wavelength field (a tunable source's setting while something else varies).
    static member experimentSourceWaveLength : ConstructorUiId = ConstructorUiId "ExperimentSourceWaveLength"
    static member experimentSourceWaveLengthNote : ConstructorUiId = ConstructorUiId "ExperimentSourceWaveLengthNote"
    /// Spec 0046: Export data (the draft's computed run through the export seam) and its status line.
    static member experimentExportButton : ConstructorUiId = ConstructorUiId "ExperimentExportButton"
    static member experimentExportStatus : ConstructorUiId = ConstructorUiId "ExperimentExportStatus"
    /// The range inputs.
    static member experimentRangeMin : ConstructorUiId = ConstructorUiId "ExperimentRangeMin"
    static member experimentRangeMax : ConstructorUiId = ConstructorUiId "ExperimentRangeMax"
    static member experimentRangePoints : ConstructorUiId = ConstructorUiId "ExperimentRangePoints"
    /// The Add / Update and New actions.
    static member experimentAddButton : ConstructorUiId = ConstructorUiId "ExperimentAddButton"
    static member experimentNewButton : ConstructorUiId = ConstructorUiId "ExperimentNewButton"
    /// The collection list and a row's Edit / Remove actions.
    static member experimentCollection : ConstructorUiId = ConstructorUiId "ExperimentCollection"
    /// A collected experiment's Edit, Remove and View (open its chart window) actions.
    static member experimentEditButton (experiment : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ExperimentEdit_{experiment.value}"
    static member experimentRemoveButton (experiment : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ExperimentRemove_{experiment.value}"
    static member experimentViewButton (experiment : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ExperimentView_{experiment.value}"
    /// The inline result polyline (the first series — the intensity / Ψ curve).
    static member experimentChart : ConstructorUiId = ConstructorUiId "ExperimentChart"
    /// The ellipsometer Ψ/Δ single-point readout text.
    static member ellipsometerReadout : ConstructorUiId = ConstructorUiId "EllipsometerReadout"
    /// The chart description text shown under the inline chart.
    static member experimentChartDescription : ConstructorUiId = ConstructorUiId "ExperimentChartDescription"

    // --- The Main workbench's own surfaces around the shared controls (spec 0038 steps 013-017, 037). ---
    /// The ribbon tab-strip row's right-aligned buttons that open the single-instance Materials / Library windows.
    static member openMaterialsWindowButton : ConstructorUiId = ConstructorUiId "OpenMaterialsWindowButton"
    static member openLibraryWindowButton : ConstructorUiId = ConstructorUiId "OpenLibraryWindowButton"
    /// The staleness status line under the readout (a targeted Select return whose element has vanished).
    static member workbenchSelectStatus : ConstructorUiId = ConstructorUiId "WorkbenchSelectStatus"
    /// The Select bay's Choose… verb and its inline quick-pick strip.
    static member selectorChooseButton : ConstructorUiId = ConstructorUiId "SelectorChooseButton"
    static member selectorQuickPickStrip : ConstructorUiId = ConstructorUiId "SelectorQuickPickStrip"
    /// The kit status line and the inverse experiment data files (spec 0050 S4.3).
    static member kitStatus : ConstructorUiId = ConstructorUiId "KitStatus"
    /// The scene row's name field, save action and saved scenes (spec 0050 S4.3).
    static member sceneNameField : ConstructorUiId = ConstructorUiId "SceneNameField"
    static member saveSceneButton : ConstructorUiId = ConstructorUiId "SaveSceneButton"
    static member savedScenes : ConstructorUiId = ConstructorUiId "SavedScenes"
    static member experimentDataFiles : ConstructorUiId = ConstructorUiId "ExperimentDataFiles"

    // --- The materials workbench list surface (`MaterialsControls`). ---
    static member materialSearchBox : ConstructorUiId = ConstructorUiId "MaterialSearchBox"
    static member materialCategoryFilter : ConstructorUiId = ConstructorUiId "MaterialCategoryFilter"
    static member materialDispersionFilter : ConstructorUiId = ConstructorUiId "MaterialDispersionFilter"
    static member materialsList : ConstructorUiId = ConstructorUiId "MaterialsList"
    static member addMaterialButton : ConstructorUiId = ConstructorUiId "AddMaterialButton"
    static member editMaterialButton : ConstructorUiId = ConstructorUiId "EditMaterialButton"
    static member removeMaterialButton : ConstructorUiId = ConstructorUiId "RemoveMaterialButton"
    static member viewMaterialButton : ConstructorUiId = ConstructorUiId "ViewMaterialButton"
    /// A listed material's clickable row, by the material's id.
    static member materialRow (material : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"MaterialRow_{material.value}"
    /// A category or dispersion facet option, by its host-supplied code.
    static member materialCategoryOption (category : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"MaterialCategoryOption_{category.value}"
    static member materialDispersionOption (dispersion : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"MaterialDispersionOption_{dispersion.value}"

    // --- The samples workbench list surface (`SampleLibraryControls`). ---
    static member sampleSearchBox : ConstructorUiId = ConstructorUiId "SampleSearchBox"
    static member sampleSubstrateFilter : ConstructorUiId = ConstructorUiId "SampleSubstrateFilter"
    static member samplesList : ConstructorUiId = ConstructorUiId "SamplesList"
    static member addSampleButton : ConstructorUiId = ConstructorUiId "AddSampleButton"
    static member editSampleButton : ConstructorUiId = ConstructorUiId "EditSampleButton"
    static member removeSampleButton : ConstructorUiId = ConstructorUiId "RemoveSampleButton"
    static member viewSampleButton : ConstructorUiId = ConstructorUiId "ViewSampleButton"
    static member makeMultilayerButton : ConstructorUiId = ConstructorUiId "MakeMultilayerButton"
    /// A listed sample's clickable row, by the sample's id.
    static member sampleRow (sample : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"SampleRow_{sample.value}"
    /// A substrate facet option, by its host-supplied code.
    static member sampleSubstrateOption (substrate : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"SampleSubstrateOption_{substrate.value}"

    // --- The Category-manager list surface (`CategoryControls`). ---
    static member categoriesList : ConstructorUiId = ConstructorUiId "CategoriesList"
    static member addCategoryButton : ConstructorUiId = ConstructorUiId "AddCategoryButton"
    static member categoryNameBox : ConstructorUiId = ConstructorUiId "CategoryNameBox"
    static member removeCategoryButton : ConstructorUiId = ConstructorUiId "RemoveCategoryButton"
    static member categorySaveButton : ConstructorUiId = ConstructorUiId "CategorySaveButton"
    static member categoryCancelButton : ConstructorUiId = ConstructorUiId "CategoryCancelButton"
    static member categoryBlockMessage : ConstructorUiId = ConstructorUiId "CategoryBlockMessage"
    /// A row's inline name box, Save, Remove (a non-built-in row only) and Cancel, by the row's category id.
    static member categoryRowNameBox (category : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"CategoryNameBox_{category.value}"
    static member categoryRowSaveButton (category : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"CategorySaveButton_{category.value}"
    static member categoryRowRemoveButton (category : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"RemoveCategoryButton_{category.value}"
    static member categoryRowCancelButton (category : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"CategoryCancelButton_{category.value}"

    // --- The domain-free faceted, filterable tree (`FacetedTreeControls`). ---
    static member facetFilterBox : ConstructorUiId = ConstructorUiId "FacetFilterBox"
    static member facetRepresentationPicker : ConstructorUiId = ConstructorUiId "FacetRepresentationPicker"
    static member facetBreadcrumbStrip : ConstructorUiId = ConstructorUiId "FacetBreadcrumbStrip"
    static member facetResultCount : ConstructorUiId = ConstructorUiId "FacetResultCount"
    static member facetOffersPanel : ConstructorUiId = ConstructorUiId "FacetOffersPanel"
    static member facetShowTreeButton : ConstructorUiId = ConstructorUiId "FacetShowTreeButton"
    static member facetTree : ConstructorUiId = ConstructorUiId "FacetTree"
    static member facetScroll : ConstructorUiId = ConstructorUiId "FacetScroll"
    /// A representation option, a breadcrumb chip and an offer group, each by its code.
    static member facetRepresentationOption (representation : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"FacetRepresentationOption_{representation.value}"
    static member facetBreadcrumbChip (chip : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"FacetBreadcrumbChip_{chip.value}"
    static member facetOfferGroup (group : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"FacetOfferGroup_{group.value}"
    /// An offered value, by its group's code THEN its own: the same discrete key can appear under two facets.
    static member facetOfferedValue (group : ConstructorUiKey) (offered : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"FacetOfferedValue_{group.value}_{offered.value}"
    /// A numeric offer group's manual min–max box, by the group's code.
    static member facetManualRangeBox (group : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"FacetManualRangeBox_{group.value}"
    /// A tree node's clickable row, by the node's tree-unique code.
    static member facetTreeNode (node : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"FacetTreeNode_{node.value}"
    /// A parent node's disclosure chevron (spec 0040 step 002); a leaf renders none. Its own prefix, so a
    /// chevron is never counted as a label row.
    static member facetTreeChevron (node : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"FacetTreeChevron_{node.value}"

    // --- The pop-out ScottPlot chart window (`ChartWindow`). ---
    static member chartWindowPlot : ConstructorUiId = ConstructorUiId "ChartWindowPlot"
    /// Spec 030: the element picker (which part of the chart is being formatted) and the properties panel.
    static member chartWindowElement : ConstructorUiId = ConstructorUiId "ChartWindowElement"
    static member chartWindowProperties : ConstructorUiId = ConstructorUiId "ChartWindowProperties"
    /// The selected element's font stepper + readout (shown for the header / axes / legend).
    static member chartWindowFontMinus : ConstructorUiId = ConstructorUiId "ChartWindowFontMinus"
    static member chartWindowFontPlus : ConstructorUiId = ConstructorUiId "ChartWindowFontPlus"
    static member chartWindowFontSize : ConstructorUiId = ConstructorUiId "ChartWindowFontSize"
    /// Axis property controls.
    static member chartWindowAxisAuto : ConstructorUiId = ConstructorUiId "ChartWindowAxisAuto"
    static member chartWindowAxisMin : ConstructorUiId = ConstructorUiId "ChartWindowAxisMin"
    static member chartWindowAxisMax : ConstructorUiId = ConstructorUiId "ChartWindowAxisMax"
    static member chartWindowAxisFormat : ConstructorUiId = ConstructorUiId "ChartWindowAxisFormat"
    static member chartWindowAxisDecimalsMinus : ConstructorUiId = ConstructorUiId "ChartWindowAxisDecimalsMinus"
    static member chartWindowAxisDecimalsPlus : ConstructorUiId = ConstructorUiId "ChartWindowAxisDecimalsPlus"
    static member chartWindowAxisDecimals : ConstructorUiId = ConstructorUiId "ChartWindowAxisDecimals"
    /// Legend property controls.
    static member chartWindowLegendVisible : ConstructorUiId = ConstructorUiId "ChartWindowLegendVisible"
    static member chartWindowLegendPlacement : ConstructorUiId = ConstructorUiId "ChartWindowLegendPlacement"
    /// Series (chart line) property controls.
    static member chartWindowSeriesVisible : ConstructorUiId = ConstructorUiId "ChartWindowSeriesVisible"
    static member chartWindowSeriesThickMinus : ConstructorUiId = ConstructorUiId "ChartWindowSeriesThickMinus"
    static member chartWindowSeriesThickPlus : ConstructorUiId = ConstructorUiId "ChartWindowSeriesThickPlus"
    static member chartWindowSeriesThick : ConstructorUiId = ConstructorUiId "ChartWindowSeriesThick"
    static member chartWindowSeriesColor : ConstructorUiId = ConstructorUiId "ChartWindowSeriesColor"
    static member chartWindowSeriesMarkers : ConstructorUiId = ConstructorUiId "ChartWindowSeriesMarkers"
    /// The series' Y-axis side picker (left / right — spec 0033/018).
    static member chartWindowSeriesAxis : ConstructorUiId = ConstructorUiId "ChartWindowSeriesAxis"
    /// The polar ⇄ XY toggle (only for angular charts).
    static member chartWindowPolar : ConstructorUiId = ConstructorUiId "ChartWindowPolar"
    static member chartWindowMajorGrid : ConstructorUiId = ConstructorUiId "ChartWindowMajorGrid"
    static member chartWindowMinorGrid : ConstructorUiId = ConstructorUiId "ChartWindowMinorGrid"
    static member chartWindowExportPng : ConstructorUiId = ConstructorUiId "ChartWindowExportPng"
    static member chartWindowExportCsv : ConstructorUiId = ConstructorUiId "ChartWindowExportCsv"
    static member chartWindowDescription : ConstructorUiId = ConstructorUiId "ChartWindowDescription"

    // --- The SolverHandoffWindow terminal summary (`SolverHandoffView`). ---
    static member solverHandoffWindow : ConstructorUiId = ConstructorUiId "SolverHandoffWindow"
    static member solverHandoffCollectionName : ConstructorUiId = ConstructorUiId "SolverHandoffCollectionName"
    static member solverHandoffSolverMessage : ConstructorUiId = ConstructorUiId "SolverHandoffSolverMessage"
    static member solverHandoffSummaryList : ConstructorUiId = ConstructorUiId "SolverHandoffSummaryList"
    static member solverHandoffCloseButton : ConstructorUiId = ConstructorUiId "SolverHandoffCloseButton"

    // --- The combined table + element rotation scene (`TableAndElementRotationView`). ---
    static member tableElementCanvas : ConstructorUiId = ConstructorUiId "TableElementCanvas"
    static member teRotateR1MinusButton : ConstructorUiId = ConstructorUiId "TeRotateR1MinusButton"
    static member teRotateR1PlusButton : ConstructorUiId = ConstructorUiId "TeRotateR1PlusButton"
    static member teRotateR2MinusButton : ConstructorUiId = ConstructorUiId "TeRotateR2MinusButton"
    static member teRotateR2PlusButton : ConstructorUiId = ConstructorUiId "TeRotateR2PlusButton"
    static member teRotateR3MinusButton : ConstructorUiId = ConstructorUiId "TeRotateR3MinusButton"
    static member teRotateR3PlusButton : ConstructorUiId = ConstructorUiId "TeRotateR3PlusButton"
    static member teUnlockR3Button : ConstructorUiId = ConstructorUiId "TeUnlockR3Button"
    static member teResetButton : ConstructorUiId = ConstructorUiId "TeResetButton"
    static member teReadout : ConstructorUiId = ConstructorUiId "TeReadout"
    /// Spec 0046: the Main view's page-level scroll viewer (a tall bay never pushes content out of reach).
    static member tableElementScrollViewer : ConstructorUiId = ConstructorUiId "TableElementScrollViewer"
    /// Spec 0046: the Rotation bay's global option — rotate every element about the middle of its incident
    /// face (checked) instead of its centre.
    static member rotateAboutIncidentFaceCheck : ConstructorUiId = ConstructorUiId "RotateAboutIncidentFaceCheck"
    /// Spec 0038 (031): the out-of-band dispersion badge of the scene element at this position.
    static member outOfBandBadge (element : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"OutOfBandBadge_{element.value}"

    // --- The Main canvas's right-click element menu (spec 0046). ---
    static member elementMenu : ConstructorUiId = ConstructorUiId "ElementMenu"
    static member elementMenuToggleReflected : ConstructorUiId = ConstructorUiId "ElementMenuToggleReflected"
    static member elementMenuToggleTransmitted : ConstructorUiId = ConstructorUiId "ElementMenuToggleTransmitted"
    static member elementMenuPlaceOnMainBeam : ConstructorUiId = ConstructorUiId "ElementMenuPlaceOnMainBeam"
    /// Spec 0046: the menu's Choose… — the Select bay's Choose… verb for the element the menu serves.
    static member elementMenuChoose : ConstructorUiId = ConstructorUiId "ElementMenuChoose"
    /// Spec 0046: the menu's "what is it" lines — the bound entry's name and description.
    static member elementMenuInfo : ConstructorUiId = ConstructorUiId "ElementMenuInfo"
    static member elementMenuRemove : ConstructorUiId = ConstructorUiId "ElementMenuRemove"
    static member elementMenuClose : ConstructorUiId = ConstructorUiId "ElementMenuClose"

    // --- The Library's light-source editor (`LightSourceEditorView`). ---
    static member sourceEditorWindow : ConstructorUiId = ConstructorUiId "SourceEditorWindow"
    static member sourceNameBox : ConstructorUiId = ConstructorUiId "SourceNameBox"
    static member sourceWaveLengthBox : ConstructorUiId = ConstructorUiId "SourceWaveLengthBox"
    /// One wavelength-unit choice box, by the unit's short code.
    static member sourceWaveLengthUnit (waveLengthUnit : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"SourceWaveLengthUnit_{waveLengthUnit.value}"
    static member sourceWaveLengthReadout : ConstructorUiId = ConstructorUiId "SourceWaveLengthReadout"
    /// Spec 0046: the fixed-line / tunable choice and a tunable source's limits.
    static member sourceTuningFixed : ConstructorUiId = ConstructorUiId "SourceTuningFixed"
    static member sourceTuningTunable : ConstructorUiId = ConstructorUiId "SourceTuningTunable"
    static member sourceTunableMinimumBox : ConstructorUiId = ConstructorUiId "SourceTunableMinimumBox"
    static member sourceTunableMaximumBox : ConstructorUiId = ConstructorUiId "SourceTunableMaximumBox"
    static member sourceEditorStatus : ConstructorUiId = ConstructorUiId "SourceEditorStatus"
    static member sourceSaveButton : ConstructorUiId = ConstructorUiId "SourceSaveButton"
    static member sourceCancelButton : ConstructorUiId = ConstructorUiId "SourceCancelButton"
    static member sourceExitConfirm : ConstructorUiId = ConstructorUiId "SourceExitConfirm"
    static member sourceDiscardButton : ConstructorUiId = ConstructorUiId "SourceDiscardButton"
    static member sourceKeepEditingButton : ConstructorUiId = ConstructorUiId "SourceKeepEditingButton"

    // --- The Sample editor (`SampleEditorView`). ---
    static member sampleEditorWindow : ConstructorUiId = ConstructorUiId "SampleEditorWindow"
    static member sampleNameBox : ConstructorUiId = ConstructorUiId "SampleNameBox"
    static member addLayerButton : ConstructorUiId = ConstructorUiId "AddLayerButton"
    static member makeRepeatBlockButton : ConstructorUiId = ConstructorUiId "MakeRepeatBlockButton"
    static member selectByMaterialButton : ConstructorUiId = ConstructorUiId "SelectByMaterialButton"
    static member setLayerHeightButton : ConstructorUiId = ConstructorUiId "SetLayerHeightButton"
    static member setLayerMaterialButton : ConstructorUiId = ConstructorUiId "SetLayerMaterialButton"
    static member setOrientationOfSelectedButton : ConstructorUiId = ConstructorUiId "SetOrientationOfSelectedButton"
    static member removeSelectedLayersButton : ConstructorUiId = ConstructorUiId "RemoveSelectedLayersButton"
    static member repeatCountStepper : ConstructorUiId = ConstructorUiId "RepeatCountStepper"
    static member qwotEntryBox : ConstructorUiId = ConstructorUiId "QwotEntryBox"
    static member sampleEditorSaveButton : ConstructorUiId = ConstructorUiId "SampleEditorSaveButton"
    static member sampleEditorCancelButton : ConstructorUiId = ConstructorUiId "SampleEditorCancelButton"
    /// Spec 0038 (033): the unsaved-edit exit confirm surface — the prompt row and its two
    /// actions (Discard changes / Keep editing).
    static member sampleEditorExitConfirm : ConstructorUiId = ConstructorUiId "SampleEditorExitConfirm"
    static member sampleEditorDiscardButton : ConstructorUiId = ConstructorUiId "SampleEditorDiscardButton"
    static member sampleEditorKeepEditingButton : ConstructorUiId = ConstructorUiId "SampleEditorKeepEditingButton"
    // Supporting fixed ids (not slice-mandated, same naming discipline).
    static member sampleDescriptionBox : ConstructorUiId = ConstructorUiId "SampleDescriptionBox"
    static member sampleLayerHeightBox : ConstructorUiId = ConstructorUiId "SampleLayerHeightBox"
    static member sampleOrientationPhiBox : ConstructorUiId = ConstructorUiId "SampleOrientationPhiBox"
    static member sampleOrientationThetaBox : ConstructorUiId = ConstructorUiId "SampleOrientationThetaBox"
    static member sampleOrientationPsiBox : ConstructorUiId = ConstructorUiId "SampleOrientationPsiBox"
    static member clearSelectionButton : ConstructorUiId = ConstructorUiId "ClearSelectionButton"
    static member moveSelectedUpButton : ConstructorUiId = ConstructorUiId "MoveSelectedUpButton"
    static member moveSelectedDownButton : ConstructorUiId = ConstructorUiId "MoveSelectedDownButton"
    static member qwotDerivedText : ConstructorUiId = ConstructorUiId "QwotDerivedText"
    static member sampleFilmsCount : ConstructorUiId = ConstructorUiId "SampleFilmsCount"
    // Substrate / lower half-space surfaces (spec 0033 gap G12).
    static member sampleSubstrateSummary : ConstructorUiId = ConstructorUiId "SampleSubstrateSummary"
    static member sampleLowerSummary : ConstructorUiId = ConstructorUiId "SampleLowerSummary"
    static member setSubstrateButton : ConstructorUiId = ConstructorUiId "SetSubstrateButton"
    static member clearSubstrateButton : ConstructorUiId = ConstructorUiId "ClearSubstrateButton"
    static member setLowerButton : ConstructorUiId = ConstructorUiId "SetLowerButton"
    static member clearLowerButton : ConstructorUiId = ConstructorUiId "ClearLowerButton"
    static member sampleEditorStatus : ConstructorUiId = ConstructorUiId "SampleEditorStatus"
    // The supported-emission R / T checkboxes (spec 0040 Part D.2 step 010): a Plate exposes
    // both, as does a ThinFilm; a Mirror (spec 0046) shows R fixed on + disabled and renders no T box.
    static member sampleEmitReflectedCheck : ConstructorUiId = ConstructorUiId "SampleEmitReflectedCheck"
    static member sampleEmitTransmittedCheck : ConstructorUiId = ConstructorUiId "SampleEmitTransmittedCheck"
    static member sampleStackTable : ConstructorUiId = ConstructorUiId "SampleStackTable"
    static member repeatCountStepperPlus : ConstructorUiId = ConstructorUiId "RepeatCountStepperPlus"
    static member repeatCountStepperMinus : ConstructorUiId = ConstructorUiId "RepeatCountStepperMinus"
    /// A period group's inline repeat-count stepper and its + / −; the toolbar stepper carries the fixed ids.
    static member repeatCountStepperOfGroup (group : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"RepeatCountStepper_{group.value}"
    static member repeatCountStepperPlusOfGroup (group : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"RepeatCountStepperPlus_{group.value}"
    static member repeatCountStepperMinusOfGroup (group : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"RepeatCountStepperMinus_{group.value}"
    /// A top-level single layer's row, and a period group's nested cell-layer row.
    static member sampleLayerRow (item : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SampleLayerRow_{item.value}"
    static member sampleCellLayerRow (item : ConstructorUiPosition) (cell : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SampleLayerRow_{item.value}_{cell.value}"
    /// A period group's collapsible super-row and its rotating-triangle expander.
    static member sampleGroupRow (item : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SampleGroupRow_{item.value}"
    static member sampleGroupExpander (item : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SampleGroupExpander_{item.value}"
    /// A row's thickness readout cell (what the bulk set-thickness acceptance observes).
    static member sampleLayerThickness (item : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SampleLayerThickness_{item.value}"
    static member sampleCellLayerThickness (item : ConstructorUiPosition) (cell : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SampleLayerThickness_{item.value}_{cell.value}"
    /// A row's per-layer orientation editor (present ONLY for anisotropic materials).
    static member sampleLayerOrientation (item : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SampleLayerOrientation_{item.value}"
    static member sampleCellLayerOrientation (item : ConstructorUiPosition) (cell : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SampleLayerOrientation_{item.value}_{cell.value}"
    /// A SubstrateKind facet option, by its stable code.
    static member sampleSubstrateKind (substrate : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"SampleSubstrateKind_{substrate.value}"
    /// A layer row's Choose material… verb (spec 0038 step 019): a single layer's, and a period group's
    /// nested cell layer's.
    static member chooseMaterialButton (item : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"ChooseMaterialButton_{item.value}"
    static member cellChooseMaterialButton (item : ConstructorUiPosition) (cell : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"ChooseMaterialButton_{item.value}_{cell.value}"

    // --- The Material editor (`MaterialEditorView`). ---
    static member materialEditorWindow : ConstructorUiId = ConstructorUiId "MaterialEditorWindow"
    static member materialNameBox : ConstructorUiId = ConstructorUiId "MaterialNameBox"
    static member anisotropyToggle : ConstructorUiId = ConstructorUiId "AnisotropyToggle"
    static member absorbingToggle : ConstructorUiId = ConstructorUiId "AbsorbingToggle"
    /// Spec 0035 (017): the eps branch is two mutually-exclusive options — Constant / Dispersive.
    static member constantToggle : ConstructorUiId = ConstructorUiId "ConstantToggle"
    static member dispersiveToggle : ConstructorUiId = ConstructorUiId "DispersiveToggle"
    static member activeToggle : ConstructorUiId = ConstructorUiId "ActiveToggle"
    static member magneticToggle : ConstructorUiId = ConstructorUiId "MagneticToggle"
    /// Spec 0035 (011) — the per-rung Constant/Dispersive sub-toggles.
    static member activityDispersiveToggle : ConstructorUiId = ConstructorUiId "ActivityDispersiveToggle"
    static member magneticDispersiveToggle : ConstructorUiId = ConstructorUiId "MagneticDispersiveToggle"
    static member gyrationClassPicker : ConstructorUiId = ConstructorUiId "GyrationClassPicker"
    static member handednessSwitch : ConstructorUiId = ConstructorUiId "HandednessSwitch"
    static member dispersionModelPicker : ConstructorUiId = ConstructorUiId "DispersionModelPicker"
    static member addSegmentButton : ConstructorUiId = ConstructorUiId "AddSegmentButton"
    static member materialEditorSaveButton : ConstructorUiId = ConstructorUiId "MaterialEditorSaveButton"
    static member materialEditorCancelButton : ConstructorUiId = ConstructorUiId "MaterialEditorCancelButton"
    /// Spec 0038 (033): the unsaved-edit exit confirm surface.
    static member materialEditorExitConfirm : ConstructorUiId = ConstructorUiId "MaterialEditorExitConfirm"
    static member materialEditorDiscardButton : ConstructorUiId = ConstructorUiId "MaterialEditorDiscardButton"
    static member materialEditorKeepEditingButton : ConstructorUiId = ConstructorUiId "MaterialEditorKeepEditingButton"
    // Supporting fixed ids (not slice-mandated, same naming discipline).
    static member materialDescriptionBox : ConstructorUiId = ConstructorUiId "MaterialDescriptionBox"
    static member materialCategoryPicker : ConstructorUiId = ConstructorUiId "MaterialCategoryPicker"
    static member gyrationAxisPicker : ConstructorUiId = ConstructorUiId "GyrationAxisPicker"
    static member muDiagonalBox : ConstructorUiId = ConstructorUiId "MuDiagonalBox"
    static member muParallelBox : ConstructorUiId = ConstructorUiId "MuParallelBox"
    static member muGyrationBox : ConstructorUiId = ConstructorUiId "MuGyrationBox"
    static member materialEditorStatus : ConstructorUiId = ConstructorUiId "MaterialEditorStatus"
    static member materialComplexitySummary : ConstructorUiId = ConstructorUiId "MaterialComplexitySummary"
    static member materialPreviewChart : ConstructorUiId = ConstructorUiId "MaterialPreviewChart"
    static member materialGainWarning : ConstructorUiId = ConstructorUiId "MaterialGainWarning"
    static member materialViewOnlyNote : ConstructorUiId = ConstructorUiId "MaterialViewOnlyNote"
    /// Spec 0038 (032): the two-pane split's vertical GridSplitter, the tabbed preview's TabControl,
    /// its three tabs, and the gyration / μ tab chart hosts (the n/k tab reuses `previewChart`).
    static member materialEditorSplitter : ConstructorUiId = ConstructorUiId "MaterialEditorSplitter"
    static member materialPreviewTabs : ConstructorUiId = ConstructorUiId "MaterialPreviewTabs"
    static member materialPreviewNkTab : ConstructorUiId = ConstructorUiId "MaterialPreviewNkTab"
    static member materialPreviewGyrationTab : ConstructorUiId = ConstructorUiId "MaterialPreviewGyrationTab"
    static member materialPreviewMuTab : ConstructorUiId = ConstructorUiId "MaterialPreviewMuTab"
    static member materialGyrationChart : ConstructorUiId = ConstructorUiId "MaterialGyrationChart"
    static member materialMuChart : ConstructorUiId = ConstructorUiId "MaterialMuChart"
    /// A preview curve's show / hide toggle, by its tab's code and the series name.
    static member previewSeriesToggle (tab : ConstructorUiKey) (series : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"PreviewSeriesToggle_{tab.value}_{series.value}"
    /// An anisotropy option, by the choice's stable code.
    static member anisotropyOption (anisotropy : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"AnisotropyOption_{anisotropy.value}"
    /// A principal-index (n) entry and a principal absorption (k) entry, by the 1-based axis slot.
    static member principalIndexBox (axis : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"PrincipalIndexBox_{axis.value}"
    static member absorptionIndexBox (axis : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"AbsorptionIndexBox_{axis.value}"
    /// A segment's model picker: segment 0 carries the fixed `dispersionModelPicker`, a later one its index.
    static member dispersionModelPickerOfSegment (segment : ConstructorUiPosition) : ConstructorUiId =
        match segment.value with
        | 0 -> ConstructorUiId.dispersionModelPicker
        | index -> ConstructorUiId $"DispersionModelPicker_{index}"
    /// A segment's wavelength bounds and its Remove.
    static member segmentLowerBox (segment : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SegmentLowerBox_{segment.value}"
    static member segmentUpperBox (segment : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SegmentUpperBox_{segment.value}"
    static member segmentRemoveButton (segment : ConstructorUiPosition) : ConstructorUiId = ConstructorUiId $"SegmentRemoveButton_{segment.value}"
    /// A segment's dispersion-model option, by the model kind's code.
    static member dispersionModelOption (segment : ConstructorUiPosition) (model : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"DispersionModelOption_{segment.value}_{model.value}"
    /// A gyration-class, handedness ("Right" / "Left"), magnetization-axis ("X" / "Y" / "Z") and Polder-mu
    /// kind ("Scalar" / "Gyromagnetic") option, each by its stable code.
    static member gyrationClassOption (gyrationClass : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"GyrationClassOption_{gyrationClass.value}"
    static member handednessOption (hand : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"HandednessOption_{hand.value}"
    static member gyrationAxisOption (axis : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"GyrationAxisOption_{axis.value}"
    static member muKindOption (kind : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"MuKindOption_{kind.value}"
    /// A segment's dispersion-model coefficient entry (spec 0033 gap G7), by the parameter's stable key.
    static member segmentParamBox (segment : ConstructorUiPosition) (parameter : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"SegmentParamBox_{segment.value}_{parameter.value}"
    /// The per-principal-axis ids (spec 0033 comment 009). The first axis has no `laterAxisOpt` and takes the
    /// single-axis id; the extraordinary / y / z axis appends its code.
    static member dispersionModelPickerOfAxis (segment : ConstructorUiPosition) (laterAxisOpt : ConstructorUiKey option) : ConstructorUiId =
        let single = ConstructorUiId.dispersionModelPickerOfSegment segment
        match laterAxisOpt with
        | None -> single
        | Some axis -> ConstructorUiId $"{single.value}_ax{axis.value}"
    static member dispersionModelOptionOfAxis (segment : ConstructorUiPosition) (laterAxisOpt : ConstructorUiKey option) (model : ConstructorUiKey) : ConstructorUiId =
        let single = ConstructorUiId.dispersionModelOption segment model
        match laterAxisOpt with
        | None -> single
        | Some axis -> ConstructorUiId $"{single.value}_ax{axis.value}"
    static member segmentParamBoxOfAxis (segment : ConstructorUiPosition) (laterAxisOpt : ConstructorUiKey option) (parameter : ConstructorUiKey) : ConstructorUiId =
        let single = ConstructorUiId.segmentParamBox segment parameter
        match laterAxisOpt with
        | None -> single
        | Some axis -> ConstructorUiId $"{single.value}_ax{axis.value}"
    /// A gyration-tensor component entry (spec 0033 gap G9), by the component's code (`g11` / `g33` / …).
    static member gyrationComponentBox (tensorComponent : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"GyrationComponentBox_{tensorComponent.value}"
    /// Spec 0035 (011): a gyration or Polder component's dispersion-formula editor, and one coefficient entry
    /// inside it, by the wrapped model parameter's key.
    static member gyrationFormulaEditor (tensorComponent : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"GyrationFormulaEditor_{tensorComponent.value}"
    static member gyrationFormulaBox (tensorComponent : ConstructorUiKey) (parameter : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"GyrationFormulaBox_{tensorComponent.value}_{parameter.value}"
    static member polderFormulaEditor (tensorComponent : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"PolderFormulaEditor_{tensorComponent.value}"
    static member polderFormulaBox (tensorComponent : ConstructorUiKey) (parameter : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"PolderFormulaBox_{tensorComponent.value}_{parameter.value}"

    // --- The Category editor window (`CategoryEditorView`): its rows and verbs are the Category list's. ---
    static member categoryEditorWindow : ConstructorUiId = ConstructorUiId "CategoryEditorWindow"

    // --- The single-instance Library window (`LibraryWindowView`). ---
    static member libraryWindow : ConstructorUiId = ConstructorUiId "LibraryWindow"
    static member libraryFacetTreeHost : ConstructorUiId = ConstructorUiId "LibraryFacetTreeHost"
    static member libraryViewPanel : ConstructorUiId = ConstructorUiId "LibraryViewPanel"
    static member libraryAddSampleButton : ConstructorUiId = ConstructorUiId "LibraryAddSampleButton"
    static member libraryMakeMultilayerButton : ConstructorUiId = ConstructorUiId "LibraryMakeMultilayerButton"
    /// Spec 0046: open the source editor on a NEW user light source.
    static member libraryAddSourceButton : ConstructorUiId = ConstructorUiId "LibraryAddSourceButton"
    static member libraryEditButton : ConstructorUiId = ConstructorUiId "LibraryEditButton"
    static member libraryRemoveButton : ConstructorUiId = ConstructorUiId "LibraryRemoveButton"
    static member libraryRemoveConfirmButton : ConstructorUiId = ConstructorUiId "LibraryRemoveConfirmButton"
    static member libraryRemoveCancelButton : ConstructorUiId = ConstructorUiId "LibraryRemoveCancelButton"
    static member libraryWindowMessage : ConstructorUiId = ConstructorUiId "LibraryWindowMessage"
    /// The lifecycle surface (spec 0038 step 023).
    static member libraryShowInactiveToggle : ConstructorUiId = ConstructorUiId "LibraryShowInactiveToggle"
    static member libraryMarkInactiveButton : ConstructorUiId = ConstructorUiId "LibraryMarkInactiveButton"
    static member libraryMarkActiveButton : ConstructorUiId = ConstructorUiId "LibraryMarkActiveButton"
    static member librarySupersedeButton : ConstructorUiId = ConstructorUiId "LibrarySupersedeButton"
    static member libraryLifecycleConfirmButton : ConstructorUiId = ConstructorUiId "LibraryLifecycleConfirmButton"
    static member libraryLifecycleCancelButton : ConstructorUiId = ConstructorUiId "LibraryLifecycleCancelButton"
    static member libraryVersionsPanel : ConstructorUiId = ConstructorUiId "LibraryVersionsPanel"
    static member libraryViewOnlyNote : ConstructorUiId = ConstructorUiId "LibraryViewOnlyNote"
    /// The Select-state pair and the fixed-constraint banner (spec 0038 step 016).
    static member librarySelectButton : ConstructorUiId = ConstructorUiId "LibrarySelectButton"
    static member librarySelectCloseButton : ConstructorUiId = ConstructorUiId "LibrarySelectCloseButton"
    static member librarySelectConstraint : ConstructorUiId = ConstructorUiId "LibrarySelectConstraint"

    // Spec 0050 S2.4: library-wide backup and confirmed restore.
    static member libraryBackUpNowButton : ConstructorUiId = ConstructorUiId "LibraryBackUpNowButton"
    static member libraryRestoreButton : ConstructorUiId = ConstructorUiId "LibraryRestoreButton"
    static member libraryRestorePanel : ConstructorUiId = ConstructorUiId "LibraryRestorePanel"
    static member libraryBackupRow (backup : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"LibraryBackupRow_{backup.value}"
    static member libraryRestoreConfirmButton : ConstructorUiId = ConstructorUiId "LibraryRestoreConfirmButton"
    static member libraryRestoreCancelButton : ConstructorUiId = ConstructorUiId "LibraryRestoreCancelButton"
    static member libraryBackupStatusText : ConstructorUiId = ConstructorUiId "LibraryBackupStatusText"

    // Spec 0050 S3.1: registered catalogue folders and import status.
    static member libraryRegisterCatalogueFolderButton : ConstructorUiId = ConstructorUiId "LibraryRegisterCatalogueFolderButton"
    static member libraryCatalogueFoldersPanel : ConstructorUiId = ConstructorUiId "LibraryCatalogueFoldersPanel"
    static member libraryCatalogueFolderRow (catalogue : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"LibraryCatalogueFolderRow_{catalogue.value}"
    static member libraryUnregisterCatalogueButton (catalogue : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"LibraryUnregisterCatalogueButton_{catalogue.value}"
    static member libraryImportStatusText : ConstructorUiId = ConstructorUiId "LibraryImportStatusText"

    // Spec 0050 S3.2: reopen pending catalogue import choices.
    static member libraryReviewImportsButton : ConstructorUiId = ConstructorUiId "LibraryReviewImportsButton"

    // Spec 0050 S3.3: Library exports and selected-entry licence attribution.
    static member libraryExportButton : ConstructorUiId = ConstructorUiId "LibraryExportButton"
    static member libraryExportShownButton : ConstructorUiId = ConstructorUiId "LibraryExportShownButton"
    static member libraryExportMyLibraryButton : ConstructorUiId = ConstructorUiId "LibraryExportMyLibraryButton"
    static member libraryExportStatusText : ConstructorUiId = ConstructorUiId "LibraryExportStatusText"
    static member libraryAttributionPanel : ConstructorUiId = ConstructorUiId "LibraryAttributionPanel"

    // --- The single-instance Materials window (`MaterialsWindowView`). ---
    static member materialsWindow : ConstructorUiId = ConstructorUiId "MaterialsWindow"
    static member materialsFacetTreeHost : ConstructorUiId = ConstructorUiId "MaterialsFacetTreeHost"
    static member materialsViewPanel : ConstructorUiId = ConstructorUiId "MaterialsViewPanel"
    static member materialsViewPanelNkChart : ConstructorUiId = ConstructorUiId "MaterialsViewPanelNkChart"
    static member materialsAddButton : ConstructorUiId = ConstructorUiId "MaterialsAddButton"
    static member materialsEditButton : ConstructorUiId = ConstructorUiId "MaterialsEditButton"
    static member materialsRemoveButton : ConstructorUiId = ConstructorUiId "MaterialsRemoveButton"
    static member materialsCategoriesButton : ConstructorUiId = ConstructorUiId "MaterialsCategoriesButton"
    static member materialsRemoveConfirmButton : ConstructorUiId = ConstructorUiId "MaterialsRemoveConfirmButton"
    static member materialsRemoveCancelButton : ConstructorUiId = ConstructorUiId "MaterialsRemoveCancelButton"
    static member materialsWindowMessage : ConstructorUiId = ConstructorUiId "MaterialsWindowMessage"
    // Spec 0050 S3.2: dropped-entry import completion or refusal.
    static member materialsImportStatusText : ConstructorUiId = ConstructorUiId "MaterialsImportStatusText"
    // Spec 0050 S3.3: material export outcomes and licence attribution.
    static member materialsExportButton : ConstructorUiId = ConstructorUiId "MaterialsExportButton"
    static member materialsExportShownButton : ConstructorUiId = ConstructorUiId "MaterialsExportShownButton"
    static member materialsExportStatusText : ConstructorUiId = ConstructorUiId "MaterialsExportStatusText"
    static member materialsAttributionPanel : ConstructorUiId = ConstructorUiId "MaterialsAttributionPanel"
    /// The lifecycle surface (spec 0038 step 023).
    static member materialsShowInactiveToggle : ConstructorUiId = ConstructorUiId "MaterialsShowInactiveToggle"
    static member materialsMarkInactiveButton : ConstructorUiId = ConstructorUiId "MaterialsMarkInactiveButton"
    static member materialsMarkActiveButton : ConstructorUiId = ConstructorUiId "MaterialsMarkActiveButton"
    static member materialsSupersedeButton : ConstructorUiId = ConstructorUiId "MaterialsSupersedeButton"
    static member materialsLifecycleConfirmButton : ConstructorUiId = ConstructorUiId "MaterialsLifecycleConfirmButton"
    static member materialsLifecycleCancelButton : ConstructorUiId = ConstructorUiId "MaterialsLifecycleCancelButton"
    static member materialsVersionsPanel : ConstructorUiId = ConstructorUiId "MaterialsVersionsPanel"
    static member materialsViewOnlyNote : ConstructorUiId = ConstructorUiId "MaterialsViewOnlyNote"
    /// The Select-state pair and the fixed-constraint banner (spec 0038 step 016).
    static member materialsSelectButton : ConstructorUiId = ConstructorUiId "MaterialsSelectButton"
    static member materialsSelectCloseButton : ConstructorUiId = ConstructorUiId "MaterialsSelectCloseButton"
    static member materialsSelectConstraint : ConstructorUiId = ConstructorUiId "MaterialsSelectConstraint"

    // The import decision dialog (spec 0050 S3.2).
    static member importDialogWindow : ConstructorUiId = ConstructorUiId "ImportDialogWindow"
    static member importDialogHeading : ConstructorUiId = ConstructorUiId "ImportDialogHeading"
    static member importDecisionList : ConstructorUiId = ConstructorUiId "ImportDecisionList"
    static member importDecisionRow (entry : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ImportDecisionRow_{entry.value}"
    static member importActionOption (entry : ConstructorUiKey) (action : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ImportActionOption_{entry.value}_{action.value}"
    static member importDiffToggle (entry : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ImportDiffToggle_{entry.value}"
    static member importDiffPreview (entry : ConstructorUiKey) : ConstructorUiId = ConstructorUiId $"ImportDiffPreview_{entry.value}"
    static member importDialogMessage : ConstructorUiId = ConstructorUiId "ImportDialogMessage"
    static member importApplyButton : ConstructorUiId = ConstructorUiId "ImportApplyButton"
    static member importCancelButton : ConstructorUiId = ConstructorUiId "ImportCancelButton"

    // --- The renderer test scene (`RendererTestView`). ---
    static member rendererTestCanvas : ConstructorUiId = ConstructorUiId "RendererTestCanvas"

    // --- The snap-to-reflected-light test scene (`SnapToReflectedView`). ---
    static member snapReflectedCanvas : ConstructorUiId = ConstructorUiId "SnapReflectedCanvas"
    static member snapReflectedReadout : ConstructorUiId = ConstructorUiId "SnapReflectedReadout"

    // --- The snap-to-beam test scene (`SnapToBeamView`). ---
    static member snapToBeamCanvas : ConstructorUiId = ConstructorUiId "SnapToBeamCanvas"
    static member snapToBeamReadout : ConstructorUiId = ConstructorUiId "SnapToBeamReadout"

    // --- The element-movement test scene (`ElementMovementView`). ---
    static member elementMovementCanvas : ConstructorUiId = ConstructorUiId "ElementMovementCanvas"

    // --- The element-rotation test scene (`ElementRotationView`). ---
    static member elementRotationCanvas : ConstructorUiId = ConstructorUiId "ElementRotationCanvas"
    static member elemRotateR1MinusButton : ConstructorUiId = ConstructorUiId "ElemRotateR1MinusButton"
    static member elemRotateR1PlusButton : ConstructorUiId = ConstructorUiId "ElemRotateR1PlusButton"
    static member elemRotateR2MinusButton : ConstructorUiId = ConstructorUiId "ElemRotateR2MinusButton"
    static member elemRotateR2PlusButton : ConstructorUiId = ConstructorUiId "ElemRotateR2PlusButton"
    static member elemRotateR3MinusButton : ConstructorUiId = ConstructorUiId "ElemRotateR3MinusButton"
    static member elemRotateR3PlusButton : ConstructorUiId = ConstructorUiId "ElemRotateR3PlusButton"
    static member elemUnlockR3Button : ConstructorUiId = ConstructorUiId "ElemUnlockR3Button"
    static member elemResetButton : ConstructorUiId = ConstructorUiId "ElemResetButton"
    static member elemRotationReadout : ConstructorUiId = ConstructorUiId "ElemRotationReadout"

    // --- The table-rotation test scene (`TableRotationView`). ---
    static member tableRotationCanvas : ConstructorUiId = ConstructorUiId "TableRotationCanvas"
    static member rotateR1MinusButton : ConstructorUiId = ConstructorUiId "RotateR1MinusButton"
    static member rotateR1PlusButton : ConstructorUiId = ConstructorUiId "RotateR1PlusButton"
    static member rotateR2MinusButton : ConstructorUiId = ConstructorUiId "RotateR2MinusButton"
    static member rotateR2PlusButton : ConstructorUiId = ConstructorUiId "RotateR2PlusButton"
    static member rotateR3MinusButton : ConstructorUiId = ConstructorUiId "RotateR3MinusButton"
    static member rotateR3PlusButton : ConstructorUiId = ConstructorUiId "RotateR3PlusButton"
    static member resetViewButton : ConstructorUiId = ConstructorUiId "ResetViewButton"
    static member tableRotationReadout : ConstructorUiId = ConstructorUiId "TableRotationReadout"

    // --- The product launcher window (`LauncherWindow`, project App). ---
    static member openMainButton : ConstructorUiId = ConstructorUiId "OpenMainButton"
    static member openInverseButton : ConstructorUiId = ConstructorUiId "OpenInverseButton"
    static member openMaterialsButton : ConstructorUiId = ConstructorUiId "OpenMaterialsButton"
    static member openLibraryButton : ConstructorUiId = ConstructorUiId "OpenLibraryButton"
    /// The footer line showing the app version (spec 0046).
    static member launcherVersionText : ConstructorUiId = ConstructorUiId "LauncherVersionText"
    /// The library-open failure shown on the launcher (spec 0050 S2.4).
    static member launcherLibraryErrorText : ConstructorUiId = ConstructorUiId "LauncherLibraryErrorText"
    /// The picked kit's opening failure on the launcher (spec 0050 S4.3).
    static member launcherKitErrorText : ConstructorUiId = ConstructorUiId "LauncherKitErrorText"
    /// Recreates an incompatible library from catalogues after preserving its backup (spec 0050 001 §C).
    static member launcherRecreateLibraryButton : ConstructorUiId = ConstructorUiId "LauncherRecreateLibraryButton"
    /// Counts every startup import row still awaiting confirmation (spec 0050 S3.2).
    static member launcherPendingImportsText : ConstructorUiId = ConstructorUiId "LauncherPendingImportsText"
    /// Reopens the next pending startup catalogue's import decisions (spec 0050 S3.2).
    static member launcherReviewImportsButton : ConstructorUiId = ConstructorUiId "LauncherReviewImportsButton"

    // --- The diagnostic test launcher window (`TestLauncherWindow`, project TestWindows.App). ---
    static member openTableRotationTestButton : ConstructorUiId = ConstructorUiId "OpenTableRotationTestButton"
    static member openElementRotationTestButton : ConstructorUiId = ConstructorUiId "OpenElementRotationTestButton"
    static member openTableAndElementRotationTestButton : ConstructorUiId = ConstructorUiId "OpenTableAndElementRotationTestButton"
    static member openElementMovementTestButton : ConstructorUiId = ConstructorUiId "OpenElementMovementTestButton"
    static member openRendererTestButton : ConstructorUiId = ConstructorUiId "OpenRendererTestButton"
    static member openSnapToBeamTestButton : ConstructorUiId = ConstructorUiId "OpenSnapToBeamTestButton"
    static member openSnapToReflectedTestButton : ConstructorUiId = ConstructorUiId "OpenSnapToReflectedTestButton"

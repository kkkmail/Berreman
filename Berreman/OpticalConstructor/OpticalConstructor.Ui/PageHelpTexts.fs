/// What every page of the kit window is for, what it needs and what to do next: the one place the pages' help lives
/// (spec 0051 `.manual/007` 1.6, 9.3).
module OpticalConstructor.Ui.PageHelpTexts

open OpticalConstructor.Controls
open OpticalConstructor.Ui.TableAndElementRotationView

type ConstructorFlowStep with

    /// The page's help: what it is for, what must exist first and what to do next (spec 0051 `.manual/007` 1.6).
    member this.help : PageHelp =
        let paragraphs (texts : string list) : PageHelp = { paragraphs = texts |> List.map ControlCaption }
        match this with
        | ItemsFlowStep ->
            paragraphs [
                "Items are the physical pieces of this kit: the light source, polarizers, samples and detectors on your bench. Each instantiates a model from the library and holds its own parameters."
                "Press an item to see it below its row; press it again to close it. Edit changes its label, notes, photo and parameters; Save writes them once, Cancel drops them."
                "New item… opens the library in its own window. Choose a model there, then give the item a label and a mounting, and Create it."
                "Next: put the items on a table in Setups."
            ]
        | SetupsFlowStep ->
            paragraphs [
                "A setup is a named arrangement of this kit's items on the table, such as CPL-(LR#45)-CPL: which item sits in which slot, at which angle."
                "Press a setup to choose it: its slots are listed on its line and its table is drawn under it. The chosen setup is the one new experiments use. Press it again to choose none."
                "Edit makes its table editable: the chips above the table put an item in each slot, and the Add, Rotate, Move, Render and Details tabs change the table. Save writes the setup's next revision; Cancel drops the changes."
                "New setup starts from a light source and a detector. Copy… copies the chosen setup under a new name. Right-click a setup to copy its name or rename it."
                "Next: choose a setup, then make its experiments in Experiments."
            ]
        | ExperimentsFlowStep ->
            paragraphs [
                "An experiment is one scan over a setup: what is varied (an angle, a dial reading or the wavelength), over which range, and which ray the detectors capture."
                "New experiment needs a chosen setup: press Setups, then the setup. The new experiment is made over it; if the setup already has experiments, you are asked first."
                "Press an experiment to see its setup and scan under its row. View ↗ opens its chart; Edit changes its scan, role, session and notes, and re-points it to another setup revision."
                "New experiments from a setup grid… makes many setups and their experiments at once."
                "Next: in Main, view the charts. In Inverse, attach the measured data in Data."
            ]
        | DataFlowStep ->
            paragraphs [
                "The experiment matrix lays out the kit's experiments by what they measure: each row is what prepares the light (the Prep slots) and what it crosses (each object at its angle); each column is what analyses it (the analyzer slots)."
                "Press an experiment in the matrix to open its data under its row: its lab file, the import profile it is read through, and its points. Press it again to close it. An experiment marked ⚠ has no data yet."
                "Attach data file… maps one lab file to the experiment. Edit lets you exclude or include points, each exclusion with a reason; Save writes them. The lab file itself is never changed."
                "Split a file by key… reads one lab file that holds many runs and matches each run to its experiment."
                "Next: say what to fit in Unknowns."
            ]
        | UnknownsFlowStep ->
            paragraphs [
                "A study is one fit problem: the experiments fitted together, and the parameters the fit is to find. Those are the parameters marked Unknown on the items the experiments use, and the parameters of the materials the study opens."
                "Press a study to see its experiments, its parameters and its sessions. Edit adds or removes experiments and opens materials; press a parameter in Edit to change its state, bounds, prior, scope (one value per item, material, session or experiment) and link (one parameter following another)."
                "New study makes a study of the experiments you choose."
                "Next: run the study in Fit. Plan says what the kit can recover, for a kit with a Mueller polarimeter."
            ]
        | PlanFlowStep ->
            paragraphs [
                "The Measurement Planner says, for the chosen study, which unknowns the kit's experiments can recover and how well, and which further experiments would recover more."
                "It needs a Mueller polarimeter in the kit and a crystal material opened in the study. Run plan computes it; Add to kit adds a proposed experiment."
                "Choose the study in Unknowns first."
            ]
        | FitFlowStep ->
            paragraphs [
                "Fit runs the chosen study: it finds the values of its unknowns that make the computed curves match the measured data."
                "New fit run… shows the checks made before the fit and asks how to fit: joint (every unknown at once), stage 1 (calibrate the instruments on the reference runs, such as AIR) or stage 2 (fit the samples, the calibration held fixed or used as priors). Run starts it."
                "Every run is kept as a fit run of the study. Press one to see its summary; Open results shows its values."
                "Choose the study in Unknowns first."
            ]
        | ResultsFlowStep ->
            paragraphs [
                "The chosen fit run's results: the fitted values and their σ, the correlations, and the residuals, in tabs."
                "Every table can be copied as CSV. Choose the fit run in Fit first."
                "Next: publish the calibration from the Kit page."
            ]
        | KitFlowStep ->
            paragraphs [
                "Operations on the kit as a whole: send it as one zip, show its folder, copy experiments from another kit, reload files changed outside the app, and close it."
                "The naming template names new setups and experiments from their slots. Publish writes a fit run's calibrated values into its items as their next revision, and promotes fitted materials to the library."
                "Guide… walks through building a direct problem and an inverse one."
            ]

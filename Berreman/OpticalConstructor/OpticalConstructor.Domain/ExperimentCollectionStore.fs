namespace OpticalConstructor.Domain

/// The named experiment snapshot shown by the solver hand-off (spec 0050 S4.3).
/// Experiments persist individually in the kit; S11.5 retires this hand-off shape.
module ExperimentCollectionStore =

    /// The collection's display name in the solver hand-off.
    type CollectionName =
        | CollectionName of string

        member this.value = let (CollectionName s) = this in s

    /// A named, ordered collection preserving each experiment's setup and attachment.
    type ExperimentCollectionSnapshot =
        {
            name : CollectionName
            experiments : Experiments.Experiment list
        }

namespace OpticalConstructor.Domain

/// The window's snap mode, which a setup does not carry: a setup is the optical path, and how the table places its
/// elements is the window's (spec 0038; spec 0050 S8.2).
module Scene =

    /// Elements snap onto the beam chain in the constructor or use free placement in static
    /// test scenes; the boolean wire form is used only at the IO boundary (spec 0038).
    type SnapMode =
        | SnapToBeam
        | FreePlacement

        /// The host / wire form: true = snap onto the beam (IO boundary only).
        member this.value =
            match this with
            | SnapToBeam -> true
            | FreePlacement -> false

        static member create (snap : bool) : SnapMode =
            match snap with
            | true -> SnapToBeam
            | false -> FreePlacement

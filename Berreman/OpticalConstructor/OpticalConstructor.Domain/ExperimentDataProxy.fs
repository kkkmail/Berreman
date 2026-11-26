namespace OpticalConstructor.Domain

open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ImportProfiles

/// Attaches measured data through one kit, reads an attachment's points through its import profile and row selection,
/// and reads a picked file for the wizard (spec 0050 S4.2, S9.2).
module ExperimentData =

    /// Result-returning operations over one kit's measured-data attachments (spec 0050 S4.2, S9.2).
    [<ReferenceEquality>]
    type ExperimentDataProxy =
        {
            /// A picked file's absolute name and bytes, read without touching the kit: the wizard's preview (spec 0050 S9.2).
            readPickedFile : FileName -> Result<LabFileSource, KitError>
            /// Attaches a picked file in place inside the kit, or copies it into data/ otherwise.
            attachDataFile : FileName -> Result<DataAttachment, KitError>
            /// Reads the attachment's current status through the kit.
            attachmentStatus : DataAttachment -> Result<DataAttachmentStatus, KitError>
            /// The points of an attachment's file read through its import profile, a built-in or the kit's, kept to its row
            /// selection (spec 0050 S9.2).
            readMapped : MappedAttachment -> Result<MappedPoints, ExperimentDataError>
        }

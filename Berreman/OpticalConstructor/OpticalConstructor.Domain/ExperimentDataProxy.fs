namespace OpticalConstructor.Domain

open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData

/// Attaches measured data and reads parsed series through one kit (spec 0050 S4.2).
module ExperimentData =

    /// Result-returning operations over one kit's measured-data attachments (spec 0050 S4.2).
    [<ReferenceEquality>]
    type ExperimentDataProxy =
        {
            /// Attaches a picked file in place inside the kit, or copies it into data/ otherwise.
            attachDataFile : FileName -> Result<DataAttachment, KitError>
            /// Reads the attachment's current status through the kit.
            attachmentStatus : DataAttachment -> Result<DataAttachmentStatus, KitError>
            /// Loads an intensity series by the attachment's kit-relative path.
            tryLoadIntensity : DataAttachment -> Result<IntensitySeries, ExperimentDataError>
            /// Loads an ellipsometric series by the attachment's kit-relative path.
            tryLoadEllipsometric : DataAttachment -> Result<EllipsometricSeries, ExperimentDataError>
        }

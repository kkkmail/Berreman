# Implementation plan

Implement S42-OPEN-STATUS in the two authorized source files. Add the changed
and missing attachment cases and their captions to ExperimentFileStatus in
TableAndElementRotationView.fs. Extract the existing attach-time validation
into one private function and reuse it when CollectionLoad checks each loaded
attachment through ExperimentDataProxy.attachmentStatus. Preserve loaded
attachments, omit unattached experiments from the status map, and replace any
previous session statuses on successful load.

Add exactly the two authorized facts in InverseConstructorTests.fs, using
saved collections, fresh inverse models and injected data proxies. Cover
unchanged validation and its error mapping, changed and missing attachments,
read failures, retained attachments and unattached experiments without adding
test identities beyond the census.

The main risks are duplicating validation, validating changed data, retaining
stale statuses, or losing attachment metadata. Reuse loadAndValidate,
ExperimentDataError.fileStatus and KitRelativePath.toControlCaption. No other
message, model field, storage implementation or durable format changes.

Per IMPLEMENT invariant 6, do not execute builds, tests, analyzers or invariant
gates. The supervisor owns the compiled 0136.gates roster and measured counts.
Write the required implementation log and state of the world at the supplied
paths; leave supervisor state and existing unrelated edits untouched.

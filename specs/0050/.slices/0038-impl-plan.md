# Step 0038 - IMPLEMENT - plan

Add `CatalogueProxy.createBuiltIn () : CatalogueProxy` beside `create` in
Storage's `CatalogueFolderStore.fs`. Construct the elevated folder at origin
from `System.AppContext.BaseDirectory`, `Catalogue`, and `Builtin`, then reuse
the existing folder proxy. Give the member a one-line contract citing spec 0050
§3.5. No new persistence format or dependency is needed.

Add the specified fact to `CatalogueFolderTests.fs` first. Require a successful
read, the frozen catalogue id and Built-in name, and equality with a read using
an independently spelled-out output path. Replace the path in
`BuiltInCatalogueParityTests.fs` with the new member and remove unused imports;
preserve all five parity facts and their assertions.

The Content item already copies the shipped catalogue into referencing outputs.
The main risks are accidentally relying on the working directory or allowing
the location fact to share the implementation's path expression. Keep the
production base-directory type fully qualified and the test path independent.

The IMPLEMENT worker's invariant 6 assigns all gates to the supervisor after
exit. Record the pending build, elevation, full constructor suite and six s06
invariants without running or claiming their results. Write the implementation
log and state-of-the-world only at the task's prescribed paths.

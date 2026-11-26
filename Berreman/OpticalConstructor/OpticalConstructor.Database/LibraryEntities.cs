namespace OpticalConstructor.Database;

/// <summary>
/// One immutable library entry version with canonical payload and hash (spec 0050, 001 §C).
/// </summary>
public sealed class LibraryEntryVersionRow
{
    public Guid EntryId { get; set; }
    public int Version { get; set; }
    public int Kind { get; set; }
    public string Name { get; set; } = string.Empty;
    public int Lifecycle { get; set; }
    public int Protection { get; set; }
    public int PinState { get; set; }
    public int ProvenanceKind { get; set; }
    public Guid? CatalogueId { get; set; }
    public string PayloadText { get; set; } = string.Empty;
    public string ContentHash { get; set; } = string.Empty;
}

/// <summary>
/// The locally selected latest held version of a library entry (spec 0050, 001 §C).
/// </summary>
public sealed class LibraryLatestVersionRow
{
    public Guid EntryId { get; set; }
    public int Version { get; set; }
}

/// <summary>
/// One queryable facet value of a library entry version (spec 0050, 001 §C).
/// </summary>
public sealed class LibraryEntryFacetRow
{
    public Guid EntryId { get; set; }
    public int Version { get; set; }
    public string FacetKey { get; set; } = string.Empty;
    public string FacetValue { get; set; } = string.Empty;
}

/// <summary>
/// A material version referenced by a sample version (spec 0050, 001 §C).
/// </summary>
public sealed class LibrarySampleMaterialRow
{
    public Guid SampleId { get; set; }
    public int SampleVersion { get; set; }
    public Guid MaterialId { get; set; }
    public int MaterialVersion { get; set; }
}

/// <summary>
/// A catalogue import's origin, manifest hash, time and entry count (spec 0050, 001 §C).
/// </summary>
public sealed class LibraryCatalogueImportRow
{
    public Guid CatalogueId { get; set; }
    public string Folder { get; set; } = string.Empty;
    public string ManifestHash { get; set; } = string.Empty;
    public DateTime ImportedAtUtc { get; set; }
    public int EntryCount { get; set; }
}

/// <summary>
/// An online source record and the entry version it minted, with attribution (spec 0050, 001 §C–D).
/// </summary>
public sealed class LibraryPulledSourceRow
{
    public Guid EntryId { get; set; }
    public int Version { get; set; }
    public string Source { get; set; } = string.Empty;
    public string RecordPath { get; set; } = string.Empty;
    public string SourceVersion { get; set; } = string.Empty;
    public string Licence { get; set; } = string.Empty;
    public string Citation { get; set; } = string.Empty;
}

/// <summary>
/// The library schema version used to reject incompatible databases (spec 0050, 001 §C).
/// </summary>
public sealed class LibrarySchemaStampRow
{
    public int Id { get; set; }
    public int SchemaVersion { get; set; }
}

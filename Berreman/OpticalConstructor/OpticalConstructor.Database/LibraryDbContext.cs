using Microsoft.EntityFrameworkCore;

namespace OpticalConstructor.Database;

/// <summary>
/// The slow-data library model, created by EnsureCreated and schema-stamped, never migrated
/// (spec 0050, 001 §C). Kit data, measurements and preferences belong outside this database.
/// </summary>
public class LibraryDbContext : DbContext
{
    public LibraryDbContext(DbContextOptions<LibraryDbContext> options) : base(options)
    {
    }

    public DbSet<LibraryEntryVersionRow> EntryVersions => Set<LibraryEntryVersionRow>();
    public DbSet<LibraryLatestVersionRow> LatestVersions => Set<LibraryLatestVersionRow>();
    public DbSet<LibraryEntryFacetRow> EntryFacets => Set<LibraryEntryFacetRow>();
    public DbSet<LibrarySampleMaterialRow> SampleMaterials => Set<LibrarySampleMaterialRow>();
    public DbSet<LibraryCatalogueImportRow> CatalogueImports => Set<LibraryCatalogueImportRow>();
    public DbSet<LibraryPulledSourceRow> PulledSources => Set<LibraryPulledSourceRow>();
    public DbSet<LibrarySchemaStampRow> SchemaStamps => Set<LibrarySchemaStampRow>();

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<LibraryEntryVersionRow>(entity =>
        {
            entity.ToTable("EntryVersion");
            entity.HasKey(row => new { row.EntryId, row.Version });
            entity.Property(row => row.EntryId).ValueGeneratedNever();
            entity.Property(row => row.Version).ValueGeneratedNever();
            entity.Property(row => row.Name).IsRequired();
            entity.Property(row => row.PayloadText).IsRequired();
            entity.Property(row => row.ContentHash).IsRequired();
            entity.HasIndex(row => row.Kind);
            entity.HasIndex(row => row.CatalogueId);
        });

        modelBuilder.Entity<LibraryLatestVersionRow>(entity =>
        {
            entity.ToTable("LatestVersion");
            entity.HasKey(row => row.EntryId);
            entity.Property(row => row.EntryId).ValueGeneratedNever();
            entity.HasOne<LibraryEntryVersionRow>()
                .WithMany()
                .HasForeignKey(row => new { row.EntryId, row.Version })
                .OnDelete(DeleteBehavior.Restrict);
        });

        modelBuilder.Entity<LibraryEntryFacetRow>(entity =>
        {
            entity.ToTable("EntryFacet");
            entity.HasKey(row => new { row.EntryId, row.Version, row.FacetKey, row.FacetValue });
            entity.Property(row => row.EntryId).ValueGeneratedNever();
            entity.Property(row => row.Version).ValueGeneratedNever();
            entity.Property(row => row.FacetKey).ValueGeneratedNever().IsRequired();
            entity.Property(row => row.FacetValue).ValueGeneratedNever().IsRequired();
            entity.HasOne<LibraryEntryVersionRow>()
                .WithMany()
                .HasForeignKey(row => new { row.EntryId, row.Version })
                .OnDelete(DeleteBehavior.Cascade);
            entity.HasIndex(row => new { row.FacetKey, row.FacetValue });
        });

        modelBuilder.Entity<LibrarySampleMaterialRow>(entity =>
        {
            entity.ToTable("SampleMaterial");
            entity.HasKey(row => new { row.SampleId, row.SampleVersion, row.MaterialId, row.MaterialVersion });
            entity.Property(row => row.SampleId).ValueGeneratedNever();
            entity.Property(row => row.SampleVersion).ValueGeneratedNever();
            entity.Property(row => row.MaterialId).ValueGeneratedNever();
            entity.Property(row => row.MaterialVersion).ValueGeneratedNever();
            entity.HasOne<LibraryEntryVersionRow>()
                .WithMany()
                .HasForeignKey(row => new { row.SampleId, row.SampleVersion })
                .OnDelete(DeleteBehavior.Cascade);
            // Spec 0050 S2.1: a sample may be stored before the material versions it references.
            entity.HasIndex(row => new { row.MaterialId, row.MaterialVersion });
        });

        modelBuilder.Entity<LibraryCatalogueImportRow>(entity =>
        {
            entity.ToTable("CatalogueImport");
            entity.HasKey(row => row.CatalogueId);
            entity.Property(row => row.CatalogueId).ValueGeneratedNever();
            entity.Property(row => row.Folder).IsRequired();
            entity.Property(row => row.ManifestHash).IsRequired();
        });

        modelBuilder.Entity<LibraryPulledSourceRow>(entity =>
        {
            entity.ToTable("PulledSource");
            entity.HasKey(row => new { row.EntryId, row.Version });
            entity.Property(row => row.EntryId).ValueGeneratedNever();
            entity.Property(row => row.Version).ValueGeneratedNever();
            entity.Property(row => row.Source).IsRequired();
            entity.Property(row => row.RecordPath).IsRequired();
            entity.Property(row => row.SourceVersion).IsRequired();
            entity.Property(row => row.Licence).IsRequired();
            entity.Property(row => row.Citation).IsRequired();
            entity.HasOne<LibraryEntryVersionRow>()
                .WithMany()
                .HasForeignKey(row => new { row.EntryId, row.Version })
                .OnDelete(DeleteBehavior.Cascade);
            entity.HasIndex(row => new { row.Source, row.RecordPath });
        });

        modelBuilder.Entity<LibrarySchemaStampRow>(entity =>
        {
            entity.ToTable("SchemaStamp");
            entity.HasKey(row => row.Id);
            entity.Property(row => row.Id).ValueGeneratedNever();
        });
    }
}

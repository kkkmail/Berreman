using Microsoft.EntityFrameworkCore;

namespace OpticalConstructor.Database;

/// <summary>
/// Spec 0038 Part M (step 040): the EFC-wiring proof context. Modeled on the STL
/// migrations shape (Softellect/Apps/DistrProc/Migrations/Common — a per-service
/// DbContext over the shared base) but trimmed to a single <see cref="Test"/>
/// entity. Provider + connection string are chosen by the CALLER (the
/// OpticalConstructor.Tests F# test) from appsettings.json and injected via
/// <see cref="DbContextOptions{TestDbContext}"/>, so this type is provider-agnostic
/// and holds no hardcoded connection string.
/// </summary>
public class TestDbContext : DbContext
{
    public TestDbContext(DbContextOptions<TestDbContext> options)
        : base(options)
    {
    }

    public DbSet<Test> Tests { get; set; } = null!;

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

        // The migration and the assertions both name the table "Test"; the default
        // convention would pluralize to the DbSet name, so pin it explicitly.
        modelBuilder.Entity<Test>().ToTable("Test");
    }
}

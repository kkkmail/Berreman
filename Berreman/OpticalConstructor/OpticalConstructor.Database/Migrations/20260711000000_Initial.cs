using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace OpticalConstructor.Database.Migrations
{
    /// <summary>
    /// Spec 0038 Part M (step 040): the initial migration. SQLite-shaped
    /// (<c>INTEGER</c>/<c>TEXT</c>, <c>Sqlite:Autoincrement</c>) so it applies
    /// cleanly against SQLite — the ONLY provider the step-040 test exercises. It
    /// creates the single <c>Test</c> table proving EFC create + migrate wiring.
    /// </summary>
    /// <inheritdoc />
    public partial class Initial : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "Test",
                columns: table => new
                {
                    Id = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    Name = table.Column<string>(type: "TEXT", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Test", x => x.Id);
                });
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "Test");
        }
    }
}

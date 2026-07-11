namespace OpticalConstructor.Database;

/// <summary>
/// Spec 0038 Part M (step 040): the ONE entity of the EFC-wiring proof — an int
/// <see cref="Id"/> key plus a <see cref="Name"/> column. It exists only so the
/// initial migration has a table to create; no product code models against it.
/// </summary>
public class Test
{
    public int Id { get; set; }

    public string Name { get; set; } = null!;
}

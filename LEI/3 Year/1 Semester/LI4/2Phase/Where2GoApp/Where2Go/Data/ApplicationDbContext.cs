using Microsoft.AspNetCore.Identity.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore;
using Where2Go.Models;

namespace Where2Go.Data
{
    public class ApplicationDbContext : IdentityDbContext<ApplicationUser>
    {
        public ApplicationDbContext(DbContextOptions<ApplicationDbContext> options) : base(options)
        {
        }

        public DbSet<City> Cities { get; set; }
        public DbSet<PointsOfInterest> PointsOfInterests { get; set; }


        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            modelBuilder.Entity<History>()
                .HasKey(t => new { t.PointsOfInterestId, t.UserId });

            modelBuilder.Entity<History>()
                .HasOne(pt => pt.PointsOfInterest)
                .WithMany(p => p.Histories)
                .HasForeignKey(pt => pt.PointsOfInterestId);

            modelBuilder.Entity<History>()
                .HasOne(pt => pt.User)
                .WithMany(t => t.Histories)
                .HasForeignKey(pt => pt.UserId);

            modelBuilder.Entity<Classification>()
                .HasKey(t => new { t.PointsOfInterestId, t.UserId });

            modelBuilder.Entity<Classification>()
                .HasOne(pt => pt.PointsOfInterest)
                .WithMany(p => p.Classifications)
                .HasForeignKey(pt => pt.PointsOfInterestId);

            modelBuilder.Entity<Classification>()
                .HasOne(pt => pt.User)
                .WithMany(t => t.Classifications)
                .HasForeignKey(pt => pt.UserId);
        }


        public DbSet<Where2Go.Models.History> History { get; set; }


        public DbSet<Where2Go.Models.Classification> Classification { get; set; }
    }
}

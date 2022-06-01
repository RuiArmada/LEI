using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;

namespace Where2Go.Models
{
    public class PointsOfInterest
    {
        [Key]
        public int Id { get; set; }
        [Required]
        [StringLength(100)]
        [DisplayName("Name")]
        public string Name { get; set; }
        [Required]
        [DisplayName("Latitude")]
        public float Latitude { get; set; }
        [Required]
        [DisplayName("Longitude")]
        public float Longitude { get; set; }
        [Required]
        [StringLength(500)]
        [DisplayName("Description")]
        public string Description { get; set; }
        [Required]
        [StringLength(100)]
        [DisplayName("Type")]
        public string Type { get; set; }
        [Required]
        public int CityId { get; set; }

        public virtual City City { get; set; }
        public virtual ICollection<History> Histories { get; set; }
        public virtual ICollection<Classification> Classifications { get; set; }
    }
}

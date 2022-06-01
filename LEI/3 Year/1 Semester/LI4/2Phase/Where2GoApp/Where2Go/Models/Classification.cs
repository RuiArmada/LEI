using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;

namespace Where2Go.Models
{
    public class Classification
    {
        [Key]
        public int Id { get; set; }
        [Required]
        [Range(0, 5)]
        [DisplayName("Classification")]
        public int Rate { get; set; }
        [Required]
        [StringLength(500)]
        [DisplayName("Description")]
        public string Description { get; set; }

        public string UserId { get; set; }

        public virtual ApplicationUser User { get; set; }
        [DisplayName("Point of interest")]
        public int PointsOfInterestId { get; set; }
        public virtual PointsOfInterest PointsOfInterest { get; set; }
    }
}

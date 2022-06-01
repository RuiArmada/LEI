using System;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;

namespace Where2Go.Models
{
    public class History
    {
        [Key]
        public int Id { get; set; }

        [Required]
        [DataType(DataType.Date)]
        [DisplayFormat(DataFormatString = "{0:yyyy-MM-dd}", ApplyFormatInEditMode = true)]
        [DisplayName("Date")]
        public DateTime VisitDate { get; set; }

        public string UserId { get; set; }

        public virtual ApplicationUser User { get; set; }
        [DisplayName("Point of interest")]
        public int PointsOfInterestId { get; set; }
        public virtual PointsOfInterest PointsOfInterest { get; set; }
    }
}

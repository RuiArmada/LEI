using System.ComponentModel;
using System.ComponentModel.DataAnnotations;

namespace Where2Go.Models
{
    public class City
    {
        [Key]
        public int Id { get; set; }
        [Required]
        [StringLength(100)]
        [DisplayName("Name")]
        public string Name { get; set; }
    }
}

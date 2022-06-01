using Microsoft.AspNetCore.Identity;
using System.Collections.Generic;

namespace Where2Go.Models
{
    public class ApplicationUser : IdentityUser
    {
        public virtual ICollection<History> Histories { get; set; }
        public virtual ICollection<Classification> Classifications { get; set; }
    }
}
